{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NixFromNpm.ConvertToNix where

import qualified Prelude as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import NixFromNpm.Common
import Nix.Types
import Nix.Parser
import Nix.Pretty (prettyNix)
import NixFromNpm.Options
import NixFromNpm.NpmTypes
import NixFromNpm.SemVer
import NixFromNpm.Parsers.SemVer
import NixFromNpm.PackageMap (PackageMap, pmLookup, pmDelete, pmMap)
import NixFromNpm.NpmLookup (FullyDefinedPackage(..),
                             NpmFetcher(..),
                             NpmFetcherSettings(..),
                             NpmFetcherState(..),
                             toFullyDefined,
                             startState,
                             runNpmFetchWith,
                             resolveNpmVersionRange,
                             resolveVersionInfo,
                             extractPkgJson,
                             PreExistingPackage(..))

data ConversionError
  = NoPackagesGenerated
  | FolderDoesNotExist FilePath
  | FileDoesNotExist FilePath
  | InvalidStartingSource String
  deriving (Show, Eq, Typeable)

_startingSrc :: String
_startingSrc = "\
  \{pkgs ? import <nixpkgs> {},               \
  \ nodejs ? pkgs.nodejs-4_1, \
  \ buildNodePackage ? import <buildNodePackage> { \
  \   inherit pkgs nodejs; }}: \
  \let                                                               \
  \  inherit (pkgs.lib) attrValues foldl;                            \
  \  joinSets = foldl (a: b: a // b) {};                             \
  \  joinedExtensions = joinSets (attrValues extensions);            \
  \    brokenPackage = {name, reason}:                               \
  \      let \
  \      deriv = pkgs.stdenv.mkDerivation { \
  \          name = \"broken-${name}\"; \
  \          buildCommand = '' \
  \            echo \"Package ${name} is broken: ${reason}\"; \
  \            exit 1 \
  \          ''; \
  \          passthru.withoutTests = deriv; \
  \          passthru.pkgName = name; \
  \          passthru.version = \"0.0.0\"; \
  \        }; \
  \      in \
  \      deriv; \
  \  allPkgs = pkgs // nodePkgs // joinedExtensions //            \
  \   {inherit buildNodePackage brokenPackage;};  \
  \  callPackage = pkgs.lib.callPackageWith allPkgs;              \
  \  nodePkgs = joinedExtensions // byVersion // defaults;           \
  \in                                                                \
  \nodePkgs // {inherit callPackage;}"

_startingExpr :: MonadIO io => io NExpr
_startingExpr = do
  case parseNixString $! _startingSrc of
    Success e -> return e
    Failure e -> error $ unlines ["FATAL: Starting source failed to parse:",
                                  show e]

callPackage :: NExpr -> NExpr
callPackage = callPackageWith []

callPackageWith :: [Binding NExpr] -> NExpr -> NExpr
callPackageWith args e = mkApp (mkApp (mkSym "callPackage") e)
                               (mkNonRecSet args)

-- | Turns a string into one that can be used as an identifier.
fixName :: Name -> Name
fixName = T.replace "." "-"

-- | Converts a package name and semver into an identifier.
toDepName :: Name -> SemVer -> Name
toDepName name (a, b, c, tags) = do
  let suffix = pack $ intercalate "-" $ (map show [a, b, c]) <> map unpack tags
  fixName name <> "_" <> suffix

-- | Converts a ResolvedDependency to a nix expression.
toNixExpr :: Name -> ResolvedDependency -> NExpr
toNixExpr name (Resolved semver) = mkSym $ toDepName name semver
toNixExpr name (Broken reason) = mkApp (mkSym "brokenPackage") $ mkNonRecSet
  [ "name" `bindTo` str name, "reason" `bindTo` str (pack $ show reason)]

-- | Gets the .nix filename of a semver. E.g. (0, 1, 2) -> 0.1.2.nix
toDotNix :: SemVer -> Text
toDotNix v = renderSV v <> ".nix"

-- | Creates a doublequoted string from some text.
str :: Text -> NExpr
str = mkStr DoubleQuoted

-- | Converts distinfo into a nix fetchurl call.
distInfoToNix :: Maybe DistInfo -> NExpr
distInfoToNix Nothing = mkPath False "./."
distInfoToNix (Just DistInfo{..}) = mkApp (mkSym "fetchurl") $ do
  let (algo, hash) = case diShasum of
        SHA1 hash' -> ("sha1", hash')
        SHA256 hash' -> ("sha256", hash')
  mkNonRecSet ["url" `bindTo` str diUrl, algo `bindTo`  str hash]

-- | Tests if there is information in the package meta.
metaNotEmpty :: PackageMeta -> Bool
metaNotEmpty PackageMeta{..} = isJust pmDescription

-- | Converts package meta to a nix expression.
metaToNix :: PackageMeta -> NExpr
metaToNix PackageMeta{..} = case pmDescription of
  Nothing -> mkNonRecSet []
  Just d -> mkNonRecSet ["description" `bindTo` str d]

-- | Converts a resolved package object into a nix expression. The expresion
-- will be a function where the arguments are its dependencies, and its result
-- is a call to `buildNodePackage`.
resolvedPkgToNix :: ResolvedPkg -> NExpr
resolvedPkgToNix ResolvedPkg{..} = do
  let -- Get a string representation of each dependency in name-version format.
      deps = map (uncurry toNixExpr) $ H.toList rpDependencies
      devDeps = map (uncurry toNixExpr) . H.toList <$> rpDevDependencies
      allDeps = H.toList $ rpDependencies <> maybe mempty id rpDevDependencies
      -- List of non-broken packages
      nonBrokenDeps = catMaybes $ flip map allDeps $ \case
        (_, Broken _) -> Nothing
        (name, Resolved ver) -> Just (name, ver)
      -- Get the parameters of the package function (deps + utility functions).
      _funcParams = map (uncurry toDepName) nonBrokenDeps
                    <> ["buildNodePackage", "fetchurl", "brokenPackage"]
      -- None of these have defaults, so put them into pairs with Nothing.
      funcParams = mkFormalSet $ map (\x -> (x, Nothing)) _funcParams
  let args = mkNonRecSet $ catMaybes [
        Just $ "name" `bindTo` str rpName,
        Just $ "version" `bindTo` (str $ renderSV rpVersion),
        Just $ "src" `bindTo` distInfoToNix rpDistInfo,
        Just $ "deps" `bindTo` mkList deps,
        bindTo "devDependencies" . mkList <$> devDeps,
        maybeIf (metaNotEmpty rpMeta) $ "meta" `bindTo` metaToNix rpMeta
        ]
  mkFunction funcParams $ mkApp (mkSym "buildNodePackage") args

-- | Creates the `default.nix` file that is the top-level expression we are
-- generating.
mkDefaultNix :: NExpr -- The initial nix expression for default.nix.
             -> Record [SemVer] -- ^ Map of names to versions of packages that
                                --   exist in this library.
             -> Record FilePath -- ^ Map of extensions being included.
             -> IO NExpr -- ^ A generated nix expression.
mkDefaultNix baseExpr versionMap extensionMap = do
  let mkPath' = mkPath False . unpack
      toPath name ver = mkPath' $ concat ["./", name, "/", toDotNix ver]
      -- Make a set of all of the extensions
      extensionsSet = mkNonRecSet $
        -- Map over the expression map, creating a binding for each pair.
        flip map (H.toList extensionMap) $ \(name, path) ->
          name `bindTo` (mkApp
                          (mkApp (mkSym "import")
                                 (mkPath False (pathToString path)))
                          (mkNonRecSet [Inherit Nothing [mkSelector "nixpkgs"]]))
      mkBinding name ver = toDepName name ver
                            `bindTo` callPackage (toPath name ver)
      mkBindings name vers = map (mkBinding name) vers
      mkDefVer name vers = case vers of
        [] -> errorC ["FATAL: no versions generated for package ", name]
        _  -> fixName name `bindTo` mkSym (toDepName name $ maximum vers)
      -- This bit of map gymnastics will create a list of pairs of names
      -- with all of the versions of that name that we have.
      versOnly = sortOn fst $ H.toList versionMap
      byVersion = mkNonRecSet $ concatMap (uncurry mkBindings) versOnly
      defaults = mkWith (mkSym "byVersion") $
        mkNonRecSet $ map (uncurry mkDefVer) versOnly
      newBindings = ["extensions" `bindTo` extensionsSet,
                     "byVersion" `bindTo` byVersion,
                     "defaults" `bindTo` defaults]
  return $ modifyFunctionBody (appendBindings newBindings) baseExpr

-- | The npm lookup utilities will produce a bunch of fully defined packages.
-- However, the only packages that we want to write are the new ones; that
-- is, the ones that we've discovered and the ones that already exist. This
-- will perform the appropriate filter.
takeNewPackages :: PackageMap FullyDefinedPackage
                -> (PackageMap ResolvedPkg, PackageMap NExpr)
takeNewPackages startingRec = do
  let isNew (NewPackage rpkg) = Just rpkg
      isNew _ = Nothing
      exists (FromExistingInOutput expr) = Just expr
      exists _ = Nothing
      newPkgs = H.map (modifyMap isNew) startingRec
      existingPkgs = H.map (modifyMap exists) startingRec
      removeEmpties = H.filter (not . H.null)
  (removeEmpties newPkgs, removeEmpties existingPkgs)

-- | Given the path to a package, finds all of the .nix files which parse
--   correctly.
parseVersion :: MonadIO io => Name -> FilePath -> io (Maybe (SemVer, NExpr))
parseVersion pkgName path = do
  let pth = pathToString path
      versionTxt :: Text
      versionTxt = pack $ dropSuffix ".nix" $ unpack $ takeBaseName path
  case parseSemVer versionTxt of
    Left _ -> return Nothing -- not a version file
    Right version -> parseNixString . pack <$> readFile pth >>= \case
      Failure err -> do
        putStrsLn ["Warning: expression for ", pkgName, " version ",
                   versionTxt, " failed to parse:\n", pack $ show err]
        return Nothing -- invalid nix, should overwrite
      Success expr -> return $ Just (version, expr)

-- | Given the path to a file possibly containing nix expressions, finds all
--   expressions findable at that path and returns a map of them.
findExisting :: (MonadBaseControl IO io, MonadIO io)
             => Maybe Name -- ^ Is `Just` if this is an extension.
             -> FilePath       -- ^ The path to search.
             -> io (PackageMap PreExistingPackage) -- ^ Mapping of package
                                                   --   names to maps of
                                                   --   versions to nix
                                                   --   expressions.
findExisting maybeName path = do
  doesDirectoryExist path >>= \case
    False -> case maybeName of
               Just name -> errorC ["Extension ", pshow name, " at path ",
                                    pathToText path, " does not exist."]
               Nothing -> return mempty
    True -> withDir path $ do
      let wrapper :: NExpr -> PreExistingPackage
          wrapper = case maybeName of Nothing -> FromOutput
                                      Just name -> FromExtension name
      putStrsLn ["Searching for existing expressions in ", pathToText path,
                 "..."]
      contents <- getDirectoryContents path
      verMaps <- map catMaybes $ forM contents $ \dir -> do
        exprs <- doesDirectoryExist dir >>= \case
          True -> withDir dir $ do
            contents <- getDirectoryContents "."
            let files = filter (hasExt "nix") contents
            catMaybes <$> mapM (parseVersion $ pathToText dir) files
          False -> do
            return mempty -- not a directory
        case exprs of
          [] -> return Nothing
          vs -> return $ Just (pathToText dir, H.map wrapper $ H.fromList exprs)
      let total = sum $ map (H.size . snd) verMaps
      putStrsLn ["Found ", render total, " existing expressions:"]
      forM verMaps $ \(name, vers) -> do
        putStrs ["  ", name, ": "]
        putStrLn $ mapJoinBy ", " renderSV $ H.keys vers
      return $ H.fromList verMaps

-- | Given the output directory and any number of extensions to load,
-- finds any existing packages.
preloadPackages :: NpmFetcher () -- ^ Add the existing packages to the state.
preloadPackages = do
  existing <- asks nfsNoCache >>= \case
    True -> return mempty
    False -> findExisting Nothing =<< asks nfsOutputPath
  toExtend <- asks nfsExtendPaths
  libraries <- fmap concat $ forM (H.toList toExtend) $ \(name, path) -> do
    findExisting (Just name) path
  let all = existing <> libraries
  modify $ \s -> s {
    resolved = pmMap toFullyDefined all <> resolved s
    }

-- | Actually writes the packages to disk. Takes in the new packages to write,
--   and the names/paths to the libraries being extended.
dumpPkgs :: MonadIO m
         => NExpr                  -- ^ Base default.nix expression.
         -> FilePath               -- ^ Path to output directory.
         -> PackageMap ResolvedPkg -- ^ New packages being written.
         -> PackageMap NExpr       -- ^ Existing packages to be included
                                   --   in the generated default.nix.
         -> Record FilePath        -- ^ Libraries being extended.
         -> m ()
dumpPkgs baseExpr path newPackages existingPackages extensions = liftIO $ do
  -- If there aren't any new packages, we can stop here.
  if H.null newPackages
  then putStrLn "No new packages created." >> return ()
  else do
    putStrsLn ["Creating new packages at ", pathToText path]
    createDirectoryIfMissing path
    withDir path $ do
      -- Write the .nix file for each version of this package.
      forM_ (H.toList newPackages) $ \(pkgName, pkgVers) -> do
        let subdir = path </> fromText pkgName
        createDirectoryIfMissing subdir
        withDir subdir $ forM_ (H.toList pkgVers) $ \(ver, rpkg) -> do
          let expr = resolvedPkgToNix rpkg
              fullPath = subdir </> fromText (toDotNix ver)
          putStrsLn ["Writing package file at ", pathToText fullPath]
          writeFile (unpack $ toDotNix ver) $ show $ prettyNix expr
      -- Write the default.nix file for the library.
      -- We need to build up a record mapping package names to the list of
      -- versions being defined in this library.
      let versionMap = map H.keys newPackages <> map H.keys existingPackages
      defaultNix <- mkDefaultNix baseExpr versionMap extensions
      writeFile "default.nix" $ show $ prettyNix defaultNix

-- | Given a set of fetched packages, generates the expressions needed to
-- build that package and writes them to disk.
dumpPackages :: NpmFetcher ()       -- ^ Writes files to a folder.
dumpPackages = do
  baseExpr <- asks nfsBaseExpr
  path <- asks nfsOutputPath
  packages <- gets resolved
  let (new, existing) = takeNewPackages packages
  extensions <- asks nfsExtendPaths
  dumpPkgs baseExpr path new existing extensions

dumpFromPkgJson :: FilePath -- ^ Path to folder containing package.json.
                -> NpmFetcher ()
dumpFromPkgJson path = do
  doesDirectoryExist path >>= \case
    False -> errorC ["No such directory ", pathToText path]
    True -> withDir path $ do
      doesFileExist "package.json" >>= \case
        False -> errorC ["No package.json found in ", pathToText path]
        True -> do
          verinfo <- extractPkgJson "package.json"
          let (name, version) = (viName verinfo, viVersion verinfo)
          putStrsLn ["Generating expression for package ", name,
                     ", version ", renderSV version]
          resolveVersionInfo verinfo
          nixexpr <- pmLookup name version <$> gets resolved >>= \case
            Nothing -> errorC ["FATAL: could not build nix file"]
            Just (FromExistingInOutput e) -> return e
            Just (FromExistingInExtension _ e) -> return e
            Just (NewPackage rpkg) -> return $ resolvedPkgToNix rpkg
          liftIO $ writeFile "default.nix" $ show $ prettyNix nixexpr
          modify $ \s -> s {
            resolved = pmDelete name version $ resolved s
            }

optionsToSettings :: MonadIO m => NixFromNpmOptions -> m NpmFetcherSettings
optionsToSettings NixFromNpmOptions{..} = do
  baseExpr <- _startingExpr
  return (NpmFetcherSettings {
    nfsGithubAuthToken = nfnoGithubToken,
    nfsRegistries = nfnoRegistries,
    nfsRequestTimeout = fromIntegral nfnoTimeout,
    nfsOutputPath = nfnoOutputPath,
    nfsExtendPaths = nfnoExtendPaths,
    nfsBaseExpr = baseExpr,
    nfsMaxDevDepth = nfnoDevDepth,
    nfsNoCache = nfnoNoCache
  })

dumpPkgFromOptions :: NixFromNpmOptions -> IO ()
dumpPkgFromOptions (opts@NixFromNpmOptions{..}) = do
  settings <- optionsToSettings opts
  runNpmFetchWith settings startState $ do
    forM nfnoPkgNames $ \(name, range) -> do
      resolveNpmVersionRange name range
    case nfnoPkgPath of
      Nothing -> return ()
      Just path -> dumpFromPkgJson path
    dumpPackages
  return ()
