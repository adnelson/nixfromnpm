{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module NixFromNpm.ConvertToNix where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

import NixFromNpm.Common
import Nix.Types
import Nix.Parser
import Nix.Pretty (prettyNix)
import NixFromNpm.Options
import NixFromNpm.NpmTypes
import NixFromNpm.SemVer
import NixFromNpm.Parsers.SemVer
import NixFromNpm.NpmLookup (getPkg)


_startingSrc :: String
_startingSrc = "\
  \{nixpkgs ? import <nixpkgs> {}}:                                  \
  \let                                                               \
  \  inherit (builtins) attrValues;                                  \
  \  joinSets = foldl (a: b: a // b) {};                             \
  \  joinedExtensions = joinSets (attrValues extensions);            \
  \  allPkgs = nixpkgs // nodePkgs // joinedExtensions //            \
  \   {inherit (nixpkgs.nodePackages)buildNodePackage;};             \
  \  callPackage = nixpkgs.lib.callPackageWith allPkgs;              \
  \  nodePkgs = byVersion // defaults;                               \
  \in                                                                \
  \nodePkgs"

_startingExpr :: NExpr
_startingExpr = case parseNixString _startingSrc of
  Success e -> e
  Failure e -> error $ unlines ["Starting source failed to parse:", show e]

callPackage :: NExpr -> NExpr
callPackage = callPackageWith []

callPackageWith :: [Binding NExpr] -> NExpr -> NExpr
callPackageWith args e = mkApp (mkApp (mkSym "callPackage") e)
                               (mkNonRecSet args)

callPackageWithRec :: [Binding NExpr] -> NExpr -> NExpr
callPackageWithRec args e = mkApp (mkApp (mkSym "callPackage") e)
                                  (mkRecSet args)

-- | Turns a string into one that can be used as an identifier.
fixName :: Name -> Name
fixName = T.replace "." "-"

-- | Converts a package name and semver into an identifier.
toDepName :: Name -> SemVer -> Name
toDepName name (a, b, c) = concat [fixName name, "_", pack $
                                   intercalate "-" $ map show [a, b, c]]

-- | Gets the .nix filename of a semver. E.g. (0, 1, 2) -> 0.1.2.nix
toDotNix :: SemVer -> Text
toDotNix (a,b,c) = pack $ intercalate "." (map show [a,b,c]) <> ".nix"

-- | Creates a doublequoted string from some text.
str :: Text -> NExpr
str = mkStr DoubleQuoted

-- | Converts distinfo into a nix fetchurl call.
distInfoToNix :: DistInfo -> NExpr
distInfoToNix DistInfo{..} = mkApp (mkSym "fetchurl") $ mkNonRecSet
  [ "url" `bindTo` str diUrl,
    "sha1" `bindTo`  str diShasum ]

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
      deps = map (mkSym . uncurry toDepName) $ H.toList rpDependencies
      -- Get the parameters of the package function (deps + utility functions).
      _funcParams = map (uncurry toDepName) (H.toList rpDependencies)
                    <> ["buildNodePackage", "fetchurl"]
      -- None of these have defaults, so put them into pairs with Nothing.
      funcParams = mkFormalSet $ map (\x -> (x, Nothing)) _funcParams
  let args = mkNonRecSet $ catMaybes [
        Just $ "name" `bindTo` str rpName,
        Just $ "version" `bindTo` (str $ renderSV rpVersion),
        Just $ "src" `bindTo` distInfoToNix rpDistInfo,
        maybeIf (length deps > 0) $ "deps" `bindTo` mkList deps,
        maybeIf (metaNotEmpty rpMeta) $ "meta" `bindTo` metaToNix rpMeta
        ]
  mkFunction funcParams $ mkApp (mkSym "buildNodePackage") args

-- | Creates the `default.nix` file that is the top-level expression we are
-- generating.
mkDefaultNix :: Record (HashMap SemVer a)
             -> Record Path -- ^ Map of libraries being included.
             -> NExpr
mkDefaultNix rec extensionMap = do
  let mkPath' = mkPath False . unpack
      toPath name ver = mkPath' $ concat ["./", name, "/", toDotNix ver]
      -- Make a set of all of the extensions
      extensionsSet = mkNonRecSet $
        -- Map over the expression map, creating a binding for each pair.
        flip map (H.toList extensionMap) $ \(name, path) ->
          name `bindTo` mkApp (mkSym "import") (mkPath False (unpack path))
      mkBinding name ver = toDepName name ver
                            `bindTo` callPackage (toPath name ver)
      mkBindings name vers = map (mkBinding name) vers
      mkDefVer name vers = do
        fixName name `bindTo` mkSym (toDepName name $ maximum vers)
      -- This bit of map gymnastics will create a list of pairs of names
      -- with all of the versions of that name that we have.
      versOnly = map (map (map fst)) $ H.toList $ map H.toList rec
      byVersion = mkNonRecSet $ concatMap (uncurry mkBindings) versOnly
      defaults = mkWith (mkSym "byVersion") $
        mkNonRecSet $ map (uncurry mkDefVer) versOnly
      newBindings = ["extensions" `bindTo` extensionsSet,
                     "byVersion" `bindTo` byVersion,
                     "defaults" `bindTo` defaults]
  modifyFunctionBody (appendBindings newBindings) _startingExpr

takeNewPackages :: Record (HashMap SemVer (Either NExpr ResolvedPkg))
                -> Record (HashMap SemVer ResolvedPkg)
takeNewPackages startingRec = do
  let takeNews = modifyMap $ \case Left _ -> Nothing
                                   Right rpkg -> Just rpkg
      takeNonEmptys = H.filter (not . H.null)
  takeNonEmptys $ H.map takeNews startingRec

dumpPkgs :: MonadIO m
         => String
         -> Record (HashMap SemVer (Either NExpr ResolvedPkg))
         -> Record Path -- ^ Libraries being extended.
         -> m ()
dumpPkgs path rPkgs extensions = liftIO $ do
  let newPackages = takeNewPackages rPkgs
  -- if there aren't any new packages, we can stop here
  if H.null newPackages
  then putStrLn "No new packages created." >> return ()
  else do
    putStrsLn ["Creating new packages at ", pack path]
    createDirectoryIfMissing True path
    withDir path $ forM_ (H.toList newPackages) $ \(pkgName, pkgVers) -> do
      writeFile "default.nix" $ show $ prettyNix $ mkDefaultNix rPkgs extensions
      let subdir = path </> unpack pkgName
      -- If we don't have any new packages, we don't need to do
      -- anything.
      createDirectoryIfMissing False subdir
      withDir subdir $ forM_ (H.toList pkgVers) $ \(ver, rpkg) -> do
        let nixexpr = resolvedPkgToNix rpkg
        writeFile (unpack $ toDotNix ver) $ show $ prettyNix nixexpr

parseVersion :: String -> IO (Maybe (SemVer, NExpr))
parseVersion pth = do
  case parseSemVer . pack $ dropSuffix ".nix" $ takeBaseName pth of
    Left _ -> return Nothing -- not a version file
    Right version -> parseNixString . pack <$> readFile pth >>= \case
      Failure err -> do
        putStrsLn [pack pth, " failed to parse: ", pack $ show err]
        return Nothing -- invalid nix, should overwrite
      Success expr -> return $ Just (version, expr)

-- | Given the path to a file possibly containing nix expressions, finds all
--   expressions findable at that path and returns a map of them.
findExisting :: Bool -- ^ Whether to fail if the path does not exist.
             -> Path -- ^ The path to search.
             -> IO (Record (HashMap SemVer NExpr)) -- ^ Mapping of package
                                                   --   names to maps of
                                                   --   versions to nix
                                                   --   expressions.
findExisting failIfNotExists path = do
  doesDirectoryExist (unpack path) >>= \case
    False -> if failIfNotExists
             then errorC ["Path ", path, " does not exist."]
             else return mempty
    True -> withDir (unpack path) $ do
      putStrsLn ["Searching for existing expressions in ", path, "..."]
      contents <- getDirectoryContents "."
      verMaps <- forM contents $ \dir -> do
        exprs <- doesDirectoryExist dir >>= \case
          True -> withDir dir $ do
            contents <- getDirectoryContents "."
            let files = filter (endswith ".nix") contents
            catMaybes <$> mapM parseVersion files
          False -> do
            return mempty -- not a directory
        case exprs of
          [] -> return Nothing
          vs -> return $ Just (pack dir, H.fromList exprs)
      let total = sum $ map (H.size . snd) $ catMaybes verMaps
      putStrsLn ["Found ", render total, " existing expressions"]
      return $ H.fromList $ catMaybes verMaps

-- | Given the name of a package and a place to dump expressions to, generates
--   the expressions needed to build that package.
dumpPkgNamed :: Bool        -- ^ Whether to skip the existence check.
             -> Text        -- ^ The name of the package to fetch.
             -> Text        -- ^ The path to output to.
             -> Record Path -- ^ Mapping of names of libraries to extend,
                            --   and paths to those libraries.
             -> Maybe Text  -- ^ Optional github token.
             -> IO ()
dumpPkgNamed noExistCheck name path toExtend token = do
  existing <- if noExistCheck
              then pure mempty
              else findExisting False path
  libraries <- fmap concat $ forM (H.toList toExtend) $ \(name, path) -> do
    findExisting True path
  pwd <- getCurrentDirectory
  packages <- getPkg name (existing <> libraries) token
  dumpPkgs (pwd </> unpack path) packages toExtend

getExtensions :: [Text] -> Record Path
getExtensions = foldl' step mempty where
  step :: Record Path -> Text -> Record Path
  step exts nameEqPath = case T.split (== '=') nameEqPath of
    [name, path] -> case H.lookup name exts of
      Nothing -> H.insert name path exts
      Just path' -> errorC ["Extension ", name, " is mapped to both path ",
                            path, " and path ", path']
    _ -> errorC ["Extensions must be of the form NAME=PATH (in argument ",
                 nameEqPath, ")"]

dumpPkgFromOptions :: NixFromNpmOptions -> IO ()
dumpPkgFromOptions NixFromNpmOptions{..} = do
  forM_ nfnoPkgNames $ \name -> do
    let extensions = getExtensions nfnoExtendPaths
    dumpPkgNamed nfnoNoCache name nfnoOutputPath extensions nfnoGithubToken
