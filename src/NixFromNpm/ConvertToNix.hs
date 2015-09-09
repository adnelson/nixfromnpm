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
import NixFromNpm.NpmLookup


_startingSrc :: String
_startingSrc = "\
  \{nixpkgs ? import <nixpkgs> {}}:\n\
  \let\n\
  \  allPkgs = nixpkgs // nodePkgs // {inherit (nixpkgs.nodePackages)\n\
  \      buildNodePackage;};\n\
  \  callPackage = pth: overrides: let\n\
  \    f = import pth;\n\
  \  in\n\
  \    f ((builtins.intersectAttrs (builtins.functionArgs f) allPkgs)\n\
  \       // overrides);\n\
  \  nodePkgs = byVersion // defaults;\n\
  \in\n\
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
mkDefaultNix :: Record (HashMap SemVer a) -> NExpr
mkDefaultNix rec = do
  let mkPath' = mkPath False . unpack
      toPath name ver = mkPath' $ concat ["./", name, "/", toDotNix ver]
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
      newBindings = ["byVersion" `bindTo` byVersion,
                     "defaults" `bindTo` defaults]
  modifyFunctionBody (appendBindings newBindings) _startingExpr

dumpPkgs :: MonadIO m
         => String
         -> Record (HashMap SemVer (Either NExpr ResolvedPkg))
         -> m ()
dumpPkgs path rPkgs = liftIO $ do
  createDirectoryIfMissing True path
  withDir path $ forM_ (H.toList rPkgs) $ \(pkgName, pkgVers) -> do
    writeFile "default.nix" $ show $ prettyNix $ mkDefaultNix rPkgs
    let subdir = path </> unpack pkgName
    createDirectoryIfMissing False subdir
    withDir subdir $ forM_ (H.toList pkgVers) $ \(ver, rpkg) -> do
      let nixexpr = case rpkg of
            -- A left value means it had already existed; we can simply
            -- dump out the package we had before.
            Left e -> e
            -- A right value means it's a ResolvedPkg object; in this case
            -- we convert it to a nix expression.
            Right r -> resolvedPkgToNix r
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

findExisting :: String -> IO (Record (HashMap SemVer NExpr))
findExisting path = do
  putStrLn "Searching for existing expressions..."
  doesDirectoryExist path >>= \case
    False -> return mempty
    True -> withDir path $ do
      contents <- getDirectoryContents "."
      verMaps <- forM contents $ \dir -> do
        exprs <- doesDirectoryExist dir >>= \case
          True -> withDir dir $ do
            contents <- getDirectoryContents "."
            let files = filter (endswith ".nix") contents
            catMaybes <$> mapM parseVersion files
          False -> do
            putStrsLn [pack dir, " is not a directory"]
            return mempty -- not a directory
        case exprs of
          [] -> return Nothing
          vs -> return $ Just (pack dir, H.fromList exprs)
      let total = sum $ map (H.size . snd) $ catMaybes verMaps
      putStrsLn ["Found ", render total, " existing expressions"]
      return $ H.fromList $ catMaybes verMaps

dumpPkgNamed :: Bool -> Text -> Text -> Maybe Text -> IO ()
dumpPkgNamed noExistCheck name path token = do
  existing <- if noExistCheck then pure mempty else findExisting $ unpack path
  pwd <- getCwd
  getPkg name existing token >>= dumpPkgs (pwd </> unpack path)

dumpPkgFromOptions :: NixFromNpmOptions -> IO ()
dumpPkgFromOptions NixFromNpmOptions{..} = do
  dumpPkgNamed nfnoNoCache nfnoPkgName nfnoOutputPath nfnoGithubToken
