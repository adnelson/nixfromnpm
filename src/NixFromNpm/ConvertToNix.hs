{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module NixFromNpm.ConvertToNix where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T

import NixFromNpm.Common
import NixFromNpm.NixExpr
import NixFromNpm.Parsers.Nix
import NixFromNpm.NpmTypes
import NixFromNpm.SemVer
import NixFromNpm.NpmLookup


_startingSrc :: Text
_startingSrc = unlines [
  "{nixpkgs ? import <nixpkgs> {}}:",
  "let",
  "  allPkgs = nixpkgs // nodePkgs // {inherit (nixpkgs.nodePackages)",
  "      buildNodePackage;};",
  "  callPackage = pth: overrides: let",
  "    f = import pth;",
  "  in",
  "    f ((builtins.intersectAttrs (builtins.functionArgs f) allPkgs)",
  "       // overrides);",
  "  nodePkgs = byVersion // defaults;",
  "in",
  "nodePkgs"
  ]

_startingExpr :: NixExpr
_startingExpr = fromRight $ parseNix _startingSrc

callPackage :: NixExpr -> NixExpr
callPackage e = Apply (Apply (Var "callPackage") e) (Set False [])

fixName :: Name -> Name
fixName = T.replace "." "-"

toDepName :: Name -> SemVer -> Name
toDepName name (a, b, c) = concat [fixName name, "_", pack $
                                   intercalate "-" $ map show [a, b, c]]

toDotNix :: SemVer -> Text
toDotNix (a,b,c) = pack $ intercalate "." (map show [a,b,c]) <> ".nix"

distInfoToNix :: DistInfo -> NixExpr
distInfoToNix DistInfo{..} = Apply (Var "fetchurl") $ Set False
  [ "url" =$= str diUrl,
    "sha1" =$=  str diShasum ]

resolvedPkgToNix :: ResolvedPkg -> NixExpr
resolvedPkgToNix ResolvedPkg{..} = do
  let deps = map (Var . uncurry toDepName) $ H.toList rpDependencies
      _funcParams = map (uncurry toDepName) (H.toList rpDependencies)
                    <> ["buildNodePackage", "fetchurl"]
      funcParams = toKwargs $ map (\x -> (x, Nothing)) _funcParams
  let args = Set False $ catMaybes [
        Just $ "name" =$= fromString (unpack rpName),
        Just $ "version" =$= fromString (renderSV' rpVersion),
        Just $ "src" =$= distInfoToNix rpDistInfo,
        maybeIf (length deps > 0) $ "deps" =$= List deps
        ]
  Function funcParams $ Apply (Var "buildNodePackage") args

mkDefault :: Record (HashMap SemVer a) -> NixExpr
mkDefault rec = do
  let mkPath name ver = fromText $ concat ["./", name, "/", toDotNix ver]
      mkAssign name ver = Assign [Plain $ toDepName name ver]
                                 (callPackage $ Path $ mkPath name ver)
      mkAssigns name vers = map (mkAssign name) vers
      mkDefVer name vers = do
        Assign [Plain $ fixName name] (Var $ toDepName name $ maximum vers)
      -- This bit of map gymnastics will create a list of pairs of names
      -- with all of the versions of that name that we have.
      versOnly = map (map (map fst)) $ H.toList $ map H.toList rec
      byVersion = Set False $ concatMap (uncurry mkAssigns) versOnly
      defaults = With (Var "byVersion") $
        Set False $ map (uncurry mkDefVer) versOnly
      newAssigns = ["byVersion" =$= byVersion, "defaults" =$= defaults]
      Function params (Let assigns e) = _startingExpr
  Function params (Let (assigns <> newAssigns) e)

dumpPkgs :: MonadIO m
         => String
         -> Record (HashMap SemVer ResolvedPkg)
         -> m ()
dumpPkgs path rPkgs = liftIO $ do
  createDirectoryIfMissing True path
  withDir path $ forM_ (H.toList rPkgs) $ \(pkgName, pkgVers) -> do
    writeFile "default.nix" $ renderNixExpr $ mkDefault rPkgs
    let subdir = path </> unpack pkgName
    createDirectoryIfMissing False subdir
    withDir subdir $ forM_ (H.toList pkgVers) $ \(ver, rpkg) -> do
      let nixexpr = resolvedPkgToNix rpkg
      writeFile (unpack $ toDotNix ver) $ renderNixExpr nixexpr

dumpPkgNamed :: Text -> Text -> IO ()
dumpPkgNamed name path = getPkg name >>= dumpPkgs (unpack path)
