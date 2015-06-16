{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module NixFromNpm.ConvertToNix where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import NixFromNpm.Common
import NixFromNpm.NixExpr
import NixFromNpm.NpmTypes
import NixFromNpm.SemVer

callPackage :: NixExpr -> NixExpr
callPackage e = Apply (Apply (Var "callPackage") e) (Set False [])

toDepName :: Name -> SemVer -> Name
toDepName name (a, b, c) = concat [name, "_", pack $
                                   intercalate "-" $ map show [a, b, c]]

toDotNix :: SemVer -> Text
toDotNix (a,b,c) = pack $ intercalate "." (map show [a,b,c]) <> ".nix"

rPkgToNix :: ResolvedPkg -> NixExpr
rPkgToNix ResolvedPkg{..} = do
  let deps = map (Var . uncurry toDepName) $ H.toList rpDependencies
      _funcParams = map (uncurry toDepName) (H.toList rpDependencies)
                    <> ["buildNodePackage"]
      funcParams = toKwargs $ map (\x -> (x, Nothing)) _funcParams
  let args = Set False $ catMaybes [
        Just $ "name" =$= fromString (unpack rpName),
        Just $ "version" =$= fromString (renderSV' rpVersion),
        maybeIf (length deps > 0) $ "propagatedBuildInputs" =$= List deps
        ]
  Function funcParams $ Apply (Var "buildNodePackage") args


mkDefault :: Record (HashMap SemVer a) -> NixExpr
mkDefault rec = do
  let mkPath name ver = fromText $ concat ["./", name, "/", toDotNix ver]
      mkAssign name ver = Assign [Plain $ toDepName name ver]
                                 (callPackage $ Path $ mkPath name ver)
      mkAssigns name vers = map (mkAssign name) vers
      -- This bit of map gymnastics will create a list of pairs of names
      -- with all of the versions of that name that we have.
      versOnly :: [(Name, [SemVer])]
      versOnly = map (map (map fst)) $ H.toList $ map H.toList rec
  Set True $ concatMap (uncurry mkAssigns) versOnly


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
      writeFile (unpack $ toDotNix ver) $ renderNixExpr $ rPkgToNix rpkg

