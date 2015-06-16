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

toDepName :: Name -> SemVer -> Name
toDepName name (a, b, c) = concat [name, "_", pack $
                                   intercalate "-" $ map show [a, b, c]]

rPkgToNix :: ResolvedPkg -> NixExpr
rPkgToNix ResolvedPkg{..} = do
  let deps = map (Var . uncurry toDepName) $ H.toList rpDependencies
      _funcParams = map (uncurry toDepName) (H.toList rpDependencies)
                    <> ["buildNodePackage"]
      funcParams = toKwargs $ map (\x -> (x, Nothing)) _funcParams
  let args = Set $ catMaybes [
        Just $ "name" =$= Var rpName,
        Just $ "version" =$= fromString (renderSV' rpVersion),
        maybeIf (length deps > 0) $ "propagatedBuildInputs" =$= List deps
        ]
  Function funcParams $ Apply "buildNodePackage" args
