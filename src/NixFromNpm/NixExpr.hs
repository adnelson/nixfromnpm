{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module NixFromNpm.NixExpr where

import NixFromNpm.Common

data FuncArgs
  = Arg Name
  | Kwargs (HashMap Name (Maybe NixExpr)) Bool (Maybe Name)
  deriving (Show, Eq)

data NixExpr
  = Var Name
  | Num Int
  | Bool Bool
  | Null
  | OneLineString NixString
  | MultiLineString NixString
  | Path FilePath
  | List [NixExpr]
  | Set [NixAssign]
  | Let [NixAssign] NixExpr
  | Function FuncArgs NixExpr
  | Apply NixExpr NixExpr
  | With NixExpr NixExpr
  | If NixExpr NixExpr NixExpr
  | Dot NixExpr [NixString] (Maybe NixExpr)
  | BinOp NixExpr Text NixExpr
  | Not NixExpr
  | Assert NixExpr NixExpr
  deriving (Show, Eq)

data NixAssign
  = Assign [NixString] NixExpr
  | Inherit (Maybe NixExpr) (Set Name)
  deriving (Show, Eq)

data NixString
  = Plain Text
  | Antiquote NixString NixExpr NixString
  deriving (Show, Eq)
