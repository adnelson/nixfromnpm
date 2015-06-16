{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module NixFromNpm.NixExpr where

import NixFromNpm.Common
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T

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

instance IsString NixString where
  fromString = Plain . fromString

instance IsString NixExpr where
  fromString = OneLineString . fromString

(=$=) :: Name -> NixExpr -> NixAssign
k =$= v = Assign [Plain k] v

toKwargs :: [(Name, Maybe NixExpr)] -> FuncArgs
toKwargs stuff = Kwargs (H.fromList stuff) False Nothing

renderPth = undefined

renderAssign :: NixAssign -> Text
renderAssign (Assign p e) = renderPth p <> " = " <> renderNixExpr e <> ";"

renderOneLineString :: NixString -> Text
renderOneLineString (Plain s) = pack $ show s
renderOneLineString _ = undefined

renderMultiLineString = undefined

renderKwargs :: [(Name, Maybe NixExpr)] -> Bool -> Text
renderKwargs ks dotdots = case (ks, dotdots) of
  ([], True) -> "{...}"
  ([], False) -> "{}"
  (ks, True) -> "{" <> ren ks <> ", ...}"
  (ks, False) -> "{" <> ren ks <> "}"
  where ren ks = T.intercalate ", " $ map ren' ks
        ren' (k, Nothing) = k
        ren' (k, Just e) = k <> " ? " <> renderNixExpr e

renderFuncArgs :: FuncArgs -> Text
renderFuncArgs (Arg a) = a
renderFuncArgs (Kwargs k dotdots mname) =
  let args = renderKwargs (H.toList k) dotdots
  in args <> maybe "" (\n -> " @ " <> n) mname

renderDot :: NixExpr -> [NixString] -> Maybe NixExpr -> Text
renderDot = undefined

renderNixExpr :: NixExpr -> Text
renderNixExpr = \case
  Var name -> name
  Num n -> pack $ show n
  Bool True -> "true"
  Bool False -> "false"
  Null -> "null"
  OneLineString s -> renderOneLineString s
  MultiLineString s -> renderMultiLineString s
  Path pth -> pack $ show pth
  List es -> T.intercalate " " $ map renderNixExpr es
  Set asns -> "{" <> concatMap renderAssign asns <> "}"
  Let asns e -> concat ["let ", concatMap renderAssign asns, " in ",
                        renderNixExpr e]
  Function arg e -> renderFuncArgs arg <> ": " <> renderNixExpr e
  Apply e1 e2@(Apply _ _) ->
    renderNixExpr e1 <> " (" <> renderNixExpr e2 <> ")"
  Apply e1 e2 -> renderNixExpr e1 <> " " <> renderNixExpr e2
  With e1 e2 -> "with " <> renderNixExpr e1 <> "; " <> renderNixExpr e2
  Assert e1 e2 -> "assert " <> renderNixExpr e1 <> "; " <> renderNixExpr e2
  If e1 e2 e3 -> "if " <> renderNixExpr e1 <> " then "
                       <> renderNixExpr e2 <> " else " <> renderNixExpr e3
  Dot e pth alt -> renderDot e pth alt
  BinOp e1 op e2 -> "(" <> renderNixExpr e1 <> ")" <> op <>
                    "(" <> renderNixExpr e2 <> ")"
  Not e -> "! " <> renderNixExpr e
