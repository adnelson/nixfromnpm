{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module NixFromNpm.NixExpr where

import NixFromNpm.Common
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
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
  | Inherit (Maybe NixExpr) (HashSet Name)
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

isValidIdentifier :: Name -> Bool
isValidIdentifier s = case unpack s of
  [c] -> validFirst c
  (c:cs) -> validFirst c && validRest cs
  ""  -> False
  where validFirst c = isAlpha c || c == '-' || c == '_'
        validRest (c:cs) = (validFirst c || isDigit c) && validRest cs
        validRest "" = True

renderPth :: [NixString] -> Text
renderPth = T.intercalate "." . map ren where
  ren (Plain txt) | isValidIdentifier txt = txt
  ren (Plain txt) = pack $ show txt
  ren (Antiquote _ _ _) = error "can't do antiquoted strings yet"

renderAssign :: NixAssign -> Text
renderAssign (Assign p e) = renderPth p <> " = " <> renderNixExpr e <> ";"
renderAssign (Inherit maybE names) = do
  let ns = T.intercalate " " $ HS.toList names
      e = maybe "" (\e -> " " <> renderParens e <> " ") maybE
  "inherit " <> e <> ns <> ";"

renderOneLineString :: NixString -> Text
renderOneLineString (Plain s) = pack $ show s
renderOneLineString _ = undefined

renderMultiLineString = undefined

renderParens e = "(" <> renderNixExpr e <> ")"

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
  Path pth -> pathToText pth
  List es -> "[" <> T.intercalate " " (map renderNixExpr es) <> "]"
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
  BinOp e1 op e2 -> renderParens e1 <> " " <> op <> " " <> renderParens e2
  Not e -> "! " <> renderNixExpr e

callPackage :: NixExpr -> NixExpr
callPackage e = Apply (Apply (Var "callPackage") e) (Set [])
