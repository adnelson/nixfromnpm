{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
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


---------- TODO: recursive sets

data NixExpr
  = Var Name
  | Num Int
  | Bool Bool
  | Null
  | OneLineString NixString
  | MultiLineString NixString
  | Path FilePath
  | List [NixExpr]
  | Set Bool [NixAssign]
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
isValidIdentifier "" = False
isValidIdentifier (unpack -> c:cs) = validFirst c && validRest cs
  where validFirst c = isAlpha c || c == '-' || c == '_'
        validRest (c:cs) = (validFirst c || isDigit c) && validRest cs
        validRest "" = True

renderPath :: [NixString] -> Text
renderPath = mapJoinBy "." ren where
  ren (Plain txt) | isValidIdentifier txt = txt
  ren txt = renderOneLineString txt

renderAssign :: NixAssign -> Text
renderAssign (Assign p e) = renderPath p <> " = " <> renderNixExpr e <> ";"
renderAssign (Inherit maybE names) = do
  let ns = joinBy " " $ HS.toList names
      e = maybe "" (\e -> " " <> renderParens e <> " ") maybE
  "inherit " <> e <> ns <> ";"

renderOneLineString :: NixString -> Text
renderOneLineString s = "\"" <> escape escapeSingle s <> "\""

renderMultiLineString :: NixString -> Text
renderMultiLineString s = "''" <> escape escapeMulti s <> "''"

renderParens e | isTerm e = renderNixExpr e
renderParens e = "(" <> renderNixExpr e <> ")"

renderKwargs :: [(Name, Maybe NixExpr)] -> Bool -> Text
renderKwargs ks dotdots = case (ks, dotdots) of
  ([], True) -> "{...}"
  ([], False) -> "{}"
  (ks, True) -> "{" <> ren ks <> ", ...}"
  (ks, False) -> "{" <> ren ks <> "}"
  where ren ks = mapJoinBy ", " ren' ks
        ren' (k, Nothing) = k
        ren' (k, Just e) = k <> " ? " <> renderNixExpr e

renderFuncArgs :: FuncArgs -> Text
renderFuncArgs (Arg a) = a
renderFuncArgs (Kwargs k dotdots mname) =
  let args = renderKwargs (H.toList k) dotdots
  in args <> maybe "" (\n -> " @ " <> n) mname

renderDot :: NixExpr -> [NixString] -> Maybe NixExpr -> Text
renderDot e pth alt = renderParens e <> rpth <> ralt where
  rpth = case pth of {[] -> ""; _ -> "." <> renderPath pth}
  ralt = case alt of {Nothing -> ""; Just e' -> " or " <> renderNixExpr e'}

isTerm :: NixExpr -> Bool
isTerm (Var _) = True
isTerm (Num _) = True
isTerm (Bool _) = True
isTerm Null = True
isTerm (Path p) = True
isTerm (OneLineString _) = True
isTerm (MultiLineString _) = True
isTerm (List _) = True
isTerm (Set _ _) = True
isTerm (Dot _ _ Nothing) = True
isTerm _ = False

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
  List es -> "[" <> mapJoinBy " " renderNixExpr es <> "]"
  Set True asns -> "rec " <> renderNixExpr (Set False asns)
  Set False asns -> "{" <> concatMap renderAssign asns <> "}"
  Let asns e -> concat ["let ", concatMap renderAssign asns, " in ",
                        renderNixExpr e]
  Function arg e -> renderFuncArgs arg <> ": " <> renderNixExpr e
  Apply e1@(Apply _ _) e2 -> renderNixExpr e1 <> " " <> renderNixExpr e2
  Apply e1 e2 -> renderNixExpr e1 <> " " <> renderParens e2
  With e1 e2 -> "with " <> renderNixExpr e1 <> "; " <> renderNixExpr e2
  Assert e1 e2 -> "assert " <> renderNixExpr e1 <> "; " <> renderNixExpr e2
  If e1 e2 e3 -> "if " <> renderNixExpr e1 <> " then "
                       <> renderNixExpr e2 <> " else " <> renderNixExpr e3
  Dot e pth alt -> renderDot e pth alt
  BinOp e1 op e2 -> renderParens e1 <> " " <> op <> " " <> renderParens e2
  Not e -> "!" <> renderNixExpr e

escapeSingle :: String -> String
escapeSingle s = case s of
  '$':'{':s' -> '\\':'$':'{':escapeSingle s'
  '\n':s' -> '\\':'n':escapeSingle s'
  '\t':s' -> '\\':'t':escapeSingle s'
  '\r':s' -> '\\':'r':escapeSingle s'
  '\b':s' -> '\\':'b':escapeSingle s'
  c:s' -> c : escapeSingle s'
  "" -> ""

escapeMulti :: String -> String
escapeMulti s = case s of
  '$':'{':s' -> '\\':'$':'{':escapeMulti s
  c:s' -> c : escapeMulti s'
  "" -> ""

escape :: (String -> String) -> NixString -> Text
escape esc (Plain s) = pack $ esc $ unpack s
escape esc (Antiquote s e s') = concat [escape esc s, "${", renderNixExpr e,
                                        "}", escape esc s']
