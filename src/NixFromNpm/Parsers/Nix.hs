{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module NixFromNpm.Parsers.Nix where

import qualified Prelude as P
import NixFromNpm.Parsers.Common hiding (spaces, spaces1, sstring, lexeme,
                                         schar, pInt)
import qualified Data.Set as S
import qualified Data.HashMap.Strict as H

import NixFromNpm.NixExpr

comment :: Parser ()
comment = lineComment <|> blockComment >> return ()
  where
    lineComment = do
      char '#'
      manyTill anyChar (choice [char '\n' >> return (), eof])
    blockComment = try $ do
      string "/*"
      manyTill anyChar (choice [try $ string "*/" >> return (), eof])

-- | Consumes any spaces (not other whitespace).
spaces :: Parser ()
spaces = many (oneOf "\n\t " *> return () <|> comment) >> return ()

-- | Consumes at least one space (not other whitespace).
spaces1 :: Parser ()
spaces1 = char ' ' >> spaces

-- | Parses the given string and any trailing spaces.
sstring :: String -> Parser String
sstring = lexeme . string

-- | Parses the given character and any trailing spaces.
schar :: Char -> Parser Char
schar = lexeme . char

-- | Parses `p` and any trailing spaces.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parses an integer.
pInt :: Parser Int
pInt = lexeme $ P.read <$> many1 digit

-- | Parses the given string. Does not fail if it's a keyword.
keyword :: String -> Parser String
keyword = try . sstring

-- | Parses an identifier
pIdentifier :: Parser Text
pIdentifier = notKeyword $ lexeme $ do
  first <- letter <|> char '_'
  rest <- many $ letter <|> digit <|> char '_' <|> char '-'
  return $ pack $ first : rest

-- | Parses `p`, but fails if the result is a reserved word.
notKeyword :: Parser Text -> Parser Text
notKeyword p = try $ do
  ident <- p
  if ident `member` keywords then unexpected $ "keyword " <> show ident
  else return ident

-- | Set of keywords.
keywords :: Set Text
keywords = S.fromList ["if", "then", "else", "null", "inherit",
                       "true", "false", "let", "in", "with", "assert"]

-- | Set of operators.
operators :: Set String
operators = S.fromList ["+", "-", "*", "/", "++", "&&", "||", "//", "?",
                        "==", "!=", ">=", "<=", ">", "<"]

-- | Characters found in operators.
opChars :: [Char]
opChars = "+-*/&|?=><!$"

addLeft :: Text -> NixString -> NixString
addLeft txt (Plain p) = Plain (txt <> p)
addLeft txt (Antiquote s e s') = Antiquote (addLeft txt s) e s'

-- | Parses an interpolated string, without parsing quotes.
pInterp :: Bool -> Parser NixString
pInterp inMultiLine = do
  plain <- pack <$> many (noneOf "$\\\"'")
  let stop = return $ Plain plain
      continue = pInterp inMultiLine
      -- Append `s` to what we're building, and keep parsing.
      continueWith s = fmap (addLeft (plain <> s)) continue
      consume c = char c >> continueWith (singleton c)
  option (Plain plain) $ do
    lookAhead (oneOf "$\\\"'") >>= \case
      '$' -> char '$' >> lookAhead anyChar >>= \case
        -- If it's an open parens, grab what's in the parens.
        '{' -> Antiquote (Plain plain) <$> curlies <*> continue
        -- If it's another dollar sign, grab both dollar signs.
        '$' -> char '$' >> continueWith "$$"
        -- Otherwise, just keep going.
        c -> continueWith "$"
      -- If there's a backslash, we're escaping whatever's next.
      '\\' -> char '\\' >> singleton <$> anyChar >>= continueWith
      -- If it's a quote and we're not in a multi-line string, return.
      -- Note that we're not consuming the quote;
      -- that happens in the outer parser calling this function.
      '"' | not inMultiLine -> stop
      -- If we're in a multiline string, we can just keep going.
      '"' -> consume '"'
      -- A single quote in a non multiline string is fine.
      '\'' | not inMultiLine -> consume '\''
      -- If we see a single quote followed by another one, and we're in
      -- a multiline string, we stop here.
      '\'' -> choice [lookAhead (string "''") >> stop,
                      consume '\'']
  where curlies = between (schar '{') (char '}') pNixExpr

pOneLineString :: Parser NixString
pOneLineString = between (char '"') (schar '"') $ pInterp False

pMultiLineString :: Parser NixString
pMultiLineString = between (string "''") (sstring "''") $ pInterp True

pString :: Parser NixExpr
pString = choice [OneLineString <$> pOneLineString,
                  MultiLineString <$> pMultiLineString]

pVar :: Parser NixExpr
pVar = try $ fmap Var $ pIdentifier <* notFollowedBy (char ':')

pBool :: Parser NixExpr
pBool = choice (map keyword ["true", "false"]) >>= \case
  "true" -> return $ Bool True
  "false" -> return $ Bool False

pNull :: Parser NixExpr
pNull = keyword "null" >> return Null

pFuncArgs :: Parser FuncArgs
pFuncArgs = choice [Arg <$> pIdentifier, pKwargs]

-- | Gets all of the arguments for a function.
pKwargs :: Parser FuncArgs
pKwargs = do
  (args, dotdots) <- between (schar '{') (schar '}') _getKwargs
  argsNname <- optionMaybe $ schar '@' >> pIdentifier
  return $ Kwargs (H.fromList args) dotdots argsNname
  where
    _getKwargs :: Parser ([(Name, Maybe NixExpr)], Bool)
    _getKwargs = go [] where
      -- Attempt to parse `...`. If this succeeds, stop and return True.
      -- Otherwise, attempt to parse an argument, optionally with a
      -- default. If this fails, then return what has been accumulated
      -- so far.
      go acc = (sstring "..." >> return (acc, True)) <|> getMore acc
      getMore acc = do
        -- Could be nothing, in which just return what we have so far.
        option (acc, False) $ do
          -- Get an argument name and an optional default.
          pair <- liftA2 (,) pIdentifier (optionMaybe $ schar '?' >> pNixExpr)
          -- Either return this, or attempt to get a comma and restart.
          option (acc `snoc` pair, False) $ do
            schar ','
            go (acc `snoc` pair)

pFunction :: Parser NixExpr
pFunction = try $ do
  args <- pFuncArgs
  sstring ":"
  body <- pNixExpr
  return $ Function args body

pSet :: Parser NixExpr
pSet = try $ Set <$> between (schar '{') (schar '}') (many pNixAssign)

pLet :: Parser NixExpr
pLet = liftA2 Let (between (keyword "let") (keyword "in") $ many pNixAssign)
                  pNixExpr

pNixAssign :: Parser NixAssign
pNixAssign = choice [inherit, assign] <* schar ';' where
  inherit = keyword "inherit" >> do
    from <- optionMaybe pParens
    names <- S.fromList <$> many pIdentifier
    return $ Inherit from names
  assign = do
    assignee <- pKeyPath
    schar '='
    val <- pNixExpr
    return $ Assign assignee val

pPath :: Parser NixExpr
pPath = lexeme $ try $ do
  dots <- option "" $ try (string "..") <|> string "."
  rest <- try $ do
    char '/' <* notFollowedBy (char '*')
    path <- many1 (noneOf "\n\t ;#(){}")
    return $ '/' : path
  return $ Path $ fromString $ dots <> rest

pList :: Parser NixExpr
pList = fmap List $ between (schar '[') (schar ']') $ many pDot

pParens :: Parser NixExpr
pParens = between (schar '(') (schar ')') pNixExpr

pNixTerm :: Parser NixExpr
pNixTerm = choice [pString, pUriString, pVar, Num <$> pInt,
                   pBool, pNull, pList, pParens, pSet, pPath]

pKeyPath :: Parser [NixString]
pKeyPath = (Plain <$> pIdentifier <|> pOneLineString) `sepBy1` dot
  where dot = try $ schar '.' <* notFollowedBy (char '.')

pDot :: Parser NixExpr
pDot = do
  term <- pNixTerm
  option term $ try $ do
    schar '.'
    keypath <- pKeyPath
    alt <- optionMaybe (keyword "or" >> pNixExpr)
    return $ Dot term keypath alt

pNot :: Parser NixExpr
pNot = do
  optnot <- optionMaybe $ schar '!'
  expr <- pApply
  case optnot of
    Nothing -> return expr
    Just _ -> return $ Not expr

pNixExpr :: Parser NixExpr
pNixExpr = choice [pBinary, pFunction, pLet, pAssert, pWith, pIf, pNot]

pApply :: Parser NixExpr
pApply = pDot `chainl1` (pure Apply)

-- | Two expressions joined by a binary operator.
pBinary :: Parser NixExpr
pBinary = pNot `chainl1` fmap (flip BinOp) op where
  op = try $ do
    oper <- lexeme $ many1 (oneOf opChars)
    if not $ S.member oper operators
    then unexpected $ "Invalid operator " <> show oper
    else return $ pack oper

pAssert :: Parser NixExpr
pAssert = liftA2 Assert (keyword "assert" *> pNixExpr) (schar ';' *> pNixExpr)

pWith :: Parser NixExpr
pWith = liftA2 With (keyword "with" *> pNixExpr) (schar ';' *> pNixExpr)

pIf :: Parser NixExpr
pIf = liftA3 If (keyword "if" *> pNixExpr)
                (keyword "then" *> pNixExpr)
                (keyword "else" *> pNixExpr)

pUriString :: Parser NixExpr
pUriString = lexeme $ try $ do
  scheme <- many1 $ noneOf "\n\t :;"
  string ":"
  rest <- many1 $ noneOf "\n\t ;"
  return $ OneLineString $ Plain $ pack $ scheme <> ":" <> rest

pTopLevel :: Parser NixExpr
pTopLevel = pNixExpr

parseFile :: String -> IO (Either ParseError NixExpr)
parseFile = parseFileWith pTopLevel

parseFileWith :: Parser a -> String -> IO (Either ParseError a)
parseFileWith p path = parseFull (spaces >> p) <$> readFile path

_p = parseFull pTopLevel

_p' = parseFull pNixAssign

pf' = parseFileWith pNixAssign "test1.nix"

pf = parseFile "/Users/anelson/nixpkgs/pkgs/top-level/all-packages.nix"
