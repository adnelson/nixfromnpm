{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Parsers.Common (
    module Text.Parsec,
    module NixFromNpm.Common,
    Parser,
    parse, parseFull, spaces, spaces1, sstring, schar, lexeme, pInt
  ) where

import qualified Prelude as P
import Text.Parsec hiding (many, (<|>), spaces, parse, State, uncons)
import qualified Text.Parsec as Parsec

import NixFromNpm.Common hiding (try)
import NixFromNpm.SemVer

type Parser = ParsecT String () Identity

-- | Given a parser and a string, attempts to parse the string.
parse :: Parser a -> Text -> Either ParseError a
parse p = Parsec.parse p "" . unpack

parseFull :: Parser a -> Text -> Either ParseError a
parseFull p = Parsec.parse (p <* eof) "" . unpack

-- | Consumes any spaces (not other whitespace).
spaces :: Parser String
spaces = many $ char ' '

-- | Consumes at least one space (not other whitespace).
spaces1 :: Parser String
spaces1 = many1 $ char ' '

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
