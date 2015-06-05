{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ParseSemVer where

import ClassyPrelude hiding (try)
import Control.Applicative
import qualified Prelude as P
import Prelude (String)
import Text.Parsec hiding (many, (<|>), spaces, parse)
import qualified Text.Parsec as Parsec
import SemVer

type Parser = ParsecT String () Identity

-- | Parse a string as a version range, or return an error.
parseSemVerRange :: String -> Either ParseError SemVerRange
parseSemVerRange = parse (pSemVerRange <* eof)

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

spaces :: Parser String
spaces = many $ char ' '

spaces1 :: Parser String
spaces1 = many1 $ char ' '

sym :: String -> Parser String
sym s = string s <* spaces

cmp :: Parser String
cmp = choice $ fmap (try . sym) [">=", "<=", ">", "<", "="]

pNum :: Parser Int
pNum = (P.read <$> many1 digit) <* spaces

-- | Parses a semantic version.
pSemVer :: Parser SemVer
pSemVer = (,,) <$> pNum <*> (sym "." *> pNum) <*> (sym "." *> pNum)

-- | Parses versions with an explicit range qualifier (gt, lt, etc).
pSemVerRangeSingle :: Parser SemVerRange
pSemVerRangeSingle = choice [wildcardToRange <$> pWildCard,
                             tildeToRange <$> pTildeRange,
                             caratToRange <$> pCaratRange,
                             Eq <$> (sym "=" *> pSemVer),
                             Gt <$> (sym ">" *> pSemVer),
                             Lt <$> (sym "<" *> pSemVer),
                             Geq <$> (sym ">=" *> pSemVer),
                             Leq <$> (sym "<=" *> pSemVer)
                            ]

-- | Joins semantic version ranges with Ands and Ors.
pJoinedSemVerRange :: Parser SemVerRange
pJoinedSemVerRange = do
  first <- pSemVerRangeSingle
  option first $ do
    lookAhead (sym "||" <|> cmp) >>= \case
      "||" -> Or first <$> (sym "||" *> pJoinedSemVerRange)
      _ -> And first <$> pJoinedSemVerRange

-- | Translates a hyphenated range to an actual range.
-- Ex: 1.2.3 - 2.3.4 := >=1.2.3 <=2.3.4
-- Ex: 1.2 - 2.3.4 := >=1.2.0 <=2.3.4
-- Ex: 1.2.3 - 2 := >=1.2.3 <3.0.0
pHyphen :: Parser SemVerRange
pHyphen = do
  wc1 <- pWildCard
  sym "-"
  wc2 <- pWildCard
  let sv1 = case wc1 of
              Any -> Geq (0, 0, 0)
              One n -> Geq (n, 0, 0)
              Two n m -> Geq (n, m, 0)
              Three n m o -> Geq (n, m, o)
  let sv2 = case wc2 of
              Any -> Geq (0, 0, 0) -- Refers to "any version"
              One n -> Lt (n+1, 0, 0)
              Two n m -> Lt (n, m + 1, 0)
              Three n m o -> Leq (n, m, o)
  return $ And sv1 sv2


-- | Parses a "wildcard" (which is a possibly partial semantic version).
pWildCard :: Parser Wildcard
pWildCard = try $ do
  let bound = choice [sym "x" *> pure Nothing, Just <$> pNum]
  sepBy1 bound (sym ".") >>= \case
    [Nothing] -> return Any
    [Just n] -> return $ One n
    [Just n, Nothing] -> return $ One n
    [Just n, Just m] -> return $ Two n m
    [Just n, Just m, Nothing] -> return $ Two n m
    [Just n, Just m, Just o] -> return $ Three n m o
    w -> unexpected ("Invalid version " ++ show w)

pTildeRange :: Parser Wildcard
pTildeRange = sym "~" *> pWildCard

pCaratRange :: Parser Wildcard
pCaratRange = sym "^" *> pWildCard

pSemVerRange :: Parser SemVerRange
pSemVerRange = pJoinedSemVerRange <|> pHyphen
