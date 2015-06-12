{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module NixFromNpm.Parsers.SemVer (
    parseSemVer, parseSemVerRange, pSemVerRange, pSemVer
  ) where

import qualified Prelude as P
import NixFromNpm.Parsers.Common
import NixFromNpm.SemVer

-- | Parse a string as a version range, or return an error.
parseSemVerRange :: Text -> Either ParseError SemVerRange
parseSemVerRange = parse pSemVerRange

-- | Parse a string as an explicit version, or return an error.
parseSemVer :: Text -> Either ParseError SemVer
parseSemVer = parse pSemVer

-- | Parses a semantic version.
pSemVer :: Parser SemVer
pSemVer = wildcardToSemver <$> pWildCard

pVersionComp :: Parser SemVerRange
pVersionComp = do
  comparator <- cmp
  ver <- pSemVer
  let func = case comparator of {"=" -> Eq; ">" -> Gt; "<" -> Lt;
                                 ">=" -> Geq; "<=" -> Leq; "==" -> Eq}
  return $ func ver

-- | Parses a comparison operator.
cmp :: Parser String
cmp = choice $ fmap (try . sstring) [">=", "<=", ">", "<", "==", "="]

-- | Parses versions with an explicit range qualifier (gt, lt, etc).
pSemVerRangeSingle :: Parser SemVerRange
pSemVerRangeSingle = choice [
    wildcardToRange <$> pWildCard,
    tildeToRange <$> pTildeRange,
    caratToRange <$> pCaratRange,
    pVersionComp
  ]

-- | Parses semantic version ranges joined with Ands and Ors.
pJoinedSemVerRange :: Parser SemVerRange
pJoinedSemVerRange = do
  first <- pSemVerRangeSingle
  option first $ do
    lookAhead (sstring "||" <|> cmp) >>= \case
      "||" -> Or first <$> (sstring "||" *> pJoinedSemVerRange)
      _ -> And first <$> pJoinedSemVerRange

-- | Parses a hyphenated range.
pHyphen :: Parser SemVerRange
pHyphen = hyphenatedRange <$> pWildCard <*> (sstring "-" *> pWildCard)

-- | Parses a "wildcard" (which is a possibly partial semantic version).
pWildCard :: Parser Wildcard
pWildCard = try $ do
  let seps = choice $ map sstring ["x", "X", "*"]
  let bound = choice [seps *> pure Nothing, Just <$> pInt]
  let stripNothings [Nothing] = []
      stripNothings (Just x:xs) = x : stripNothings xs
  takeWhile isJust <$> sepBy1 bound (sstring ".") >>= \case
    [] -> return Any
    [Just n] -> return $ One n
    [Just n, Just m] -> return $ Two n m
    [Just n, Just m, Just o] -> return $ Three n m o
    w -> unexpected ("Invalid version " ++ show w)

-- | Parses a tilde range (~1.2.3).
pTildeRange :: Parser Wildcard
pTildeRange = sstring "~" *> pWildCard

-- | Parses a carat range (^1.2.3).
pCaratRange :: Parser Wildcard
pCaratRange = sstring "^" *> pWildCard

-- | Top-level parser. Parses a semantic version range.
pSemVerRange :: Parser SemVerRange
pSemVerRange = try pHyphen <|> pJoinedSemVerRange
