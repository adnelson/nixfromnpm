{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tools for parsing NPM version range indicators.
module NixFromNpm.Npm.Version.Parser where

import qualified Prelude as P
import Text.Parsec hiding (many, (<|>), spaces, parse, State, uncons)
import qualified Text.Parsec as Parsec

import Data.Aeson
import qualified Data.Aeson.Types as DAT

import Data.SemVer (pSemVerRange, anyVersion)
import NixFromNpm.Npm.Version
import NixFromNpm.Git.Types hiding (Tag)

import NixFromNpm.Common hiding (try)

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
pInt = lexeme pInt'

-- | Parses an integer without consuming trailing spaces.
pInt' :: Parser Int
pInt' = P.read <$> many1 digit


pUri :: Parser NpmVersionRange
pUri = try $ fmap NpmUri $ do
  parseURI <$> many anyChar >>= \case
    Nothing -> unexpected "Not a valid URI"
    Just uri -> case uriScheme uri of
      "git:" -> return uri
      "git+http:" -> return uri
      "git+https:" -> return uri
      "http:" -> return uri
      "https:" -> return uri
      scheme -> unexpected ("Unknown URI scheme " <> scheme)

pGitId :: Parser NpmVersionRange
pGitId = try $ do
  let sources = choice $ map (try . sstring) ["github", "gitlab", "gist",
                                              "bitbucket"]
  source <- optionMaybe (sources <* char ':') >>= \case
    Just "github" -> return Github
    Just "gitlab" -> return GitLab
    Just "bitbucket" -> return Bitbucket
    Just "gist" -> return Gist
    Just proto -> unexpected $ "Unknown git protocol '" <> proto <> "'"
    Nothing -> return Github
  account <- many1 $ noneOf ":/"
  char '/'
  repo <- many1 $ noneOf "#"
  ref <- map (map SomeRef) $ optionMaybe $ char '#' *> (pack <$> many1 anyChar)
  return $ GitId source (pack account) (pack repo) ref

pLocalPath :: Parser NpmVersionRange
pLocalPath = LocalPath . fromText . pack <$> do
  -- The string must start with one of these prefixes.
  lookAhead $ choice $ map string ["/", "./", "../", "~/"]
  many anyChar

pEmptyString :: Parser NpmVersionRange
pEmptyString = try $ do
  filter (/= ' ') <$> many anyChar >>= \case
    [] -> return (SemVerRange anyVersion)
    _ -> unexpected "Not an empty string"

pTag :: Parser NpmVersionRange
pTag = do
  filter (/= ' ') <$> many anyChar >>= \case
    [] -> unexpected "empty string, not a tag"
    tag -> return $ Tag $ pack tag

pNpmVersionRange :: Parser NpmVersionRange
pNpmVersionRange = choice [pEmptyString,
                           SemVerRange <$> pSemVerRange,
                           pUri,
                           pGitId,
                           pLocalPath,
                           pTag]

parseNpmVersionRange :: Text -> Either ParseError NpmVersionRange
parseNpmVersionRange = parse pNpmVersionRange

instance FromJSON NpmVersionRange where
  parseJSON v = case v of
    String s -> case parseNpmVersionRange s of
      Left err -> DAT.typeMismatch
                    ("valid NPM version (got " <> show v <> ")"
                     <> " Error: " <> show err) v
      Right range -> return range
    _ -> DAT.typeMismatch "string" v
