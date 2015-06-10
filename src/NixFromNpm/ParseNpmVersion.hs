{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tools for parsing NPM version range indicators.
module NixFromNpm.ParseNpmVersion where

import Text.Parsec hiding (many, (<|>), spaces, parse)
import qualified Text.Parsec as Parsec
import Data.Aeson
import qualified Data.Aeson.Types as DAT

import NixFromNpm.Common hiding (try)
import NixFromNpm.SemVer
import NixFromNpm.ParseSemVer

data NpmVersionRange
  = SemVerRange SemVerRange
  | Latest -- latest stable version
  | Unstable -- latest unstable version
  | NpmUri URI
  | GithubId Name Name
  | LocalPath FilePath
  deriving (Show, Eq)

pLatestUnstable :: Parser NpmVersionRange
pLatestUnstable = choice (map sstring ["latest", "unstable"]) >>= \case
  "latest" -> return Latest
  "unstable" -> return Unstable

pUri :: Parser NpmVersionRange
pUri = try $ fmap NpmUri $ do
  parseAbsoluteURI <$> many anyChar >>= \case
    Nothing -> unexpected "Not a valid URI"
    Just uri -> return uri

pGithubId :: Parser NpmVersionRange
pGithubId = do
  account <- many1 $ noneOf "/"
  char '/'
  repo <- many1 anyChar
  return $ GithubId (pack account) (pack repo)

pLocalPath :: Parser NpmVersionRange
pLocalPath = LocalPath . fromText . pack <$> do
  -- The string must start with one of these prefixes.
  lookAhead $ choice $ map string ["/", "./", "../", "~/"]
  many anyChar

pEmptyString :: Parser NpmVersionRange
pEmptyString = try $ do
  filter (/= ' ') <$> many anyChar >>= \case
    [] -> return $ SemVerRange $ Geq (0, 0, 0)
    _ -> unexpected "Not an empty string"

pNpmVersionRange :: Parser NpmVersionRange
pNpmVersionRange = choice [pEmptyString,
                           SemVerRange <$> pSemVerRange,
                           pLatestUnstable, pUri,
                           pGithubId, pLocalPath]

parseNpmVersionRange :: Text -> Either ParseError NpmVersionRange
parseNpmVersionRange = parse pNpmVersionRange

instance FromJSON NpmVersionRange where
  parseJSON v = case v of
    String s -> case parseNpmVersionRange s of
      Left err -> DAT.typeMismatch
                    ("valid NPM version (got " <> show v <> ")") v
      Right range -> return range
    _ -> DAT.typeMismatch "string" v
