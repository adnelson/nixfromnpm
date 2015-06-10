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

data GitSource = Github | Bitbucket | Gist | GitLab deriving (Show, Eq)

data NpmVersionRange
  = SemVerRange SemVerRange
  | Latest -- latest stable version
  | Unstable -- latest unstable version
  | NpmUri URI
  | GitId GitSource Name Name
  | LocalPath FilePath
  deriving (Show, Eq)

pLatestUnstable :: Parser NpmVersionRange
pLatestUnstable = choice (map sstring ["latest", "unstable"]) >>= \case
  "latest" -> return Latest
  "unstable" -> return Unstable

pUri :: Parser NpmVersionRange
pUri = try $ fmap NpmUri $ do
  parseURI <$> many anyChar >>= \case
    Nothing -> unexpected "Not a valid URI"
    Just uri -> case uriScheme uri of
      "git:" -> return uri
      "http:" -> return uri
      "https:" -> return uri
      scheme -> unexpected ("Unknown URI scheme " <> scheme)

pGitId :: Parser NpmVersionRange
pGitId = do
  let sources = choice $ map sstring ["github", "gitlab", "gist",
                                      "bitbucket"]
  source <- optionMaybe sources >>= \case
    Just "github" -> return Github
    Just "gitlab" -> return GitLab
    Just "bitbucket" -> return Bitbucket
    Just "gist" -> return Gist
    Nothing -> return Github
  account <- many1 $ noneOf ":/"
  char '/'
  repo <- many1 anyChar
  let urlBase = fromJust (parseURI "https://github.com/")
  return $ GitId source (pack account) (pack repo)

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
                           pGitId, pLocalPath]

parseNpmVersionRange :: Text -> Either ParseError NpmVersionRange
parseNpmVersionRange = parse pNpmVersionRange

instance FromJSON NpmVersionRange where
  parseJSON v = case v of
    String s -> case parseNpmVersionRange s of
      Left err -> DAT.typeMismatch
                    ("valid NPM version (got " <> show v <> ")") v
      Right range -> return range
    _ -> DAT.typeMismatch "string" v
