{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tools for parsing NPM version range indicators.
module NixFromNpm.Parsers.NpmVersion where

import Data.Aeson
import qualified Data.Aeson.Types as DAT

import NixFromNpm.SemVer
import NixFromNpm.Parsers.Common
import NixFromNpm.Parsers.SemVer
import NixFromNpm.NpmVersion
import NixFromNpm.GitTypes hiding (Tag)

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
