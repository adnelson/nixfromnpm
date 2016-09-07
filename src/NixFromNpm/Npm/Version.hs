{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Npm.Version where

import qualified Data.Text as T

import Data.SemVer
import Data.Aeson
import qualified Data.Aeson.Types as DAT


import NixFromNpm.Common
import NixFromNpm.Npm.PackageMap
import NixFromNpm.Git.Types hiding (Tag)
import Text.Parsec (ParseError)

data NpmVersionRange
  = SemVerRange SemVerRange
  | Tag Name
  | NpmUri URI
  | GitIdentifier GitIdentifier
  | LocalPath FilePath
  deriving (Eq, Ord)

data NpmVersionError
  = UnsupportedVersionType NpmVersionRange
  | UnsupportedUriScheme String
  | UnsupportedGitSource GitSource
  | VersionSyntaxError Text
  | UnrecognizedVersionFormat Text
  deriving (Show, Eq, Typeable)

instance Exception NpmVersionError

instance Show NpmVersionRange where
  show (SemVerRange rng) = show rng
  show (Tag name) = unpack name
  show (NpmUri uri) = uriToString uri
  show (GitIdentifier ident) = show ident
  show (LocalPath pth) = show pth

showPair :: PackageName -> SemVer -> Text
showPair name version = tshow name <> "@" <> tshow version

showPairs :: [(PackageName, SemVer)] -> Text
showPairs = mapJoinBy ", " (uncurry showPair)

showRangePair :: PackageName -> NpmVersionRange -> Text
showRangePair name range = tshow name <> "@" <> tshow range

showDeps :: [(PackageName, NpmVersionRange)] -> Text
showDeps ranges = mapJoinBy ", " (uncurry showRangePair) ranges

parseNpmVersionRange :: Text -> Maybe NpmVersionRange
parseNpmVersionRange t = do
  SemVerRange <$> eitherToMaybe (parseSemVerRange t)
  <|> GitIdentifier <$> parseGitId (unpack t)
  <|> NpmUri <$> parseURI (unpack t)
  <|> LocalPath <$> asPath
  <|> Tag <$> asTag
  where
    asPath = case unpack t of
      '/':_ -> Just $ fromText t
      '.':'/':_ -> Just $ fromText t
      '.':'.':'/':_ -> Just $ fromText t
      '~':'/':_ -> Just $ fromText t
      _ -> Nothing
    asTag = if t == "" || " " `isInfixOf` t then Nothing else Just t

instance FromJSON NpmVersionRange where
  parseJSON v = case v of
    String s -> case parseNpmVersionRange s of
      Nothing -> DAT.typeMismatch
        ("valid NPM version (got " <> show v <> ")") v
      Just range -> return range
    _ -> DAT.typeMismatch "string" v
