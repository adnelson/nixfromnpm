{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Npm.Version where

import qualified Data.Text as T

import Data.SemVer
import Data.Aeson
import qualified Data.Aeson.Types as Aeson


import NixFromNpm.Common
import NixFromNpm.Npm.PackageMap
import NixFromNpm.Git.Types hiding (Tag)
import Text.Parsec (ParseError)

data NpmVersionRange
  = SemVerRange SemVerRange
  -- ^ The most common: parsing from a semver range.
  | Tag Name
  -- ^ From an npm "tag"; this mapping lives in the package metadata.
  | NpmUri URI
  -- ^ From a URL (e.g. a tarball or zip file)
  | GitIdentifier GitIdentifier
  -- ^ From one of the "known" git services (e.g. github, bitbucket).
  | LocalPath FilePath
  -- ^ From a local file path.
  | InvalidVersion Text
  -- ^ An invalid version string: this results in a downstream failure
  -- but allows us to ignore an invalid version unless we actually
  -- need to resolve it. While proper Haskell idioms might dictate we
  -- use an Either or similar here, this is somewhat of a path of
  -- least resistance.
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
  show (InvalidVersion v) = "Version string unable to be parsed: " <> show v

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
      Nothing -> return $ InvalidVersion s
      Just range -> return range
    _ -> Aeson.typeMismatch "string" v
