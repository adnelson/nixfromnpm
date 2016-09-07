{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tools for parsing NPM version range indicators.
module NixFromNpm.Npm.Version.Parser where

import qualified Prelude as P

import Data.Aeson
import qualified Data.Aeson.Types as DAT

import Data.SemVer (pSemVerRange, anyVersion, parseSemVerRange)
import NixFromNpm.Npm.Version
import NixFromNpm.Git.Types hiding (Tag)
import NixFromNpm.Common

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
