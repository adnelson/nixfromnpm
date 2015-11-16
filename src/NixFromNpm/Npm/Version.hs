{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Npm.Version where

import qualified Data.Text as T

import Data.SemVer

import NixFromNpm.Common
import NixFromNpm.Git.Types hiding (Tag)
import Text.Parsec (ParseError)

data GitSource = Github | Bitbucket | Gist | GitLab deriving (Show, Eq, Ord)

data NpmVersionRange
  = SemVerRange SemVerRange
  | Tag Name
  | NpmUri URI
  | GitId GitSource Name Name (Maybe GitRef)
  | LocalPath FilePath
  deriving (Eq, Ord)


data NpmVersionError
  = UnsupportedVersionType NpmVersionRange
  | UnsupportedUriScheme String
  | UnsupportedGitSource GitSource
  | VersionSyntaxError Text ParseError
  | UnrecognizedVersionFormat Text
  deriving (Show, Eq, Typeable)

instance Exception NpmVersionError

instance Show NpmVersionRange where
  show (SemVerRange rng) = show rng
  show (Tag name) = unpack name
  show (NpmUri uri) = uriToString uri
  show (GitId Github account repo Nothing) = show $ account <> "/" <> repo
  show (GitId Github account repo (Just ref)) = show $
    account <> "/" <> repo <> "#" <> refText ref
  show (GitId src _ _ _) = "git fetch from " <> show src
  show (LocalPath pth) = show pth

showPair :: Name -> SemVer -> Text
showPair name version = name <> "@" <> renderSV version

showPairs :: [(Name, SemVer)] -> Text
showPairs = mapJoinBy ", " (uncurry showPair)

showRangePair :: Name -> NpmVersionRange -> Text
showRangePair name range = name <> "@" <> pshow range

showDeps :: [(Name, NpmVersionRange)] -> Text
showDeps ranges = mapJoinBy ", " (uncurry showRangePair) ranges
