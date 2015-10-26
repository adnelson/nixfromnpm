{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.NpmVersion where

import qualified Data.Text as T

import NixFromNpm.Common
import NixFromNpm.SemVer
import NixFromNpm.GitTypes hiding (Tag)
import NixFromNpm.Parsers.Common (ParseError)

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
