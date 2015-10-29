{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module NixFromNpm.Git.Types where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import NixFromNpm.Common

data Commit = Commit {
  cSha :: Text,
  cUrl :: Text
  } deriving (Show, Eq)

data Tag = Tag {
  tName :: Name,
  tCommit :: Commit
  } deriving (Show, Eq)

data Branch = Branch {
  bName :: Name,
  bCommit :: Commit
  } deriving (Show, Eq)

data Repo = Repo {
  rName :: Name,
  rDefaultBranch :: Name
  } deriving (Show, Eq)

-- | A git ref might be a commit hash, or something that could be anything.
data GitRef
  = SomeRef Text -- ^ Case where it's not yet known
  | BranchName Name
  | TagName Name
  | CommitHash Text
  deriving (Show, Eq, Ord)

refText :: GitRef -> Text
refText (SomeRef r) = r
refText (CommitHash h) = h

data GithubError
  = GithubUnreachable
  | InvalidJsonFromGithub Text
  | InvalidGitRef GitRef
  | InvalidGithubUri URI
  | NoSuchRepo Name Name
  deriving (Show, Eq, Typeable)

instance Exception GithubError

instance FromJSON Commit where
  parseJSON = getObject "commit info" >=> \o -> do
    sha <- o .: "sha"
    url <- o .: "url"
    return $ Commit sha url

instance FromJSON Tag where
  parseJSON = getObject "tag info" >=> \o -> do
    name <- o .: "name"
    commit <- o .: "commit"
    return $ Tag name commit

instance FromJSON Branch where
  parseJSON = getObject "branch info" >=> \o -> do
    name <- o .: "name"
    commit <- o .: "commit"
    return $ Branch name commit

instance FromJSON Repo where
  parseJSON = getObject "repo info" >=> \o -> do
    name <- o .: "name"
    defaultBranch <- o .: "default_branch"
    return $ Repo name defaultBranch

-- | Gets a hashmap from an object, or otherwise returns an empty hashmap.
getDict :: (FromJSON a) => Text -> Object -> Parser (HashMap Text a)
getDict key o = mapM parseJSON =<< (o .:? key .!= mempty)

getObject :: String -> Value -> Parser (HashMap Text Value)
getObject _ (Object o) = return o
getObject msg v =
  typeMismatch ("object (got " <> show v <> ", message " <> msg <> ")") v


-- | Convert a vector of tags to a hashmap mapping tag names to SHAs.
tagListToMap :: Vector Tag -> Record Text
tagListToMap tags = foldl' step mempty tags where
  step result tag = H.insert (tName tag) (cSha $ tCommit tag) result
