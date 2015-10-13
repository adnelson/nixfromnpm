{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module NixFromNpm.GitTypes where

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

bSha :: Branch -> Text
bSha = cSha . bCommit

tSha :: Tag -> Text
tSha = cSha . tCommit

data Repo = Repo {
  rName :: Name,
  rDefaultBranch :: Name
  } deriving (Show, Eq)

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
  step result tag = H.insert (tName tag) (tSha tag) result
