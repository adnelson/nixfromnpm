{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module NixFromNpm.Git.Types where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Text.Regex.PCRE.Heavy (Regex, scan, re)

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

instance IsString GitRef where
  fromString = SomeRef . fromString

refText :: GitRef -> Text
refText (SomeRef r) = r
refText (CommitHash h) = h

-- | Various public git hosting services that nixfromnpm is aware of
data GitSource
  = Github
  | Bitbucket
  | Gist
  | GitLab
  deriving (Show, Eq, Ord, Enum)

-- | Represents a git repo at a particular commit.
data GitIdentifier = GitId {
  giSource::GitSource,
  giOwner::Name,
  giRepo::Name,
  giRef:: Maybe GitRef
  } deriving (Eq, Ord)

instance Show GitIdentifier where
  show (GitId source account repo ref) = do
    let ref' = case ref of
          Nothing -> ""
          Just r -> "#" <> refText r
        proto = toLower (show source) <> "://"
    proto <> unpack (account <> "/" <> repo <> ref')


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

getObject :: String -> Value -> Parser (HashMap Text Value)
getObject _ (Object o) = return o
getObject msg v =
  typeMismatch ("object (got " <> show v <> ", message " <> msg <> ")") v

-- | Convert a vector of tags to a hashmap mapping tag names to SHAs.
tagListToMap :: Vector Tag -> Record Text
tagListToMap tags = foldl' step mempty tags where
  step result tag = H.insert (tName tag) (cSha $ tCommit tag) result

-- | Read a git source from a URI server name.
sourceFromServer :: String -> Maybe GitSource
sourceFromServer regname
  | "github.com" `isInfixOf` regname = Just Github
  | "bitbucket.com" `isInfixOf` regname = Just Bitbucket
  | "gist" `isInfixOf` regname = Just Gist
  | "gitlab.com" `isInfixOf` regname = Just GitLab
  | otherwise = Nothing

-- | Get the repo owner and repo name from a URI path.
ownerRepoFromPath :: String -> Maybe (Name, Name)
ownerRepoFromPath path = case scan [re|^/(\w+)/(\w+)$|] $ pack path of
  [(_, [owner, repo])] -> Just (owner, repo)
  _ -> Nothing

-- | Parse a git ref from a URI fragment.
refFromFragment :: String -> Maybe GitRef
refFromFragment ('#':frag) = Just $ SomeRef $ pack frag
refFromFragment _ = Nothing

class IsGitId t where
  parseGitId :: t -> Maybe GitIdentifier

instance IsGitId GitIdentifier where
  parseGitId = Just

instance IsGitId URI where
  parseGitId uri = case uri of
    -- In order to determine which git service this is, we need to
    -- examine the uri authority (which contains the server info).
    -- If that's not there this isn't a git identifier.
    URI _ Nothing _ _ _ -> Nothing
    URI scheme (Just (URIAuth _ regname _)) path _ fragment -> do
      source <- sourceFromServer regname
      (owner, repo) <- ownerRepoFromPath path
      return $ GitId source owner repo (refFromFragment fragment)

instance IsGitId String where
  -- | Github URLs are a special case of git URLs; they can be specified
  -- simply by owner/repo or owner/repo#ref
  parseGitId str = case scan githubIdRegex str of
    [(_, [owner, repo])] -> do
      return $ GitId Github (pack owner) (pack repo) Nothing
    [(_, [owner, repo, '#':ref])] -> do
      return $ GitId Github (pack owner) (pack repo) $ Just $ fromString ref
    _ -> do
      parseGitId =<< parseURI str
    where
      githubIdRegex = [re|^([\w._-]+)/([\w._-]+)(\#[^\s]+)?$|]
