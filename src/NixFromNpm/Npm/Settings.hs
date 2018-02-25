-- | Settings for NPM fetching
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Npm.Settings where

import Network.Curl (Long)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as H
import System.Environment (lookupEnv)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (handleToFd)


import NixFromNpm.Common

-- | Settings that affect the behavior of the NPM fetcher.
data NpmFetcherSettings = NpmFetcherSettings {
  nfsRegistries :: [URI],
  -- ^ List of URIs that we can use to query for NPM packages, in order of
  -- preference.
  nfsNpmAuthTokens :: Record AuthToken,
  -- ^ Used for authorization when fetching a package from a private npm.
  -- The keys are namespaces.
  nfsGithubAuthToken :: Maybe AuthToken,
  -- ^ Used for authorization when fetching a package from github.
  nfsRequestTimeout :: Long,
  -- ^ Request timeout.
  nfsRetries :: Int,
  -- ^ Number of times to retry HTTP requests.
  nfsExtendPaths :: Record FilePath,
  -- ^ Libraries we're extending.
  nfsOutputPath :: FilePath,
  -- ^ Path we're outputting generated expressions to.
  nfsMaxDevDepth :: Int,
  -- ^ Maximum dev dependency fetch depth.
  nfsCacheDepth :: Int,
  -- ^ Depth at which to start using the cache. If this is 0, then we will
  -- always use the cache if we can. If it's 1, then the top-level package
  -- will not be cached, but lower-level packages will. Et cetera.
  nfsRealTimeWrite :: Bool,
  -- ^ Whether to write packages in real-time as the expressions are generated,
  -- rather than waiting until the end.
  nfsOverwriteNixLibs :: Bool,
  -- ^ If true, allow existing nix libraries in output to be overridden.
  nfsVerbose :: Bool
  -- ^ Control verbosity
  }

-- | Default settings. The one caveat here is that there's no good way
-- to default the output path, so that will throw an error if it's not
-- set.
defaultSettings :: IO NpmFetcherSettings
defaultSettings = do
  pure NpmFetcherSettings {
    nfsRequestTimeout = 10,
    nfsNpmAuthTokens = mempty,
    nfsGithubAuthToken = Nothing,
    nfsRegistries = [fromJust $ parseURI "https://registry.npmjs.org"],
    nfsOutputPath = error "default setings provide no output path",
    nfsExtendPaths = mempty,
    nfsMaxDevDepth = 1,
    nfsCacheDepth = 0,
    nfsRetries = 1,
    nfsRealTimeWrite = False,
    nfsOverwriteNixLibs = False,
    nfsVerbose = False
    }

-- | Use the environment variables to provide better default settings.
settingsFromEnv :: IO NpmFetcherSettings
settingsFromEnv = do
  npmTokensEnv <- getNpmTokens
  githubTokenEnv <- map encodeUtf8 <$> getEnv "GITHUB_TOKEN"
  output <- getEnv "NIXFROMNPM_OUTPUT" >>= \case
    Nothing -> (</> "nixfromnpm_output") <$> getCurrentDirectory
    Just dir -> return $ fromText dir
  defaultSettings >>= \s -> pure s {
    nfsNpmAuthTokens = npmTokensEnv,
    nfsGithubAuthToken = githubTokenEnv,
    nfsOutputPath = output
    }

-- | Pull a ':'-separated list of tokens from the environment and parse
-- them. Return empty if the environment variable isn't set.
getNpmTokens :: MonadIO io => io (Record AuthToken)
getNpmTokens = getEnv "NPM_AUTH_TOKENS" >>= \case
  Nothing -> return mempty
  Just tokens -> parseNpmTokens (T.split (==':') tokens)

-- | The tokens should come in the form of NAMESPACE=TOKEN. This will
-- take all of the strings of that shape and stick them into a record.
parseNpmTokens :: MonadIO io => [Text] -> io (Record AuthToken)
parseNpmTokens = foldM step mempty where
  step tokenMap token = case T.split (=='=') token of
    [namespace, tokenText] -> do
      let tok :: AuthToken
          tok = encodeUtf8 tokenText
      return $ H.insert namespace (encodeUtf8 tokenText) tokenMap
    _ -> do
      warns ["Invalid NPM token: ", token]
      return tokenMap
