{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Options where

import Options.Applicative

import NixFromNpm.Common hiding ((<>))

-- | Various options we have available for nixfromnpm. As of right now,
-- most of these are unimplemented.
data NixFromNpmOptions = NixFromNpmOptions {
  nfnoPkgNames :: [Name],       -- ^ Names of packages to build.
  nfnoPkgPaths :: [Text],       -- ^ Paths to package.jsons to build.
  nfnoOutputPath :: Text,       -- ^ Path to output built expressions to.
  nfnoNoCache :: Bool,          -- ^ Build all expressions from scratch.
  nfnoExtendPaths :: [Text],    -- ^ Extend existing expressions.
  nfnoTest :: Bool,             -- ^ Fetch only; don't write expressions.
  nfnoRegistries :: [Text],     -- ^ List of registries to query.
  nfnoTimeout :: Int,           -- ^ Number of seconds after which to timeout.
  nfnoGithubToken :: Maybe Text -- ^ Github authentication token.
} deriving (Show, Eq)

textOption :: Mod OptionFields String -> Parser Text
textOption opts = pack <$> strOption opts

pOptions :: Maybe Text -> Parser NixFromNpmOptions
pOptions githubToken = NixFromNpmOptions
    <$> many (textOption packageName)
    <*> many (textOption packageFile)
    <*> textOption outputDir
    <*> noCache
    <*> extendPaths
    <*> isTest
    <*> liftA2 snoc registries (pure "https://registry.npmjs.org")
    <*> timeout
    <*> token
  where
    packageName = (short 'p'
                   <> long "package"
                   <> metavar "PACKAGENAME"
                   <> help "Package to generate expression for")
    packageFile = (short 'f'
                   <> long "file"
                   <> metavar "PACKAGEFILE"
                   <> help "Path to package.json to generate expression for")
    outputDir = (short 'o'
                 <> long "output"
                 <> metavar "DIRECTORY"
                 <> help "Directory to output expressions to")
    noCache = switch (long "no-cache"
                      <> help "Build all expressions from scratch")
    extendHelp = "Use expressions at PATH called NAME"
    extendPaths = many (textOption (long "extend"
                                    <> short 'E'
                                    <> metavar "NAME=PATH"
                                    <> help extendHelp))
    isTest = switch (long "test"
                     <> help "Don't write expressions; just test")
    timeout = option auto (long "timeout"
                           <> metavar "SECONDS"
                           <> help "Time requests out after SECONDS seconds"
                           <> value 10)
    registries :: Parser [Text]
    registries = many $ textOption (long "registry"
                                    <> short 'R'
                                    <> metavar "REGISTRY"
                                    <> help "NPM registry to query")
    tokenHelp = ("Token to use for github access (also can be set with " <>
                 "GITHUB_TOKEN environment variable)")
    token = (Just <$> textOption (long "github-token"
                                  <> metavar "TOKEN"
                                  <> help tokenHelp))
            <|> pure githubToken
