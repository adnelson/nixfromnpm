{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Options where

import Options.Applicative

import NixFromNpm.Common hiding ((<>))

-- | Various options we have available for nixfromnpm. As of right now,
-- most of these are unimplemented.
data NixFromNpmOptions = NixFromNpmOptions {
  nfnoPkgName :: Name,
  nfnoOutputPath :: Text,
  nfnoNoCache :: Bool,
  nfnoExtendPaths :: [Text],
  nfnoTest :: Bool,
  nfnoRegistries :: [Text],
  nfnoTimeout :: Int,
  nfnoGithubToken :: Maybe Text
} deriving (Show, Eq)

textOption :: Mod OptionFields String -> Parser Text
textOption opts = pack <$> strOption opts

pOptions :: Maybe Text -> Parser NixFromNpmOptions
pOptions githubToken = NixFromNpmOptions
    <$> (textOption packageName <|> textOption packageFile)
    <*> textOption outputDir
    <*> noCache
    <*> extendPaths
    <*> isTest
    <*> (fmap (:) (pure "https://registry.npmjs.org") <*> registries)
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
    extendPaths = many (textOption (long "extend" <> metavar "PATH" <>
                                    help "Use expressions existing at PATH"))
    isTest = switch (long "test"
                     <> help "Don't write expressions; just test")
    timeout = option auto (long "timeout"
                           <> metavar "SECONDS"
                           <> help "Time requests out after SECONDS seconds"
                           <> value 10)
    registries = many $ textOption (long "registry"
                                  <> metavar "REGISTRY"
                                  <> help "NPM registry to query")
    tokenHelp = ("Token to use for github access (also can be set with " <>
                 "GITHUB_TOKEN environment variable)")
    token = (Just <$> textOption (long "github-token"
                                  <> metavar "TOKEN"
                                  <> help tokenHelp))
            <|> pure githubToken
