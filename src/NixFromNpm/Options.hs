{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Options where
import qualified Data.Text.Encoding as T

import Options.Applicative

import NixFromNpm.Common hiding ((<>))

-- | Various options we have available for nixfromnpm. As of right now,
-- most of these are unimplemented.
data NixFromNpmOptions = NixFromNpmOptions {
  nfnoPkgNames :: [Name],       -- ^ Names of packages to build.
  nfnoPkgPath :: Maybe Text,    -- ^ Paths to package.jsons to build.
  nfnoOutputPath :: Text,       -- ^ Path to output built expressions to.
  nfnoNoDefaultNix :: Bool,     -- ^ Disable creation of default.nix file.
  nfnoNoCache :: Bool,          -- ^ Build all expressions from scratch.
  nfnoNoDev :: Bool,            -- ^ Don't fetch dev dependencies.
  nfnoExtendPaths :: [Text],    -- ^ Extend existing expressions.
  nfnoTest :: Bool,             -- ^ Fetch only; don't write expressions.
  nfnoRegistries :: [Text],     -- ^ List of registries to query.
  nfnoTimeout :: Int,           -- ^ Number of seconds after which to timeout.
  nfnoGithubToken :: Maybe ByteString -- ^ Github authentication token.
} deriving (Show, Eq)

textOption :: Mod OptionFields String -> Parser Text
textOption opts = pack <$> strOption opts

pOptions :: Maybe ByteString -> Parser NixFromNpmOptions
pOptions githubToken = NixFromNpmOptions
    <$> many (textOption packageName)
    <*> packageFile
    <*> textOption outputDir
    <*> noDefaultNix
    <*> noCache
    <*> noDev
    <*> extendPaths
    <*> isTest
    <*> liftA2 snoc registries (pure "https://registry.npmjs.org")
    <*> timeout
    <*> token
  where
    packageName = short 'p'
                   <> long "package"
                   <> metavar "NAME"
                   <> help ("Package to generate expression for (supports "
                            <> "multiples)")
    packageFileHelp = "Path to package.json to generate expression for "
                      ++ " (NOT YET SUPPORTED)"
    packageFile = (Just <$> textOption (long "file"
                                        <> short 'f'
                                        <> metavar "FILE"
                                        <> help packageFileHelp))
                  <|> pure Nothing
    outputDir = short 'o'
                 <> long "output"
                 <> metavar "OUTPUT"
                 <> help "Directory to output expressions to"
    noDefaultNix = switch (long "no-default-nix"
                           <> help ("When building from a package.json, do not"
                                    <> " create a default.nix"))
    noCache = switch (long "no-cache"
                      <> help "Build all expressions in OUTPUT from scratch")
    noDev = switch (long "no-dev"
                    <> help "Don't recur into dev dependencies (much faster)")
    extendHelp = ("Use expressions at PATH, optionally called NAME. (supports "
                  <> "multiples)")
    extendPaths = many (textOption (long "extend"
                                    <> short 'e'
                                    <> metavar "[NAME=]PATH"
                                    <> help extendHelp))
    isTest = switch (long "test"
                     <> help "Don't write expressions; just test")
    timeout = option auto (long "timeout"
                           <> metavar "SECONDS"
                           <> help "Time requests out after SECONDS seconds"
                           <> value 10)
    registries :: Parser [Text]
    registries = many $ textOption (long "registry"
                                    <> short 'r'
                                    <> metavar "REGISTRY"
                                    <> help ("NPM registry to query (supports "
                                             <> "multiples)"))
    tokenHelp = ("Token to use for github access (also can be set with " <>
                 "GITHUB_TOKEN environment variable)")
    token = (Just . T.encodeUtf8 <$> textOption (long "github-token"
                                  <> metavar "TOKEN"
                                  <> help tokenHelp))
            <|> pure githubToken
