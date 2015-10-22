{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module NixFromNpm.Options (
  RawOptions(..), NixFromNpmOptions(..),
  parseOptions, validateOptions
  ) where
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as H

import Options.Applicative

import NixFromNpm.NpmVersion
import NixFromNpm.Parsers.NpmVersion
import NixFromNpm.SemVer
import NixFromNpm.Common hiding ((<>))

-- | Various options we have available for nixfromnpm, as parsed from the
-- command-line options.
data RawOptions = RawOptions {
  roPkgNames :: [Name],       -- ^ Names of packages to build.
  roPkgPaths :: [Text],       -- ^ Paths of package.jsons to build.
  roOutputPath :: Text,       -- ^ Path to output built expressions to.
  roNoDefaultNix :: Bool,     -- ^ Disable creation of default.nix file.
  roNoCache :: Bool,          -- ^ Build all expressions from scratch.
  roDevDepth :: Int,          -- ^ Dev dependency depth.
  roExtendPaths :: [Text],    -- ^ Extend existing expressions.
  roTest :: Bool,             -- ^ Fetch only; don't write expressions.
  roRegistries :: [Text],     -- ^ List of registries to query.
  roTimeout :: Int,           -- ^ Number of seconds after which to timeout.
  roGithubToken :: Maybe ByteString, -- ^ Github authentication token.
  roNoDefaultRegistry :: Bool -- ^ Disable fetching from npmjs.org.
} deriving (Show, Eq)

-- | Various options we have available for nixfromnpm. Validated
-- versions of what's parsed from the command-line.
data NixFromNpmOptions = NixFromNpmOptions {
  nfnoPkgNames :: [(Name, NpmVersionRange)],
  -- ^ Names/versions of packages to build.
  nfnoPkgPaths :: [FilePath],    -- ^ Path of package.json to build.
  nfnoOutputPath :: FilePath,    -- ^ Path to output built expressions to.
  nfnoNoDefaultNix :: Bool,      -- ^ Disable creation of default.nix file.
  nfnoNoCache :: Bool,           -- ^ Build all expressions from scratch.
  nfnoDevDepth :: Int,           -- ^ Dev dependency depth.
  nfnoExtendPaths :: Record FilePath, -- ^ Extend existing expressions.
  nfnoTest :: Bool,              -- ^ Fetch only; don't write expressions.
  nfnoRegistries :: [URI],      -- ^ List of registries to query.
  nfnoTimeout :: Int,            -- ^ Number of seconds after which to timeout.
  nfnoGithubToken :: Maybe ByteString -- ^ Github authentication token.
} deriving (Show, Eq)

textOption :: Mod OptionFields String -> Parser Text
textOption opts = pack <$> strOption opts

parseNameAndRange :: MonadIO m => Text -> m (Name, NpmVersionRange)
parseNameAndRange name = case T.split (== '@') name of
  [name] -> return (name, SemVerRange anyVersion)
  [name, range] -> case parseNpmVersionRange range of
    Left err -> errorC ["Invalid NPM version range ", range, ":\n", pshow err]
    Right nrange -> return (name, nrange)

validateOptions :: RawOptions -> IO NixFromNpmOptions
validateOptions opts = do
  pwd <- getCurrentDirectory
  let
    validatePath path = do
      let p' = pwd </> path
      doesFileExist p' >>= \case
        True -> errorC ["Path ", pathToText p', " is a file, not a directory"]
        False -> doesDirectoryExist p' >>= \case
          False -> errorC ["Path ", pathToText  p', " does not exist"]
          True -> return p'
  packageNames <- mapM parseNameAndRange $ roPkgNames opts
  extendPaths <- mapM validatePath =<< getExtensions (roExtendPaths opts)
  packagePaths <- mapM (validatePath . fromText) $ roPkgPaths opts
  outputPath <- validatePath . fromText $ roOutputPath opts
  registries <- mapM validateUrl $ (roRegistries opts <>
                                    if roNoDefaultRegistry opts
                                       then []
                                       else ["https://registry.npmjs.org"])
  tokenEnv <- map encodeUtf8 <$> getEnv "GITHUB_TOKEN"
  return (NixFromNpmOptions {
    nfnoOutputPath = outputPath,
    nfnoExtendPaths = extendPaths,
    nfnoGithubToken = roGithubToken opts <|> tokenEnv,
    nfnoNoCache = roNoCache opts,
    nfnoDevDepth = roDevDepth opts,
    nfnoTest = roTest opts,
    nfnoTimeout = roTimeout opts,
    nfnoPkgNames = packageNames,
    nfnoRegistries = registries,
    nfnoPkgPaths = packagePaths,
    nfnoNoDefaultNix = roNoDefaultNix opts
    })
  where
    validateUrl rawUrl = case parseURI (unpack rawUrl) of
      Nothing -> errorC ["Invalid url: ", rawUrl]
      Just uri -> return uri
    -- Parses the NAME=PATH extension directives.
    getExtensions :: [Text] -> IO (Record FilePath)
    getExtensions = go mempty where
      go extensions [] = return extensions
      go extensions (nameEqPath:paths) = case T.split (== '=') nameEqPath of
        [name, path] -> append name path
        [path] -> append (pathToText $ basename $ fromText path) path
        _ -> errorC ["Extensions must be of the form NAME=PATH (in argument ",
                     nameEqPath, ")"]
        where
          append name path = case H.lookup name extensions of
            Nothing -> go (H.insert name (fromText path) extensions) paths
            Just path' -> errorC ["Extension ", name, " is mapped to both ",
                                  "path ", path, " and path ",
                                  pathToText path']

parseOptions :: Maybe ByteString -> Parser RawOptions
parseOptions githubToken = RawOptions
    <$> many (textOption packageName)
    <*> packageFiles
    <*> textOption outputDir
    <*> noDefaultNix
    <*> noCache
    <*> devDepth
    <*> extendPaths
    <*> isTest
    <*> liftA2 snoc registries (pure "https://registry.npmjs.org")
    <*> timeout
    <*> token
    <*> noDefaultRegistry
  where
    packageName = short 'p'
                   <> long "package"
                   <> metavar "NAME"
                   <> help ("Package to generate expression for (supports "
                            <> "multiples)")
    packageFileHelp = "Path to package.json to generate expression for "
                      ++ " (NOT YET SUPPORTED)"
    packageFiles = many $ textOption (long "file"
                                      <> short 'f'
                                      <> metavar "FILE"
                                      <> help packageFileHelp)
    outputDir = short 'o'
                 <> long "output"
                 <> metavar "OUTPUT"
                 <> help "Directory to output expressions to"
    noDefaultNix = switch (long "no-default-nix"
                           <> help ("When building from a package.json, do not"
                                    <> " create a default.nix"))
    noCache = switch (long "no-cache"
                      <> help "Build all expressions in OUTPUT from scratch")
    devDepth = option auto (long "dev-depth"
                            <> metavar "DEPTH"
                            <> help "Depth to which to fetch dev dependencies"
                            <> value 1)
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
    noDefaultRegistry = switch (long "no-default-registry"
                        <> help "Do not include default npmjs.org registry")
