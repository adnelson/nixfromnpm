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
import NixFromNpm.ConvertToNix (nodePackagesDir)
import NixFromNpm.Common hiding ((<>))

-- | Errors about node libraries
data InvalidNodeLib
  = OutputNotWritable
  | OutputParentPathDoesn'tExist
  | OutputParentNotWritable
  | IsFileNotDirectory
  | NoPackageDir
  | NoVersionFile
  | NoDefaultNix
  deriving (Show, Eq, Typeable)

instance Exception InvalidNodeLib

data InvalidOption
  = NpmVersionError NpmVersionError
  | InvalidNodeLib FilePath InvalidNodeLib
  | InvalidExtensionSyntax Text
  | DuplicatedExtensionName Name FilePath FilePath
  | InvalidURI Text
  | NoPackageJsonFoundAt FilePath
  | NotPathToPackageJson FilePath
  deriving (Show, Eq, Typeable)

instance Exception InvalidOption

-- | Various options we have available for nixfromnpm, as parsed from the
-- command-line options.
data RawOptions = RawOptions {
  roPkgNames :: [Name],       -- ^ Names of packages to build.
  roPkgPaths :: [Text],       -- ^ Paths of package.jsons to build.
  roOutputPath :: Text,       -- ^ Path to output built expressions to.
  roNoDefaultNix :: Bool,     -- ^ Disable creation of default.nix file.
  roNoCache :: Bool,          -- ^ Build all expressions from scratch.
  roCacheDepth :: Int,        -- ^ Depth at which to use cache.
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
  nfnoCacheDepth :: Int,         -- ^ Dependency depth at which to use cache.
  nfnoDevDepth :: Int,           -- ^ Dev dependency depth.
  nfnoExtendPaths :: Record FilePath, -- ^ Extend existing expressions.
  nfnoTest :: Bool,              -- ^ Fetch only; don't write expressions.
  nfnoRegistries :: [URI],      -- ^ List of registries to query.
  nfnoTimeout :: Int,            -- ^ Number of seconds after which to timeout.
  nfnoGithubToken :: Maybe ByteString -- ^ Github authentication token.
  } deriving (Show, Eq)

type Validator = ReaderT FilePath IO

textOption :: Mod OptionFields String -> Parser Text
textOption opts = pack <$> strOption opts

-- | Validates an extension folder. The folder must exist, and must contain
-- a default.nix and a node packages directory, and a .nixfromnpm-version file
-- which indicates the version of nixfromnpm used to build it.
validateExtension :: FilePath -> Validator FilePath
validateExtension = absPath >=> \path -> do
  let assert' test err = assert test (InvalidNodeLib path err)
  assert' (doesFileExist (path </> "default.nix")) NoDefaultNix
  assert' (doesFileExist (path </> ".nixfromnpm-version")) NoVersionFile
  assert' (doesDirectoryExist (path </> nodePackagesDir)) NoPackageDir
  return path

absPath :: FilePath -> Validator FilePath
absPath path = (</> path) <$> ask

-- | Validate an output folder. An output folder EITHER must not exist, but
-- its parent directory does and is writable, OR it does exist, is writable,
-- and follows the extension format.
validateOutput :: FilePath -> Validator FilePath
validateOutput = absPath >=> \path -> do
  let assert' test err = assert test (InvalidNodeLib path err)
  doesDirectoryExist path >>= \case
    True -> do assert' (isWritable path) OutputNotWritable
               putStrLn "hey"
               validateExtension path
    False -> do
      let parentPath = parent path
      assert' (doesDirectoryExist parentPath)
              OutputParentPathDoesn'tExist
      assert' (isWritable $ parentPath)
              OutputParentNotWritable
      return path

validateJsPkg :: FilePath -> Validator FilePath
validateJsPkg = absPath >=> \path -> doesDirectoryExist path >>= \case
  -- If it is a directory, it must be writable and contain a package.json file.
  True -> do assert (isWritable path) OutputNotWritable
             assert (doesFileExist (path </> "package.json"))
                    (NoPackageJsonFoundAt (path </> "package.json"))
             return path
  -- If the path isn't a directory, it must be a "package.json" file, and
  -- must exist, and the folder must be writable.
  False -> do
    assert (return $ getFilename path == "package.json")
           (NotPathToPackageJson path)
    assert (isWritable (parent path)) OutputNotWritable
    assert (doesFileExist path) (NoPackageJsonFoundAt path)
    return (parent path)

parseNameAndRange :: MonadIO m => Text -> m (Name, NpmVersionRange)
parseNameAndRange name = case T.split (== '@') name of
  [name] -> return (name, SemVerRange anyVersion)
  [name, range] -> case parseNpmVersionRange range of
    Left err -> throw $ NpmVersionError (VersionSyntaxError range err)
    Right nrange -> return (name, nrange)

validateOptions :: RawOptions -> IO NixFromNpmOptions
validateOptions opts = do
  pwd <- getCurrentDirectory
  flip runReaderT pwd $ do
    packageNames <- mapM parseNameAndRange $ roPkgNames opts
    extendPaths <- getExtensions (roExtendPaths opts)
    packagePaths <- mapM (validateJsPkg . fromText) $ roPkgPaths opts
    outputPath <- validateOutput . fromText $ roOutputPath opts
    registries <- mapM validateUrl $ (roRegistries opts <>
                                      if roNoDefaultRegistry opts
                                      then []
                                      else ["https://registry.npmjs.org"])
    tokenEnv <- map encodeUtf8 <$> getEnv "GITHUB_TOKEN"
    return (NixFromNpmOptions {
      nfnoOutputPath = outputPath,
      nfnoExtendPaths = extendPaths,
      nfnoGithubToken = roGithubToken opts <|> tokenEnv,
      nfnoCacheDepth = if roNoCache opts then -1 else roCacheDepth opts,
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
      Nothing -> throw $ InvalidURI rawUrl
      Just uri -> return uri
    -- Parses the NAME=PATH extension directives.
    getExtensions :: [Text] -> Validator (Record FilePath)
    getExtensions = foldM step mempty where
      step extensions nameEqPath = case T.split (== '=') nameEqPath of
        [name, path] -> append name path
        [path] -> append (pathToText $ basename $ fromText path) path
        _ -> throw $ InvalidExtensionSyntax nameEqPath
        where
          append name path = case H.lookup name extensions of
            Nothing -> do validPath <- validateExtension $ fromText path
                          return $ H.insert name validPath extensions
            Just path' -> throw $ DuplicatedExtensionName name
                                    (fromText path) path'

parseOptions :: Maybe ByteString -> Parser RawOptions
parseOptions githubToken = RawOptions
    <$> many (textOption packageName)
    <*> packageFiles
    <*> textOption outputDir
    <*> noDefaultNix
    <*> noCache
    <*> cacheDepth
    <*> devDepth
    <*> extendPaths
    <*> isTest
    <*> registries
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
    cacheHelp = "Depth at which to use cache. Packages at dependency depth \
                \DEPTH and lower will be pulled from the cache. If DEPTH \
                \is negative, the cache will be ignored entirely (same as \
                \using --no-cache)"
    cacheDepth = option auto (long "cache-depth"
                              <> metavar "DEPTH"
                              <> help cacheHelp
                              <> value 0)
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
