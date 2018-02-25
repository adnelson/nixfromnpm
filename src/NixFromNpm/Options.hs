{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module NixFromNpm.Options where

import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as H

import Data.SemVer (anyVersion)
import Options.Applicative (Mod, OptionFields, Parser, value, switch)
import Options.Applicative (help, long, metavar, short, auto, option, strOption)

import NixFromNpm.Common
import NixFromNpm.Conversion.ToNix (nodePackagesDir)
import NixFromNpm.Npm.PackageMap (PackageName(..))
import NixFromNpm.Npm.Settings (getNpmTokens, parseNpmTokens)
import NixFromNpm.Npm.Version (NpmVersionError, NpmVersionRange)
import NixFromNpm.Npm.Version (parseNameAndRange)

-- | Errors about node libraries
data InvalidNodeLib
  = OutputNotWritable
  | OutputParentPathDoesn'tExist
  | OutputParentNotWritable
  | IsFileNotDirectory
  | NoPackageDir
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
  roGithubToken :: Maybe AuthToken,
  -- ^ Github authentication token.
  roNpmTokens :: [Text],
  -- ^ NPM authentication tokens.
  roNoDefaultRegistry :: Bool,
  -- ^ Disable fetching from npmjs.org.
  roNoRealTime :: Bool,
  -- ^ Don't write packages to disk as they are written.
  roTopNPackages :: Maybe Int,
  -- ^ Fetch the top `n` npm packages by popularity.
  roAllTop :: Bool,
  -- ^ If true, fetch all the top packages we have defined.
  roOverwriteNixLibs :: Bool
  -- ^ If true, allow existing nix libraries in output to be overridden.
} deriving (Show, Eq)

-- | Various options we have available for nixfromnpm. Validated
-- versions of what's parsed from the command-line.
data NixFromNpmOptions = NixFromNpmOptions {
  nfnoPkgNames :: [(PackageName, NpmVersionRange)],
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
  nfnoGithubToken :: Maybe AuthToken, -- ^ Github authentication token.
  nfnoNpmTokens :: Record AuthToken, -- ^ NPM authentication token.
  nfnoRealTime :: Bool, -- ^ Write packages to disk as they are written.
  nfnoOverwriteNixLibs :: Bool -- ^ Overwrite existing nix libraries
  } deriving (Show, Eq)

-- | Same as `strOption` but for Text.
textOption :: Mod OptionFields String -> Parser Text
textOption opts = pack <$> strOption opts

-- | Validates an extension folder. The folder must exist, and must contain
-- a default.nix and a node packages directory, and a .nixfromnpm-version file
-- which indicates the version of nixfromnpm used to build it.
validateExtension :: FilePath -> IO FilePath
validateExtension = absPath >=> \path -> do
  let assert' test err = assert test (InvalidNodeLib path err)
  whenM (not <$> isDirectoryEmpty path) $ do
    assert' (doesFileExist (path </> "default.nix")) NoDefaultNix
    assert' (doesDirectoryExist (path </> nodePackagesDir)) NoPackageDir
  return path

-- | Validate an output folder. An output folder EITHER must not exist, but
-- its parent directory does and is writable, OR it does exist, is writable,
-- and follows the extension format.
validateOutput :: FilePath -> IO FilePath
validateOutput = absPath >=> \path -> do
  -- Small wrapper function arround the assertion, taking a monadic
  -- test function and returning the given error if the test fails.
  let assert' test err = assert test (InvalidNodeLib path err)
  doesDirectoryExist path >>= \case
    -- If the directory exists, it must be writable and follow the
    -- extension format.
    True -> do assert' (isWritable path) OutputNotWritable
               validateExtension path
    False -> do
      -- If it doesn't exist, look at the parent path, which must
      -- exist and be writable so that we can create the output
      -- directory. Then return the path at the end.
      let parentPath = parent path
      assert' (doesDirectoryExist parentPath)
              OutputParentPathDoesn'tExist
      assert' (isWritable $ parentPath)
              OutputParentNotWritable
      return path

-- | Check that a path to a package.json file or its parent directory is valid.
validateJsPkg :: FilePath -> IO FilePath
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

-- | Get a list of the top n packages. If n is negative, or too large, we'll
-- return all of the packages we're aware of. If it's too large,
getTopN :: MonadIO io => Maybe Int -> io [(PackageName, NpmVersionRange)]
getTopN numPackages = do
  topPackages <- map T.strip <$> T.lines <$> readDataFile "top_packages.txt"
  mapM parseNameAndRange $ case numPackages of
    Nothing -> topPackages
    Just n -> take n topPackages

-- | Validates the raw options passed in from the command line, and also
-- translates them into their "full" counterpart, NixFromNpmOptions.
validateOptions :: RawOptions -> IO NixFromNpmOptions
validateOptions opts@(RawOptions{..}) = do
  pwd <- getCurrentDirectory
  topPackagesToFetch <- case roAllTop of
    True -> getTopN Nothing
    False -> case roTopNPackages of
      Nothing -> return []
      Just n -> getTopN (Just n)
  packageNames <- mapM parseNameAndRange roPkgNames
  extendPaths <- getExtensions roExtendPaths
  packagePaths <- mapM (validateJsPkg . fromText) roPkgPaths
  outputPath <- validateOutput . fromText . stripTrailingSlash $ roOutputPath
  registries <- mapM validateUrl $ (roRegistries <>
                                    if roNoDefaultRegistry
                                    then []
                                    else ["https://registry.npmjs.org"])
  githubTokenEnv <- map encodeUtf8 <$> getEnv "GITHUB_TOKEN"
  tokensCommandLine <- parseNpmTokens roNpmTokens
  npmTokensEnv <- getNpmTokens
  return $ NixFromNpmOptions {
    nfnoOutputPath = collapse outputPath,
    nfnoExtendPaths = extendPaths,
    nfnoGithubToken = roGithubToken <|> githubTokenEnv,
    nfnoNpmTokens = tokensCommandLine <> npmTokensEnv,
    nfnoCacheDepth = if roNoCache then -1 else roCacheDepth,
    nfnoDevDepth = roDevDepth,
    nfnoTest = roTest,
    nfnoTimeout = roTimeout,
    nfnoPkgNames = packageNames <> topPackagesToFetch,
    nfnoRegistries = registries,
    nfnoPkgPaths = packagePaths,
    nfnoNoDefaultNix = roNoDefaultNix,
    nfnoRealTime = not roNoRealTime,
    nfnoOverwriteNixLibs = roOverwriteNixLibs
    }
  where
    -- Remove a trailing slash, if it exists.
    stripTrailingSlash path = case T.stripSuffix "/" path of
      Nothing -> path
      Just path' -> path'
    validateUrl rawUrl = case parseURI (unpack rawUrl) of
      Nothing -> throw $ InvalidURI rawUrl
      Just uri -> return uri
    -- Parses the NAME=PATH extension directives.
    getExtensions :: [Text] -> IO (Record FilePath)
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


-- | Parses the raw command-line options into the intermediate form that is
-- used to construct a NixFromNpmOptions object.
parseOptions :: Parser RawOptions
parseOptions = RawOptions
    <$> packageNames
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
    <*> githubToken
    <*> npmTokens
    <*> noDefaultRegistry
    <*> noRealTime
    <*> topN
    <*> allTop
    <*> overwriteNixLibs
  where
    packageNames = many $ textOption $ short 'p'
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
                            <> value 0)
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
    tokenHelp s _type envVar = concat
      ["Token", s, " to use for ", _type, " access (also can be set with ",
       envVar, " environment variable)"]
    githubToken = (Just . T.encodeUtf8 <$> textOption (long "github-token"
                                  <> metavar "TOKEN"
                                  <> help (tokenHelp "" "github" "GITHUB_TOKEN")))
                  <|> pure Nothing
    npmTokens = many $ textOption $
      long "npm-token"
      <> metavar "NAMESPACE=TOKEN"
      <> help (tokenHelp "s" "npm" "NPM_AUTH_TOKENS")
    noDefaultRegistry = switch (long "no-default-registry"
                        <> help "Do not include default npmjs.org registry")
    noRealTime = switch (long "no-real-time"
                       <> help "Write packages to disk at the end rather than \
                               \ as they are generated.")
    allTop = switch (long "all-top"
                       <> help "Fetch all of the most popular packages that \
                               \nixfromnpm knows about.")
    topN = (Just . P.read . unpack
            <$> textOption (long "top-n"
                            <> metavar "N"
                            <> help "Fetch the top N packages by popularity."))
           <|> pure Nothing
    overwriteNixLibs = switch (long "overwrite-nix-libs"
                       <> help "Overwrite existing nix libraries in output.")
