{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module NixFromNpm.NpmLookup where

--------------------------------------------------------------------------
import qualified Prelude as P
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Numeric (showHex)
import System.IO.Temp (openTempFile, createTempDirectory)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Text.Parsec as Parsec
import Shelly hiding (get, (</>), trace)
import Network.Curl
import Nix.Types
import Data.Digest.Pure.SHA (sha256, showDigest)

import NixFromNpm.Common
import NixFromNpm.NpmTypes
import qualified NixFromNpm.GitTypes as Git
import NixFromNpm.SemVer
import NixFromNpm.Parsers.Common hiding (Parser, Error, lines)
import NixFromNpm.ConvertToNix (nixExprHasDevDeps)
import NixFromNpm.Parsers.SemVer
import NixFromNpm.NpmVersion
import NixFromNpm.Parsers.NpmVersion
import NixFromNpm.PackageMap
--------------------------------------------------------------------------

-- | Things which can be converted into nix expressions: either they
-- are actual nix expressions themselves (which can be either
-- existing in the output, or existing in an extension), or they are
-- new packages which we have discovered.
data FullyDefinedPackage
  = NewPackage ResolvedPkg
  | FromExistingInOutput NExpr
  | FromExistingInExtension Name NExpr
  deriving (Show, Eq)

-- | The type of pre-existing packages, which can either come from the
-- output path, or come from an extension
data PreExistingPackage
  = FromOutput NExpr
  | FromExtension Name NExpr
  deriving (Show, Eq)

toFullyDefined :: PreExistingPackage -> FullyDefinedPackage
toFullyDefined (FromOutput expr) = FromExistingInOutput expr
toFullyDefined (FromExtension name expr) = FromExistingInExtension name expr

data NpmFetcherSettings = NpmFetcherSettings {
  nfsRegistries :: [URI],
  -- ^ List of URIs that we can use to query for NPM packages, in order of
  -- preference.
  nfsGithubAuthToken :: Maybe ByteString,
  -- ^ Used for authorization when fetching a package from github.
  nfsRequestTimeout :: Long,
  -- ^ Request timeout.
  nfsExtendPaths :: Record FilePath,
  -- ^ Libraries we're extending.
  nfsOutputPath :: FilePath,
  -- ^ Path we're outputting generated expressions to.
  nfsMaxDevDepth :: Int,
  -- ^ Maximum dev dependency fetch depth.
  nfsCacheDepth :: Int
  -- ^ Depth at which to start using the cache. If this is 0, then we will
  -- always use the cache if we can. If it's 1, then the top-level package
  -- will not be cached, but lower-level packages will. Et cetera.
  } deriving (Show, Eq)

-- | The state of the NPM fetcher.
data NpmFetcherState = NpmFetcherState {
  -- | Set of all of the packages that we have fully resolved.
  resolved :: PackageMap FullyDefinedPackage,
  -- | Set of all of the package info objects that we have fetched for
  -- a particular package.
  pkgInfos :: Record PackageInfo,
  -- | For cycle detection.
  currentlyResolving :: PackageMap (),
  -- | Stack of packages that we are resolving so we can get the path to the
  -- current package.
  packageStackTrace :: [(Name, SemVer)]
  -- | Depth to which we should fetch dev dependencies. If this value is
  -- non-zero, dev dependencies will be fetched with this value decremented.
  -- Otherwise, dev dependencies will not be fetched.
  -- devDependencyDepth :: Int
  }

-- | The monad for fetching from NPM.
type NpmFetcher = RWST NpmFetcherSettings () NpmFetcherState IO

---------------------- HTTP Fetching -----------------------


data HttpResult a
  = HttpSuccess a
  | HttpError HttpError
  deriving (Show, Eq, Typeable)

data HttpError
  = HttpErrorWithCode Int
  | HttpTimedOut Long
  | CurlError CurlCode
  deriving (Show, Eq, Typeable)

instance Exception HttpError

-- | Given a URL and some options, perform a curl request and return the
-- resulting code, HTTP status, and response body.
curlGetBS :: MonadIO m
          => URLString
          -> [CurlOption]
          -> m (CurlCode, Int, BL8.ByteString)
curlGetBS url opts = liftIO $ initialize >>= \ h -> do
  (finalBody, gatherBody) <- newIncoming
  setopt h (CurlFailOnError True)
  setDefaultSSLOpts h url
  setopt h (CurlURL url)
  setopt h (CurlWriteFunction (gatherOutput_ gatherBody))
  mapM (setopt h) opts
  rc <- perform h
  bs <- finalBody
  status <- getResponseCode h
  return (rc, status, bs)

-- | Convert (key, value) pairs into a curl Headers option.
makeHeaders :: [(Text, ByteString)] -> CurlOption
makeHeaders headers = CurlHttpHeaders $ map mk headers where
  mk (key, val) = T.unpack key <> ": " <> B.unpack val

-- | Wraps the curl function to fetch from HTTP, setting some headers and
-- telling it to follow redirects.
getHttp :: URI -- ^ URI to hit
        -> [(Text, ByteString)] -- ^ Additional headers to add
        -> NpmFetcher BL8.ByteString
getHttp uri headers = do
  let defHdrs = [("Connection", "Close"), ("User-Agent", "nixfromnpm-fetcher")]
      opts = [makeHeaders (headers <> defHdrs), CurlFollowLocation True]
  timeout <- asks nfsRequestTimeout
  let opts' = opts <> [CurlTimeout timeout]
  (code, status, content) <- curlGetBS (uriToString uri) opts'
  case code of
    CurlOK -> return content
    CurlHttpReturnedError -> throw $ HttpErrorWithCode status
    CurlOperationTimeout -> throw $ HttpTimedOut timeout
    err -> throw $ CurlError err

---------------------------------------------------------


addResolvedPkg :: Name -> SemVer -> ResolvedPkg -> NpmFetcher ()
addResolvedPkg name version _rpkg = do
  let rpkg = NewPackage _rpkg
  modify $ \s -> s {
    resolved = pmInsert name version rpkg (resolved s)
    }

-- | Get the hex sha256 of a bytestring.
hexSha256 :: BL8.ByteString -> Shasum
hexSha256 = SHA256 . pack . showDigest . sha256

-- | Queries NPM for package information.
_getPackageInfo :: Name -> URI -> NpmFetcher PackageInfo
_getPackageInfo pkgName registryUri = do
  putStrsLn ["Querying ", uriToText registryUri,
             " for package ", pkgName, "..."]
  jsonStr <- getHttp (registryUri // pkgName) []
             `catch` \case
                HttpErrorWithCode 404 -> throw (NoMatchingPackage pkgName)
  case eitherDecode jsonStr of
      Left err -> errorC ["couldn't parse JSON from NPM: ", pack err]
      Right info -> return info

-- | Same as _getPackageInfo, but caches results for speed.
getPackageInfo :: Name -> NpmFetcher PackageInfo
getPackageInfo name = do
  infos <- gets pkgInfos
  case H.lookup name infos of
    Just info -> do
      return info
    Nothing -> do
      let tryToFetch [] = throw (NoMatchingPackage name)
          tryToFetch (registry:registries) = do
            putStrsLn ["Trying to fetch from ", uriToText registry]
            _getPackageInfo name registry
            `catch` \case
              NoMatchingPackage _ -> tryToFetch registries
              NoMatchingVersion _ -> tryToFetch registries
              err -> throw err
      info <- tryToFetch =<< asks nfsRegistries
      storePackageInfo name info
      return info

storePackageInfo :: Name -> PackageInfo -> NpmFetcher ()
storePackageInfo name info = do
  infos <- gets pkgInfos
  let existingInfo = H.lookupDefault mempty name infos
      newInfo = existingInfo <> info
  modify $ \s -> s {pkgInfos = H.insert name newInfo (pkgInfos s)}

-- | Performs a shell command and reports if it errors; otherwise returns
--   the stdout from the command.
shell :: Sh Text -> NpmFetcher Text
shell action = do
  (code, out, err) <- shelly $ errExit False $ do
    out <- action
    code <- lastExitCode
    err <- lastStderr
    return (code, out, err)
  case code of
    0 -> return out
    n -> do
      errorC $ catMaybes [
                      Just "Shell command returned an error.",
                      maybeIf (out /= "") $ "\nstdout:\n" <> out,
                      Just $ "\nstderr:\n" <> err]

silentShell :: Sh Text -> NpmFetcher Text
silentShell = shell . silently

getTemp :: MonadIO m => m FilePath
getTemp = getEnv "TMPDIR" >>= \case
  Nothing -> return "/tmp"
  Just t -> return (fromText t)

tempDir :: MonadIO m => String -> m FilePath
tempDir template = do
  tmpdir <- getTemp
  liftIO $ decodeString <$> createTempDirectory (encodeString tmpdir) template

tempFile :: MonadIO m => String -> m (FilePath, Handle)
tempFile template = do
  tmpdir <- getTemp
  (path, handle) <- liftIO $ openTempFile (encodeString tmpdir) template
  return (decodeString path, handle)

-- | Returns the SHA256 hash of the result of fetching the URI, and the path
-- in which the tarball is stored.
prefetchSha256 :: URI -> NpmFetcher (Shasum, FilePath)
prefetchSha256 uri = do
  putStrsLn ["Pre-fetching url ", uriToText uri]
  -- Make a temporary directory.
  dir <- tempDir "tarball-prefetch"
  let outPath = dir </> "outfile"
  putStrsLn ["putting in location ", pathToText outPath]
  -- Download the file into memory, calculate the hash.
  tarball <- getHttp uri []
  let hash = hexSha256 tarball
  -- Return the hash and the path.
  path <- liftIO $ do
    (path, handle) <- tempFile "nixfromnpmfetch.tgz"
    BL8.hPut handle tarball
    hClose handle
    putStrsLn ["Wrote tarball to ", pathToText path]
    return path
  return (hash, path)

-- | Shell out to tar to pull the package.json out of a tarball.
extractVersionInfo :: FilePath -- ^ Path to the downloaded tarball.
                   -> FilePath -- ^ Subpath containing package.json file.
                   -> NpmFetcher VersionInfo -- ^ The version info object.
extractVersionInfo tarballPath subpath = do
  temp <- tempDir "pkg-json-extract"
  putStrsLn ["Extracting ", pathToText tarballPath, " to tempdir ",
             pathToText temp]
  shelly $ run_ "tar" ["-xf", pathToText tarballPath, "-C",
                       pathToText temp]
  result <- extractPkgJson $ temp </> subpath </> "package.json"
  removeDirectoryRecursive temp
  return result

-- | Extract the version info out of a package.json file.
extractPkgJson :: FilePath -> NpmFetcher VersionInfo
extractPkgJson path = do
  putStrsLn ["Reading information from ", pathToText path]
  pkJson <- liftIO $ B.readFile (encodeString path)
  case eitherDecode $ BL8.fromStrict pkJson of
    Left err -> errorC ["Invalid package.json file: ", pack err,
                             "\npackage.json contents:\n", decodeUtf8 pkJson]
    Right info -> return info

-- | Fetch a package over HTTP. Return the version of the fetched package,
-- and store the hash.
fetchHttp :: FilePath -- ^ Subpath in which to find the package.json.
          -> URI -- ^ The URI to fetch.
          -> NpmFetcher SemVer -- ^ The package at that URI.
fetchHttp subpath uri = do
  -- Use nix-fetch to download and hash the tarball.
  (hash, tarballPath) <- prefetchSha256 uri
  -- Extract the tarball to a temp directory and parse the package.json.
  versionInfo <- extractVersionInfo tarballPath subpath
  removeFile tarballPath
  -- Create the DistInfo.
  let dist = DistInfo {diUrl = uriToText uri,
                       diShasum = hash}
  -- Add the dist information to the version info and resolve it.
  resolveVersionInfo $ versionInfo {viDist = Just dist}

-- | Send a curl to github with some extra headers set.
githubCurl :: FromJSON a => URI -> NpmFetcher a
githubCurl uri = do
  -- Add in the github auth token if it is provided.
  extraHeaders <- asks nfsGithubAuthToken >>= \case
    Nothing -> return []
    Just token -> return [("Authorization", "token " <> token)]
  let headers = extraHeaders <>
              [("Accept", "application/vnd.github.quicksilver-preview+json")]
  putStrsLn ["GET ", uriToText uri]
  jsonStr <- getHttp uri headers
  case eitherDecode jsonStr of
    Left err -> throw $ Git.InvalidJsonFromGithub (pshow err)
    Right info -> return info

makeGithubURI :: Name -> Name -> URI
makeGithubURI owner repo = do
  let baseUri = unsafeParseURI "https://api.github.com/repos/"
  baseUri // owner // repo

-- | Queries NPM for package information.
getDefaultBranch :: Name -> Name -> NpmFetcher Name
getDefaultBranch owner repo = do
  putStrs ["Querying github for default branch of ", repo, "..."]
  Git.rDefaultBranch <$> githubCurl (makeGithubURI owner repo)

gitRefToSha :: Name -- ^ Repo owner
            -> Name -- ^ Repo name
            -> Name -- ^ Commit-ish, i.e. branch, hash or tag
            -> NpmFetcher Text -- ^ The hash of the branch
gitRefToSha owner repo ref = do
  let rpath = "/" <> owner <> "/" <> repo
  let uri = makeGithubURI owner repo
      fromBranch = do
        let bSha = Git.cSha . Git.bCommit
        bSha <$> githubCurl (uri // "branches" // ref)
      fromTag = do
        tagMap <- Git.tagListToMap <$> githubCurl (uri // "tags")
        case H.lookup ref tagMap of
          Just sha -> return sha
          Nothing -> throw (Git.InvalidGitRef ref)
      fromCommit = do
        map Git.cSha $ githubCurl (uri // "commits" // ref)
      catch404 action1 action2 = action1 `catch` \case
        HttpErrorWithCode 404 -> action2
        err -> throw err
  fromBranch `catch404` fromTag `catch404` fromCommit `catch404`
        throw (Git.InvalidGitRef ref)

-- | Given a github repo and a branch, gets the SHA of the head of that
-- branch
getShaOfBranch :: Name -- ^ Repo owner
               -> Name -- ^ Repo name
               -> Name -- ^ Name of the branch to get
               -> NpmFetcher Text -- ^ The hash of the branch
getShaOfBranch owner repo branchName = do
  let rpath = "/" <> owner <> "/" <> repo
  let uri = makeGithubURI owner repo // "branches" // branchName
  putStrs ["Querying github for sha of ", rpath, "/", branchName, "..."]
  Git.cSha . Git.bCommit <$> githubCurl uri

-- | Fetch a package from github. Will convert a branch name into a specific
-- SHA so that the generated URL is deterministic.
fetchGithub :: URI -> NpmFetcher SemVer
fetchGithub uri = do
  (owner, repo) <- case split "/" $ uriPath uri of
    [_, owner, repo] -> return (pack owner, pack $ dropSuffix ".git" repo)
    _ -> errorC ["Invalid repo path: ", pack $ uriPath uri]
  hash <- case uriFragment uri of
    -- if there isn't a ref or a tag, use the default branch.
    "" -> gitRefToSha owner repo =<< getDefaultBranch owner repo
    -- otherwise, use that as a tag.
    '#':frag -> return $ pack frag
    frag -> errorC ["Invalid URL fragment '", pack frag, "'"]
  -- Use the hash to pull down a zip.
  let uri = makeGithubTarballURI owner repo hash
  fetchHttp (fromText (repo <> "-" <> hash)) uri

makeGithubTarballURI :: Name -- ^ Owner name
              -> Name -- ^ Repo name
              -> Text -- ^ Commit hash
              -> URI -- ^ Fully-formed URI
makeGithubTarballURI owner repo commit = URI {
  uriScheme = "https:",
  uriAuthority = Just (URIAuth "" "github.com" ""),
  uriPath = '/' : unpack (
            joinBy "/" [owner, repo, "archive", commit <> ".tar.gz"]),
  uriQuery = "",
  uriFragment = ""
  }

-- | Convert an HTTP URI to a git URI, if it can be.
httpToGitUri :: URI -> NpmFetcher (URI, Text)
httpToGitUri uri = do
  case uriAuthority uri of
    Nothing -> return (uri, "package")
    Just auth -> case uriRegName auth of
      "github.com" -> case T.split (=='/') $ pack (uriPath uri) of
        ["", owner, repo, "tarball", ref] -> do
          sha <- gitRefToSha owner repo ref
          return (makeGithubTarballURI owner repo sha, repo <> "-" <> sha)
        ["", owner, repo] -> do
          branch <- getDefaultBranch owner repo
          sha <- gitRefToSha owner repo branch
          return (makeGithubTarballURI owner repo sha, repo <> "-" <> sha)
      _ -> return (uri, "package")

-- | Given an arbitrary NPM version range (e.g. a semver range, a URL, etc),
-- figure out how to resolve the dependency.
resolveNpmVersionRange :: Name -- ^ Name of the package.
                       -> NpmVersionRange -- ^ Version bounds of the package.
                       -> NpmFetcher SemVer -- ^ Dependency.
resolveNpmVersionRange name range = case range of
  SemVerRange svr -> resolveDep name svr
  NpmUri uri -> case uriScheme uri of
    "git:" -> fetchGithub uri
    "git+https:" -> fetchGithub uri
    s | s == "http:" || s == "https:" -> do
      (uri', subpath) <- httpToGitUri uri
      fetchHttp (fromText subpath) uri'
    scheme -> errorC ["Unknown uri scheme ", pack scheme]
  GitId src owner repo rev -> case src of
    Github -> do
      let frag = maybe "" ("#" <>) rev
          uri = concat ["https://github.com/", owner, "/", repo, frag]
      fetchGithub $ fromJust $ parseURI $ unpack uri
    _ -> errorC ["Can't handle git source ", pack $ show src]
  Tag tag -> resolveByTag tag name
  vr -> errorC ["Don't know how to resolve dependency '",
                     pack $ show vr, "'"]

-- | Uses the set of downloaded packages as a cache to avoid unnecessary
-- duplication.
resolveDep :: Name -> SemVerRange -> NpmFetcher SemVer
resolveDep name range = H.lookup name <$> gets resolved >>= \case
  -- We've already defined some versions of this package.
  Just versions -> case filter (matches range) (H.keys versions) of
    [] -> _resolveDep name range -- No matching versions, need to fetch.
    vs -> do
      let bestVersion = maximum vs
          versionDots = renderSV bestVersion
          package = fromJust $ H.lookup bestVersion versions
      putStrs ["Requirement ", name, " version ", pack $ show range,
                 " already satisfied: "]
      putStrsLn $ case package of
        NewPackage _ -> ["fetched package version ", versionDots]
        FromExistingInOutput _ -> ["already had version ", versionDots,
                                   " in output directory (use --no-cache",
                                   " to override)"]
        FromExistingInExtension name _ -> ["version ", versionDots,
                                           " provided by extension ", name]
      return bestVersion
  -- We haven't yet found any versions of this package.
  Nothing -> _resolveDep name range

-- | Start resolving a package, and add it to the stack.
startResolving :: Name -> SemVer -> NpmFetcher ()
startResolving name ver = do
  showTrace
  modify $ \s -> do
    s {currentlyResolving = pmInsert name ver () $ currentlyResolving s,
       packageStackTrace = (name, ver) : packageStackTrace s}

-- | Mark a package as being finished, and pop it off the stack.
finishResolving :: Name -> SemVer -> NpmFetcher ()
finishResolving name ver = do
  modify $ \s ->
    s {currentlyResolving = pmDelete name ver $ currentlyResolving s,
       packageStackTrace = P.tail $ packageStackTrace s}
  putStrsLn ["Finished resolving ", name, " ", renderSV ver]

-- | Print the current package stack trace.
showTrace :: NpmFetcher ()
showTrace = do
  trace <- gets packageStackTrace
  putStrLn $ mapJoinBy " -> " (uncurry showPair) (reverse trace)

-- | Return whether a particular version of a package is being resolved.
isBeingResolved :: Name -> SemVer -> NpmFetcher Bool
isBeingResolved name version =
  pmMember name version <$> gets currentlyResolving

-- | Recur the fetch on a list of dependencies, at a decremented
-- development dependency depth.
recurOn :: Name -- ^ Name of the package whose dependencies these are.
        -> SemVer -- ^ Version of the package whose dependencies these are.
        -> DependencyType -- ^ Type of the dependency being fetched.
        -> Record NpmVersionRange -- ^ The dependency map.
        -> NpmFetcher (Record ResolvedDependency) -- ^ The result.
recurOn name version deptype deps =
  map H.fromList $ do
    let depList = H.toList deps
        (desc, descPlural) = case deptype of
          Dependency -> ("dependency", "dependencies")
          DevDependency -> ("development dependency",
                            "development dependencies")
    when (length depList > 0) $ do
      putStrsLn [name, " version ", renderSV version, " has ",
                 descPlural, ": ", showDeps depList]
    forM depList $ \(depName, depRange) -> do
      putStrsLn ["Resolving ", showRangePair depName depRange, ", ", desc,
                 " of ", showPair name version]
      result <- (Resolved <$> resolveNpmVersionRange depName depRange)
                `catches` [
                  Handler (\reason -> return $ Broken reason),
                  Handler (\(e::Git.GithubError) -> return $ Broken $ Reason $ show e)
                  ]
      return (depName, result)

currentDepth :: NpmFetcher Int
currentDepth = length <$> gets packageStackTrace

-- | Tells us whether we should fetch development dependencies.
shouldFetchDevs :: NpmFetcher Bool
shouldFetchDevs = (<) <$> currentDepth <*> asks nfsMaxDevDepth

-- | A VersionInfo is an abstraction of an NPM package. This will resolve
-- the version info into an actual package (recurring on the dependencies)
-- and add it to the resolved package map.
resolveVersionInfo :: VersionInfo -> NpmFetcher SemVer
resolveVersionInfo VersionInfo{..} = do
  let recurOn' = recurOn viName viVersion
  isBeingResolved viName viVersion >>= \case
    True -> do -- This is a cycle: allowed for dev dependencies, but we
               -- don't want to loop infinitely so we just return.
               putStrLn "uh oh"
               return viVersion
    False -> do
      startResolving viName viVersion
      deps <- recurOn' Dependency viDependencies
      devDeps <- do
        shouldFetch <- shouldFetchDevs
        case shouldFetch || H.null viDevDependencies of
          True -> Just <$> recurOn' DevDependency viDevDependencies
          False -> return Nothing
      finishResolving viName viVersion
      -- Store this version's info.
      addResolvedPkg viName viVersion $ ResolvedPkg {
          rpName = viName,
          rpVersion = viVersion,
          rpDistInfo = viDist,
          rpMeta = viMeta,
          rpDependencies = deps,
          rpDevDependencies = devDeps
        }
      return viVersion

-- | Resolves a dependency given a name and version range.
_resolveDep :: Name -> SemVerRange -> NpmFetcher SemVer
_resolveDep name range = do
  putStrsLn ["Resolving ", name, " (", pshow range, ")"]
  pInfo <- getPackageInfo name
  current <- gets currentlyResolving
  -- Filter out packages currently being evaluated.
  let notCurrent = case H.lookup name current of
        Nothing -> piVersions pInfo
        Just cur -> H.difference (piVersions pInfo) cur
  -- Choose the entry with the highest version that matches the range.
  case filter (matches range) $ H.keys notCurrent of
    [] -> throw (NoMatchingVersion $ SemVerRange range)
    matches -> do
      let versionInfo = notCurrent H.! maximum matches
      resolveVersionInfo versionInfo

-- | Resolve a dependency by tag name (e.g. a release tag).
resolveByTag :: Name -- ^ Tag name.
             -> Name -- ^ Name of the package.
             -> NpmFetcher SemVer -- ^ A resolved dependency.
resolveByTag tag pkgName = do
  pInfo <- getPackageInfo pkgName
  case H.lookup tag $ piTags pInfo of
    Nothing -> throw (NoSuchTag tag)
    Just version -> case H.lookup version $ piVersions pInfo of
      Nothing -> errorC ["Tag ", tag, " points to version ",
                         renderSV version, ", but no such version of ",
                         pkgName, " exists."]
      Just versionInfo -> resolveVersionInfo versionInfo

defaultSettings :: NpmFetcherSettings
defaultSettings = NpmFetcherSettings {
  nfsRequestTimeout = 10,
  nfsGithubAuthToken = Nothing,
  nfsRegistries = [fromJust $ parseURI "https://registry.npmjs.org"],
  nfsOutputPath = error "default setings provide no output path",
  nfsExtendPaths = mempty,
  nfsMaxDevDepth = 1,
  nfsCacheDepth = 0
  }

startState :: NpmFetcherState
startState = do
  NpmFetcherState {
    resolved = mempty,
    packageStackTrace = [],
    pkgInfos = H.empty,
    currentlyResolving = mempty
    }

runNpmFetchWith :: NpmFetcherSettings
                -> NpmFetcherState
                -> NpmFetcher a
                -> IO (a, NpmFetcherState)
runNpmFetchWith settings state action = do
  (result, newState, _) <- runRWST action settings state
  return (result, newState)

runIt :: NpmFetcher a -> IO a
runIt action = do
  fst <$> runNpmFetchWith defaultSettings startState action
