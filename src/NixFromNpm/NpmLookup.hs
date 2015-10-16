{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
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
import Shelly hiding (get, (</>))
import Network.Curl
import Nix.Types
import Data.Digest.Pure.SHA (sha256, showDigest)

import NixFromNpm.Common
import NixFromNpm.NpmTypes
import qualified NixFromNpm.GitTypes as Git
import NixFromNpm.SemVer
import NixFromNpm.Parsers.Common hiding (Parser, Error, lines)
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

-- | The state of the NPM fetcher.
data NpmFetcherState = NpmFetcherState {
  -- | List of URIs that we can use to query for NPM packages, in order of
  -- preference.
  registries :: [URI],
  -- | Used for authorization when fetching a package from github.
  githubAuthToken :: Maybe ByteString,
  -- | Set of all of the packages that we have fully resolved.
  resolved :: PackageMap FullyDefinedPackage,
  -- | Set of all of the package info objects that we have fetched for
  -- a particular package.
  pkgInfos :: Record PackageInfo,
  -- | For cycle detection.
  currentlyResolving :: PackageMap (),
  -- | Stack of packages that we are resolving so we can get the path to the
  -- current package.
  packageStackTrace :: [(Name, SemVer)],
  -- | Set of packages known to be problematic; it is an error if one of these
  -- packages appears in a dependency tree.
  knownProblematicPackages :: HashSet Name,
  -- | Depth to which we should fetch dev dependencies. If this value is
  -- non-zero, dev dependencies will be fetched with this value decremented.
  -- Otherwise, dev dependencies will not be fetched.
  devDependencyDepth :: Int,
  -- | Request timeout.
  requestTimeout :: Long
  }

-- | The monad for fetching from NPM.
type NpmFetcher = ExceptT EList (StateT NpmFetcherState IO)


---------------------- HTTP Fetching -----------------------


data HttpResult a
  = HttpSuccess a
  | HttpError Int
  | HttpTimedOut Long
  deriving (Show, Eq)

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
getHttp :: Text -- URI to hit
        -> [(Text, ByteString)] -- Additional headers to add
        -> NpmFetcher (HttpResult BL8.ByteString)
getHttp uri headers = do
  let defHdrs = [("Connection", "Close"), ("User-Agent", "nixfromnpm-fetcher")]
      opts = [makeHeaders (headers <> defHdrs), CurlFollowLocation True]
  timeout <- gets requestTimeout
  let opts' = opts <> [CurlTimeout timeout]
  (code, status, content) <- curlGetBS (unpack uri) opts'
  case code of
    CurlOK -> return $ HttpSuccess content
    CurlHttpReturnedError -> return $ HttpError status
    CurlOperationTimeout -> return $ HttpTimedOut timeout
    err -> errorC ["Curl threw an unknown error ", pack $ show err]

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
  let uri = uriToText $ registryUri `slash` pkgName
  putStrsLn ["Querying ", uriToText registryUri,
             " for package ", pkgName, "..."]
  getHttp uri [] >>= \case
    HttpSuccess jsonStr -> case eitherDecode jsonStr of
      Left err -> throwErrorC ["couldn't parse JSON from NPM: ", pack err]
      Right info -> return info
    err -> throwError1 $ pshow err

-- | Same as _getPackageInfo, but caches results for speed.
getPackageInfo :: Name -> NpmFetcher PackageInfo
getPackageInfo name = lookup name . pkgInfos <$> get >>= \case
  Just info -> return info
  Nothing -> inContext ctx $ do
    regs <- gets registries
    info <- firstSuccess "No repos contained package" $
              map (_getPackageInfo name) regs
    storePackageInfo name info
    return info
  where ctx = pack $ "When querying NPM registry for package " <> show name

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
      throwErrorC $ catMaybes [
                      Just "Shell command returned an error.",
                      maybeIf (out /= "") $ "\nstdout:\n" <> out,
                      Just $ "\nstderr:\n" <> err]

silentShell :: Sh Text -> NpmFetcher Text
silentShell = shell . silently

getTemp :: MonadIO m => m P.FilePath
getTemp = getEnv "TMPDIR" >>= \case
  Nothing -> return "/tmp"
  Just t -> return (unpack t)

tempDir :: MonadIO m => String -> m P.FilePath
tempDir template = do
  tmpdir <- getTemp
  liftIO $ createTempDirectory tmpdir template

tempFile :: MonadIO m => String -> m (P.FilePath, Handle)
tempFile template = do
  tmpdir <- getTemp
  liftIO $ openTempFile tmpdir template

-- | Returns the SHA256 hash of the result of fetching the URI, and the path
-- in which the tarball is stored.
prefetchSha256 :: URI -> NpmFetcher (Shasum, P.FilePath)
prefetchSha256 uri = do
  putStrsLn ["Pre-fetching url ", uriToText uri]
  -- Make a temporary directory.
  dir <- tempDir "tarball-prefetch"
  let outPath = dir </> "outfile"
  putStrsLn ["putting in location ", pack outPath]
  -- Download the file into memory, calculate the hash.
  getHttp (uriToText uri) [] >>= \case
    HttpSuccess tarball -> do
      let hash = hexSha256 tarball
      -- Return the hash and the path.
      path <- liftIO $ do
        (path, handle) <- tempFile "nixfromnpmfetch.tgz"
        BL8.hPut handle tarball
        hClose handle
        putStrsLn ["Wrote tarball to ", pack path]
        return path
      return (hash, path)
    err -> do
      putStrsLn ["Could not fetch tarball at url ", uriToText uri,
                 ": ", pshow err]
      throwErrorC ["Could not fetch tarball at url ", uriToText uri]

-- | Shell out to tar to pull the package.json out of a tarball.
extractVersionInfo :: P.FilePath -- ^ Path to the downloaded tarball.
                   -> Text       -- ^ Subpath containing package.json file.
                   -> NpmFetcher VersionInfo -- ^ The version info object.
extractVersionInfo tarballPath subpath = do
  temp <- tempDir "pkg-json-extract"
  putStrsLn ["Extracting ", pack tarballPath, " to tempdir ", pack temp]
  shelly $ run_ "tar" ["-xf", pack tarballPath, "-C", pack temp]
  result <- extractPkgJson $ temp </> unpack subpath </> "package.json"
  liftIO $ removeDirectoryRecursive temp
  return result

-- | Extract the version info out of a package.json file.
extractPkgJson :: P.FilePath -> NpmFetcher VersionInfo
extractPkgJson path = do
  putStrsLn ["Reading information from ", pack path]
  pkJson <- liftIO $ B.readFile path
  case eitherDecode $ BL8.fromStrict pkJson of
    Left err -> throwErrorC ["Invalid package.json file: ", pack err,
                             "\npackage.json contents:\n", decodeUtf8 pkJson]
    Right info -> return info

-- | Fetch a package over HTTP. Return the version of the fetched package,
-- and store the hash.
fetchHttp :: Text -- ^ Subpath in which to find the package.json.
          -> URI -- ^ The URI to fetch.
          -> NpmFetcher ResolvedDependency -- ^ The package at that URI.
fetchHttp subpath uri = do
  -- Use nix-fetch to download and hash the tarball.
  (hash, tarballPath) <- prefetchSha256 uri
  -- Extract the tarball to a temp directory and parse the package.json.
  versionInfo <- extractVersionInfo tarballPath subpath
  liftIO $ removeFile tarballPath
  -- Create the DistInfo.
  let dist = DistInfo {diUrl = uriToText uri,
                       diShasum = hash,
                       diSubPath = maybeIf (subpath /= "package") subpath}
  -- Add the dist information to the version info and resolve it.
  resolveVersionInfo $ versionInfo {viDist = Just dist}

-- | Send a curl to github with some extra headers set.
githubCurl :: FromJSON a => Text -> NpmFetcher a
githubCurl uri = do
  -- Add in the github auth token if it is provided.
  extraHeaders <- gets githubAuthToken >>= \case
    Nothing -> return []
    Just token -> return [("Authorization", "token " <> token)]
  let headers = extraHeaders <>
              [("Accept", "application/vnd.github.quicksilver-preview+json")]
  getHttp uri headers >>= \case
    HttpSuccess jsonStr -> case eitherDecode jsonStr of
      Left err -> throwErrorC ["couldn't parse JSON from github: ", pack err]
      Right info -> return info
    err -> throwError1 $ pshow err

-- | Queries NPM for package information.
getDefaultBranch :: Name -> Name -> NpmFetcher Name
getDefaultBranch owner repo = do
  let rpath = "/" <> owner <> "/" <> repo
  let uri = concat ["https://api.github.com/repos", rpath]
  putStrs ["Querying github for default branch of ", rpath, "..."]
  Git.rDefaultBranch <$> githubCurl uri

gitRefToSha :: Name -- ^ Repo owner
            -> Name -- ^ Repo name
            -> Name -- ^ Commit-ish, i.e. branch, hash or tag
            -> NpmFetcher Text -- ^ The hash of the branch
gitRefToSha owner repo ref = do
  let rpath = "/" <> owner <> "/" <> repo
  let uri = concat ["https://api.github.com/repos", rpath]
      fromBranch = do
        let bSha = Git.cSha . Git.bCommit
        bSha <$> githubCurl (concat [uri, "/branches/", ref])
      fromTag = do
        tagMap <- Git.tagListToMap <$> githubCurl (concat [uri, "/tags"])
        case H.lookup ref tagMap of
          Just sha -> return sha
          Nothing -> throwErrorC ["Ref is not a tag: ", ref]
      fromCommit = do
        map Git.cSha $ githubCurl $ concat [uri, "/commits/", ref]
      invalid = throwErrorC ["Ref is not valid: ", ref]
  fromBranch `ifErrorDo` fromTag `ifErrorDo` fromCommit `ifErrorDo` invalid

-- | Given a github repo and a branch, gets the SHA of the head of that
-- branch
getShaOfBranch :: Name -- ^ Repo owner
               -> Name -- ^ Repo name
               -> Name -- ^ Name of the branch to get
               -> NpmFetcher Text -- ^ The hash of the branch
getShaOfBranch owner repo branchName = do
  let rpath = "/" <> owner <> "/" <> repo
  let uri = concat ["https://api.github.com/repos", rpath,
                    "/branches/", branchName]
  putStrs ["Querying github for sha of ", rpath, "/", branchName, "..."]
  Git.cSha . Git.bCommit <$> githubCurl uri

-- | Fetch a package from github. Will convert a branch name into a specific
-- SHA so that the generated URL is deterministic.
fetchGithub :: URI -> NpmFetcher ResolvedDependency
fetchGithub uri = do
  (owner, repo) <- case split "/" $ uriPath uri of
    [_, owner, repo] -> return (pack owner, pack $ dropSuffix ".git" repo)
    _ -> throwErrorC ["Invalid repo path: ", pack $ uriPath uri]
  hash <- case uriFragment uri of
    -- if there isn't a ref or a tag, use the default branch.
    "" -> gitRefToSha owner repo =<< getDefaultBranch owner repo
    -- otherwise, use that as a tag.
    '#':frag -> return $ pack frag
    frag -> throwErrorC ["Invalid URL fragment '", pack frag, "'"]
  -- Use the hash to pull down a zip.
  let uri = concat ["https://github.com/", owner, "/", repo, "/archive/",
                    hash, ".tar.gz"]
  fetchHttp (repo <> "-" <> hash) (fromJust $ parseURI $ unpack uri)

makeGithubUri :: Name -- ^ Owner name
              -> Name -- ^ Repo name
              -> Text -- ^ Commit hash
              -> URI -- ^ Fully-formed URI
makeGithubUri owner repo commit = URI {
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
          return (makeGithubUri owner repo sha, repo <> "-" <> sha)
        ["", owner, repo] -> do
          branch <- getDefaultBranch owner repo
          sha <- gitRefToSha owner repo branch
          return (makeGithubUri owner repo sha, repo <> "-" <> sha)
      _ -> return (uri, "package")

-- | Given an arbitrary NPM version range (e.g. a semver range, a URL, etc),
-- figure out how to resolve the dependency.
resolveNpmVersionRange :: Name -- ^ Name of the package.
                       -> NpmVersionRange -- ^ Version bounds of the package.
                       -> NpmFetcher ResolvedDependency -- ^ Dependency.
resolveNpmVersionRange name range = case range of
  SemVerRange svr -> resolveDep name svr
  NpmUri uri -> case uriScheme uri of
    "git:" -> fetchGithub uri
    "git+https:" -> fetchGithub uri
    s | s == "http:" || s == "https:" -> do
      (uri', subpath) <- httpToGitUri uri
      fetchHttp subpath uri'
    scheme -> throwErrorC ["Unknown uri scheme ", pack scheme]
  GitId src owner repo rev -> case src of
    Github -> do
      let frag = maybe "" ("#" <>) rev
          uri = concat ["https://github.com/", owner, "/", repo, frag]
      fetchGithub $ fromJust $ parseURI $ unpack uri
    _ -> throwErrorC ["Can't handle git source ", pack $ show src]
  Tag tag -> resolveByTag tag name
  vr -> throwErrorC ["Don't know how to resolve dependency '",
                     pack $ show vr, "'"]

-- | Uses the set of downloaded packages as a cache to avoid unnecessary
-- duplication.
resolveDep :: Name -> SemVerRange -> NpmFetcher ResolvedDependency
resolveDep name range = H.lookup name <$> gets resolved >>= \case
  -- We've alread defined some versions of this package.
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
      return (Resolved bestVersion)
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

-- | Convenience function to return a broken package.
broken :: BrokenPackageReason -> NpmFetcher ResolvedDependency
broken = return . Broken

-- | Recur the fetch on a list of dependencies, at a decremented
-- development dependency depth.
recurOn :: Name -- ^ Name of the package whose dependencies these are.
        -> SemVer -- ^ Version of the package whose dependencies these are.
        -> DependencyType -- ^ Type of the dependency being fetched.
        -> Record NpmVersionRange -- ^ The dependency map.
        -> NpmFetcher (Record ResolvedDependency) -- ^ The result.
recurOn name version deptype deps = atDecrementedDevDepsDepth $
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
      result <- resolveNpmVersionRange depName depRange
      return (depName, result)

-- | Perform an action at a decremented development dependency depth.
atDecrementedDevDepsDepth :: NpmFetcher a -> NpmFetcher a
atDecrementedDevDepsDepth action = do
  depth <- gets devDependencyDepth
  modify $ \s -> s {devDependencyDepth = depth - 1}
  result <- action
  modify $ \s -> s {devDependencyDepth = depth}
  return result

-- | Tells us whether we should fetch development dependencies.
shouldFetchDevs :: NpmFetcher Bool
shouldFetchDevs = (> 0) <$> gets devDependencyDepth

-- | A VersionInfo is an abstraction of an NPM package. This will resolve
-- the version info into an actual package (recurring on the dependencies)
-- and add it to the resolved package map.
resolveVersionInfo :: VersionInfo -> NpmFetcher ResolvedDependency
resolveVersionInfo VersionInfo{..} = do
  let ctx = concat ["When resolving package ", viName, " version ",
                    renderSV viVersion]
  inContext ctx $ do
    let recurOn' = recurOn viName viVersion
    isBeingResolved viName viVersion >>= \case
      True -> do -- This is a cycle: allowed for dev dependencies, but we
                 -- don't want to loop infinitely so we just return.
                 return $ Resolved viVersion
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
        return $ Resolved viVersion

-- | Given a version range and a record of version infos, returns the
-- version info with the highest version that matches the version.

-- | Resolves a dependency given a name and version range.
_resolveDep :: Name -> SemVerRange -> NpmFetcher ResolvedDependency
_resolveDep name range = do
  let ctx = concat ["When resolving dependency ", name, " (",
                    pack $ show range, ")"]
  putStrsLn ["Resolving ", name, " (", pshow range, ")"]
  inContext ctx $ do
    pInfo <- getPackageInfo name
    current <- gets currentlyResolving
    -- Filter out packages currently being evaluated.
    let notCurrent = case H.lookup name current of
          Nothing -> piVersions pInfo
          Just cur -> H.difference (piVersions pInfo) cur
    -- Choose the entry with the highest version that matches the range.
    case filter (matches range) $ H.keys notCurrent of
      [] -> throwErrorC ["No versions satisfy given range"]
      matches -> do
        let versionInfo = notCurrent H.! maximum matches
        resolveVersionInfo versionInfo

-- | Resolve a dependency by tag name (e.g. a release tag).
resolveByTag :: Name -- ^ Tag name.
             -> Name -- ^ Name of the package.
             -> NpmFetcher ResolvedDependency -- ^ A resolved dependency.
resolveByTag tag pkgName = do
  pInfo <- getPackageInfo pkgName
  case H.lookup tag $ piTags pInfo of
    Nothing -> broken $ NoSuchTag tag
    Just version -> case H.lookup version $ piVersions pInfo of
      Nothing -> broken $ TagPointsToInvalidVersion tag version
      Just versionInfo -> resolveVersionInfo versionInfo

parseURIs :: [Text] -> [URI]
parseURIs rawUris = map p $! rawUris where
  p txt = case parseURI $ unpack txt of
            Nothing -> errorC ["Invalid URI: ", txt]
            Just uri -> uri

startState :: PackageMap PreExistingPackage
           -> [Text] -- ^ Registries to query.
           -> Maybe ByteString -- ^ A possible github token.
           -> Int -- ^ Depth to fetch dev dependencies.
           -> NpmFetcherState
startState existing registries token devDependencyDepth = do
  NpmFetcherState {
      registries = parseURIs registries,
      githubAuthToken = token,
      resolved = pmMap toFullyDefined existing,
      packageStackTrace = [],
      pkgInfos = mempty,
      currentlyResolving = mempty,
      knownProblematicPackages = HS.fromList ["websocket-server"],
      devDependencyDepth = devDependencyDepth,
      requestTimeout = 10
    }

runNpmFetchWith :: NpmFetcherState -> NpmFetcher a -> IO (a, NpmFetcherState)
runNpmFetchWith state x = do
  runStateT (runExceptT x) state >>= \case
    (Left elist, _) -> error $ "\n" <> (unpack $ render elist)
    (Right x, state) -> return (x, state)

runIt :: NpmFetcher a -> IO a
runIt action = do
  let registries = ["https://registry.npmjs.org/"]
  let state = startState mempty registries Nothing 0
  fst <$> runNpmFetchWith state action
