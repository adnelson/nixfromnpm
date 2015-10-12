{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import System.IO.Temp (openTempFile)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Text.Parsec as Parsec
import Shelly hiding (get)
import Nix.Types
import Network.Curl
import qualified Crypto.Hash.SHA1 as SHA1

import NixFromNpm.Common
import NixFromNpm.NpmTypes
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

addResolvedPkg :: Name -> SemVer -> ResolvedPkg -> NpmFetcher ()
addResolvedPkg name version _rpkg = do
  let rpkg = NewPackage _rpkg
  modify $ \s -> s {
    resolved = pmInsert name version rpkg (resolved s)
    }

-- | Get the hex sha1 of a bytestring.
hexSha1 :: BL8.ByteString -> Text
hexSha1 = pack . concatMap (flip showHex "") . SHA1.hashlazy

data HttpResult a
  = HttpSuccess a
  | HttpError Int
  | HttpTimedOut Long
  deriving (Show, Eq)

curl' :: Text -> [CurlOption] -> NpmFetcher (HttpResult BL8.ByteString)
curl' uri opts = do
  timeout <- gets requestTimeout
  let opts' = opts <> [CurlTimeout timeout]
  (code, status, content) <- curlGetBS (unpack uri) opts'
  case code of
    CurlOK -> return $ HttpSuccess content
    CurlHttpReturnedError -> return $ HttpError status
    CurlOperationTimeout -> return $ HttpTimedOut timeout
    err -> throwErrorC ["Curl threw an unknown error ", pack $ show err]

-- | Queries NPM for package information.
_getPackageInfo :: Name -> URI -> NpmFetcher PackageInfo
_getPackageInfo pkgName registryUri = do
  let uri = uriToText $ registryUri `slash` pkgName
  putStrsLn ["Querying ", uriToText registryUri,
             " for package ", pkgName, "..."]
  curl' uri [] >>= \case
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


toSemVerList :: Record a -> NpmFetcher [(SemVer, a)]
toSemVerList record = do
  -- Pairings of parsed semvers (or errors) to values.
  let parsePair (k, v) = (parseSemVer k, v)
      pairs = map parsePair $ H.toList record
  case filter (\(k, _) -> isRight k) pairs of
    [] -> throwError1 "No correctly-formatted versions strings found"
    okPairs -> return $ map (\(Right k, v) -> (k, v)) okPairs

bestMatchFromRecord :: SemVerRange -> Record a -> NpmFetcher a
bestMatchFromRecord range record = do
  pairs <- toSemVerList record
  case filter (matches range . fst) pairs of
    [] -> throwErrorC ["No versions satisfy given range"]
    matches -> return $ snd $ maximumBy (compare `on` fst) matches

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

-- | Convert (key, value) pairs into a curl Headers option.
makeHeaders :: [(Text, ByteString)] -> CurlOption
makeHeaders headers = CurlHttpHeaders $ map mk headers where
  mk (key, val) = T.unpack key <> ": " <> B.unpack val

getHttp :: Text -- URI to hit
        -> [(Text, ByteString)] -- Additional headers to add
        -> NpmFetcher (HttpResult BL8.ByteString)
getHttp uri headers = do
  let defHdrs = [("Connection", "Close"), ("User-Agent", "nixfromnpm-fetcher")]
  curl' uri [makeHeaders (headers <> defHdrs), CurlFollowLocation True]

-- | Returns the SHA1 hash of the result of fetching the URI, and the path
--   in which the tarball is stored.
prefetchSha1 :: URI -> NpmFetcher (Text, P.FilePath)
prefetchSha1 uri = do
  putStrsLn ["Pre-fetching url ", uriToText uri]
  -- Make a temporary directory.
  dir <- liftIO getTemporaryDirectory
  let outPath = dir <> "/outfile"
  putStrsLn ["putting in location ", pack outPath]
  -- Download the file into memory, calculate the hash.
  getHttp (uriToText uri) [] >>= \case
    HttpSuccess tarball -> do
      let hash = hexSha1 tarball
      -- Return the hash and the path.
      path <- liftIO $ do
        (path, handle) <- openTempFile "/tmp" "nixfromnpmfetch.tgz"
        hClose handle
        BL8.writeFile path tarball
        putStrsLn ["Wrote tarball to ", pack path]
        return path
      return (hash, path)
    err -> throwErrorC ["Could not fetch tarball at url ", uriToText uri]

extractVersionInfo :: P.FilePath -> Text -> NpmFetcher VersionInfo
extractVersionInfo tarballPath subpath = do
  pkJson <- silentShell $ withTmpDir $ \dir -> do
    chdir dir $ do
      putStrs ["Extracting ", pack tarballPath, " to tempdir"]
      run_ "tar" ["-xf", pack tarballPath]
      -- Find all of the package.json files. The one we want to read should be
      -- the one with the shortest path, i.e. the first one found.
      T.lines <$> run "find" [".", "-name", "package.json"] >>= \case
        [] -> error "No package.json found"
        jsons -> do
          -- The correct package.json should be the one that
          let pth = L.minimumBy (\p p' -> compare (length p) (length p')) jsons
          putStrsLn ["Reading information from ", pth]
          map decodeUtf8 $ readBinary $ fromString $ unpack $ pth
  case eitherDecode $ BL8.fromChunks [encodeUtf8 pkJson] of
    Left err -> throwErrorC ["couldn't parse JSON as VersionInfo: ", pack err,
                             "\npackage.json contents:\n", pkJson]
    Right info -> return info

-- | Fetch a package over HTTP. Return the version of the fetched package,
-- and store the hash.
fetchHttp :: Text -- ^ Subpath in which to find the package.json.
          -> URI -- ^ The URI to fetch.
          -> NpmFetcher ResolvedDependency -- ^ The package at that URI.
fetchHttp subpath uri = do
  -- Use nix-fetch to download and hash the tarball.
  (hash, tarballPath) <- prefetchSha1 uri
  -- Extract the tarball to a temp directory and parse the package.json.
  versionInfo <- extractVersionInfo tarballPath subpath
  -- Create the DistInfo.
  let dist = DistInfo {diUrl = uriToText uri, diShasum = hash}
  -- Add the dist information to the version info and resolve it.
  resolveVersionInfo $ versionInfo {viDist = Just dist}

githubCurl :: Text -> NpmFetcher Value
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
  githubCurl uri >>= \case
    Object o -> case H.lookup "default_branch" o of
      Just (String b) -> putStr " OK. " >> return b
      Nothing -> putStrLn "" >> error "No default branch, or not a string"
    _ -> error "Expected an object back from github"

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
  githubCurl uri >>= \case
    Object o -> case H.lookup "commit" o of
      Just (Object o') -> case H.lookup "sha" o' of
        Just (String sha) -> return sha
        Nothing -> error "No sha in commit info"
      Nothing -> error "No commit info"
    _ -> error "Didn't get an object back"

-- | Fetch a package from git.
fetchGithub :: URI -> NpmFetcher ResolvedDependency
fetchGithub uri = do
  (owner, repo) <- case split "/" $ uriPath uri of
    [_, owner, repo] -> return (pack owner, pack $ dropSuffix ".git" repo)
    _ -> throwErrorC ["Invalid repo path: ", pack $ uriPath uri]
  hash <- case uriFragment uri of
    -- if there isn't a ref or a tag, use the default branch.
    "" -> do
      branch <- getDefaultBranch owner repo
      putStrLn $ " Branch is " <> branch
      sha <- getShaOfBranch owner repo branch
      putStrLn $ " Hash is " <> sha
      return sha
    -- otherwise, use that as a tag.
    '#':frag -> return $ pack frag
    frag -> throwErrorC ["Invalid URL fragment '", pack frag, "'"]
  -- Use the hash to pull down a zip.
  let uri = concat ["https://github.com/", owner, "/", repo, "/archive/",
                    hash, ".tar.gz"]
  fetchHttp (repo <> "-" <> hash) (fromJust $ parseURI $ unpack uri)

resolveNpmVersionRange :: Name -- ^ Name of the package.
                       -> NpmVersionRange -- ^ Version bounds of the package.
                       -> NpmFetcher ResolvedDependency -- ^ Dependency.
resolveNpmVersionRange name range = case range of
  SemVerRange svr -> resolveDep name svr
  NpmUri uri -> case uriScheme uri of
    "git:" -> fetchGithub uri
    "git+https:" -> fetchGithub uri
    "http:" -> fetchHttp "package" uri
    "https:" -> fetchHttp  "package" uri
    scheme -> throwErrorC ["Unknown uri scheme ", pack scheme]
  GitId src owner repo rev -> case src of
    Github -> do
      let frag = case rev of
            Nothing -> ""
            Just r -> "#" <> r
      let uri = concat ["https://github.com/", owner, "/", repo, frag]
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

startResolving :: Name -> SemVer -> NpmFetcher ()
startResolving name ver = do
  putStrsLn ["Resolving ", name, " version ", renderSV ver]
  modify $ \s -> do
    s {currentlyResolving = pmInsert name ver () $ currentlyResolving s}

finishResolving :: Name -> SemVer -> NpmFetcher ()
finishResolving name ver = do
  modify $ \s ->
    s {currentlyResolving = pmDelete name ver $ currentlyResolving s}
  putStrsLn ["Finished resolving ", name, " ", renderSV ver]

isBeingResolved :: Name -> SemVer -> NpmFetcher Bool
isBeingResolved name version =
  pmMember name version <$> gets currentlyResolving

broken :: BrokenPackageReason -> NpmFetcher ResolvedDependency
broken = return . Broken

_recurOn :: Name
         -> SemVer
         -> DependencyType
         -> Record NpmVersionRange
         -> NpmFetcher (Record ResolvedDependency)
_recurOn name version deptype deps = map H.fromList $ do
  let depList = H.toList deps
      (desc, descPlural) = case deptype of
        Dependency -> ("dependency", "dependencies")
        DevDependency -> ("development dependency",
                          "development dependencies")
  when (length depList > 0) $ do
    putStrsLn [name, " version ", renderSV version, " has ",
               descPlural, ": ", showDeps depList]
  forM depList $ \(depName, depRange) -> do
    putStrsLn ["Resolving ", depName, ", ", desc, " of ", name]
    result <- resolveNpmVersionRange depName depRange
    return (depName, result)

_decrementDevDepsDepth :: NpmFetcher ()
_decrementDevDepsDepth = modify $ \s ->
  s {devDependencyDepth = devDependencyDepth s - 1}

resolveVersionInfo :: VersionInfo -> NpmFetcher ResolvedDependency
resolveVersionInfo versionInfo = do
  let name = viName versionInfo
      versionStr = viVersion versionInfo
      ctx = concat ["When resolving package ", name, ", version ", versionStr]
  inContext ctx $ case parseSemVer versionStr of
      Left err -> broken $ InvalidSemVerSyntax versionStr (pack $ show err)
      Right version -> do
        let recurOn = _recurOn name version
        isBeingResolved name version >>= \case
          True -> do -- This is a cycle: allowed for dev dependencies, but we
                     -- don't want to loop infinitely so we just return.
                     return $ Resolved version
          False -> do
            -- Define a recursion function that takes a string describing the
            -- dependency type, and a list of dependencies of that type, and
            -- recursively fetches all of the dependencies of that type.
            let
            -- We need to recur into the package's dependencies.
            -- To prevent the cycles, we store which packages we're currently
            -- resolving.
            startResolving name version
            deps <- recurOn Dependency (viDependencies versionInfo)
            devDeps <- gets devDependencyDepth >>= \case
              n | n <= 0 -> return mempty -- ignoring development dependencies.
                | otherwise -> do
                    _decrementDevDepsDepth
                    recurOn DevDependency (viDevDependencies versionInfo)
            finishResolving name version
            case viDist versionInfo of
              Nothing -> broken NoDistributionInfo
              Just dist -> do
                -- Store this version's info.
                addResolvedPkg name version $ ResolvedPkg {
                    rpName = name,
                    rpVersion = version,
                    rpDistInfo = dist,
                    rpMeta = viMeta versionInfo,
                    rpDependencies = deps,
                    rpDevDependencies = devDeps
                  }
                return $ Resolved version

-- | Resolves a dependency given a name and version range.
_resolveDep :: Name -> SemVerRange -> NpmFetcher ResolvedDependency
_resolveDep name range = do
  let ctx = concat ["When resolving dependency ", name, " (",
                    pack $ show range, ")"]
  putStrsLn ["Resolving ", name, " (", pshow range, ")"]
  inContext ctx $ do
    pInfo <- getPackageInfo name
    (do versionInfo <- bestMatchFromRecord range (piVersions pInfo)
        resolveVersionInfo versionInfo)
      `ifErrorDo` do
        putStrsLn ["Could not find any matching packages"]
        return $ Broken $ NoMatchingVersion $ SemVerRange range

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

-- | Read NPM registry from env or use default.
getRegistries :: IO [Text]
getRegistries = do
  let npmreg = "https://registry.npmjs.org/"
  others <- shelly $ silently $ do
    get_env "ADDITIONAL_NPM_REGISTRIES" >>= \case
      Nothing -> return []
      Just regs -> return $ T.words regs
  return (others `snoc` npmreg)

-- | Read github auth token from env or use none.
getToken :: IO (Maybe Text)
getToken = shelly $ silently $ get_env "GITHUB_TOKEN"

runItWith :: NpmFetcherState -> NpmFetcher a -> IO (a, NpmFetcherState)
runItWith state x = do
  runStateT (runExceptT x) state >>= \case
    (Left elist, _) -> error $ "\n" <> (unpack $ render elist)
    (Right x, state) -> return (x, state)

runIt :: NpmFetcher a -> IO a
runIt action = do
  registries <- getRegistries
  let state = startState mempty registries Nothing 0
  fst <$> runItWith state action

getPkg :: Name -- ^ Name of package to get.
       -> NpmVersionRange -- ^ Version bounds of the package.
       -> PackageMap PreExistingPackage -- ^ Set of pre-existing packages.
       -> Maybe ByteString -- ^ A possible github token.
       -> Int -- ^ Depth to which to fetch dev dependencies.
       -> IO (PackageMap FullyDefinedPackage) -- ^ Set of fully defined packages.
getPkg name range existing token devDependencyDepth = do
  registries <- getRegistries
  let state = startState existing registries token devDependencyDepth
  (_, finalState) <- runItWith state $ resolveNpmVersionRange name range
  return $ resolved finalState
