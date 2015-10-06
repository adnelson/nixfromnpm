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

import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Dequeue as D
import qualified Text.Parsec as Parsec
import Shelly hiding (get)
import Nix.Types
import Network.HTTP.Types.Header
import Network.HTTP.Conduit
import Codec.Archive.Tar as Tar hiding (pack, unpack)
import Codec.Archive.Tar.Entry as Tar
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
  -- | Queue of packages waiting to be resolved, since we are fetching using
  -- breadth-first search.
  packageWaitQueue :: D.BankersDequeue (Name, SemVerRange),
  -- | For cycle detection.
  currentlyResolving :: PackageMap (),
  -- | Stack of packages that we are resolving so we can get the path to the
  -- current package.
  packageStackTrace :: [(Name, SemVer)],
  -- | Set of packages known to be problematic; it is an error if one of these
  -- packages appears in a dependency tree.
  knownProblematicPackages :: HashSet Name,
  -- | Boolean telling us whether to fetch dev dependencies.
  getDevDeps :: Bool,
  -- | The connection manager for performing HTTP requests.
  connectionManager :: Manager
  }

-- | The monad for fetching from NPM.
type NpmFetcher = ExceptT EList (StateT NpmFetcherState IO)

addResolvedPkg :: Name -> SemVer -> ResolvedPkg -> NpmFetcher ()
addResolvedPkg name version _rpkg = do
  let rpkg = NewPackage _rpkg
  modify $ \s -> s {
    resolved = pmInsert name version rpkg (resolved s)
    }

-- | Get the hex sha1 of a bytestring.
hexSha1 :: BL8.ByteString -> Text
hexSha1 = pack . concatMap (flip showHex "") . SHA1.hashlazy

-- | Queries NPM for package information.
_getPackageInfo :: Name -> URI -> NpmFetcher PackageInfo
_getPackageInfo pkgName registryUri = do
  let uri = uriToText $ registryUri `slash` pkgName
  putStrsLn ["Querying ", uriToText registryUri,
             " for package ", pkgName, "..."]
  jsonStr <- getHttp uri []
  case eitherDecode jsonStr of
    Left err -> throwErrorC ["couldn't parse JSON from NPM: ", pack err]
    Right info -> return info

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


getHttp :: Text -- URI to hit
        -> RequestHeaders -- Additional headers to add
        -> NpmFetcher BL8.ByteString
getHttp uri headers = do
  manager <- gets connectionManager
  req <- liftIO $ parseUrl $ unpack uri
  let defaultHdrs = [("Connection", "Close")]
  let req' = req {
        requestHeaders = headers <> defaultHdrs <> requestHeaders req
        }
  responseBody <$> httpLbs req' manager

-- |
-- entryFileContents :: Tar.EntryContent -> Either Text ByteString
-- entryFileContents (Tar.NormalFile contents _) = Right contents
-- entryFileContents _ = Left "entry is not a normal file"

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
  tarball <- getHttp (uriToText uri) []
  let hash = hexSha1 tarball
  -- Return the hash and the path.
  path <- liftIO $ do
    (path, handle) <- openTempFile "/tmp" "nixfromnpmfetch.tgz"
    hClose handle
    BL8.writeFile path tarball
    putStrsLn ["Wrote tarball to ", pack path]
    return path

  return (hash, path)

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

-- extractVersionInfo :: -- Name -- ^ Name of package being fetched.
--                       -- -> SemVer -- ^ Version of package being fetched.
--                    P.FilePath -- ^ Contents of the tarball
--                    -> Text -- ^ Subpath within tarball.
--                    -> NpmFetcher VersionInfo -- ^ A VersionInfo object.
-- extractVersionInfo  tarballPath subpath = do

--   bytes <- liftIO $ BL8.readFile tarballPath
--   let failureFunc e = error $ "Tarball contained an error: " <> show e
--       entryList = foldEntries (:) [] failureFunc $ Tar.read bytes
--       isPkgJson e = hasSuffix "package.json" $ entryPath e
--   pkJsonContents <- case filter isPkgJson entryList of
--     [] -> throwError1 "No package.json found"
--     (e:_) -> case entryContent e of
--       NormalFile contents _ -> return contents
--       _ -> throwError1 "package.json entry was not a normal file"
--   case eitherDecode pkJsonContents of
--     Left err -> error $ "couldn't parse JSON as VersionInfo: " <> err
--     Right info -> return info

-- | Fetch a package over HTTP. Return the version of the fetched package,
-- and store the hash.
fetchHttp :: Text -- ^ Subpath in which to find the package.json.
          -> URI -- ^ The URI to fetch.
          -> NpmFetcher SemVer -- ^ The version of the package at that URI.
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
                [("Accept", "application/vnd.github.quicksilver-preview+json"),
                 ("User-Agent", "nixfromnpm-fetcher")]
  jsonStr <- getHttp uri headers
  case eitherDecode jsonStr of
    Left err -> throwErrorC ["couldn't parse JSON from github: ", pack err]
    Right info -> return info

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
fetchGithub :: URI -> NpmFetcher SemVer
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
    frag -> throwErrorC ["Invalid fragment '", pack frag, "'"]
  -- Use the hash to pull down a zip.
  let uri = concat ["https://github.com/", owner, "/", repo, "/archive/",
                    hash, ".tar.gz"]
  fetchHttp (repo <> "-" <> hash) (fromJust $ parseURI $ unpack uri)

resolveNpmVersionRange :: Name -> NpmVersionRange -> NpmFetcher SemVer
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
resolveDep :: Name -> SemVerRange -> NpmFetcher SemVer
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
      return bestVersion
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

resolveVersionInfo :: VersionInfo -> NpmFetcher SemVer
resolveVersionInfo versionInfo = do
  let name = viName versionInfo
      version = viVersion versionInfo
      ctx = concat ["When resolving package ", name, ", version ", version]
  inContext ctx $ do
    version <- case parseSemVer $ viVersion versionInfo of
      Left err -> throwErrorC ["Invalid semver in versionInfo object ",
                                   viVersion versionInfo,
                                   " Due to: ", pack $ show err]
      Right v -> return v
    isBeingResolved name version >>= \case
      True -> do putStrsLn ["Warning: cycle detected"]
                 return version
      False -> do
        -- Define a recursion function that takes a string describing the
        -- dependency type, and a list of dependencies of that type.
        let recurOn deptype deps = map H.fromList $ do
              let depList = H.toList $ deps versionInfo
              when (length depList > 0) $
                putStrsLn [name, " version ", renderSV version, " has ",
                           deptype, ": ", pack (show depList)]
              res <- map catMaybes $ forM depList $ \(depName, depRange) -> do
                HS.member depName <$> gets knownProblematicPackages >>= \case
                  True -> do
                    putStrsLn ["WARNING: ", name, " is a broken package"]
                    return Nothing
                  False -> do
                    depVersion <- resolveNpmVersionRange depName depRange
                    return $ Just (depName, depVersion)
              return res
        -- We need to recur into the package's dependencies.
        -- To prevent the cycles, we store which packages we're currently
        -- resolving.
        startResolving name version
        deps <- recurOn "dependencies" viDependencies
        devDeps <- gets getDevDeps >>= \case
          True -> recurOn "dev dependencies" viDevDependencies
          False -> return mempty
        finishResolving name version
        let dist = case viDist versionInfo of
              Nothing -> error "Version information did not include dist"
              Just d -> d
        -- Store this version's info.
        addResolvedPkg name version $ ResolvedPkg {
            rpName = name,
            rpVersion = version,
            rpDistInfo = dist,
            rpMeta = viMeta versionInfo,
            rpDependencies = deps,
            rpDevDependencies = devDeps
          }
        return version

-- | Resolves a dependency given a name and version range.
_resolveDep :: Name -> SemVerRange -> NpmFetcher SemVer
_resolveDep name range = do
  let ctx = concat ["When resolving dependency ", name, " (",
                    pack $ show range, ")"]
  inContext ctx $ do
    pInfo <- getPackageInfo name
    versionInfo <- bestMatchFromRecord range $ piVersions pInfo
    resolveVersionInfo versionInfo

resolveByTag :: Name -> Name -> NpmFetcher SemVer
resolveByTag tag pkgName = do
  pInfo <- getPackageInfo pkgName
  case H.lookup tag $ piTags pInfo of
    Nothing -> throwErrorC ["Package ", pkgName, " has no tag '", tag, "'"]
    Just version -> case H.lookup version $ piVersions pInfo of
      Nothing -> throwErrorC ["Tag '", tag, "' refers to version '", version,
                              "' but no such version exists for package ",
                              pack $ show pkgName]
      Just versionInfo -> resolveVersionInfo versionInfo

parseURIs :: [Text] -> [URI]
parseURIs rawUris = map p $! rawUris where
  p txt = case parseURI $ unpack txt of
            Nothing -> errorC ["Invalid URI: ", txt]
            Just uri -> uri

startState :: Manager
           -> PackageMap PreExistingPackage
           -> [Text] -- ^ Registries to query.
           -> Maybe ByteString -- ^ A possible github token.
           -> Bool -- ^ Whether to not fetch dev dependencies.
           -> NpmFetcherState
startState manager existing registries token noDev = do
  NpmFetcherState {
      registries = parseURIs registries,
      githubAuthToken = token,
      resolved = pmMap toFullyDefined existing,
      packageWaitQueue = D.empty,
      packageStackTrace = [],
      pkgInfos = mempty,
      currentlyResolving = mempty,
      knownProblematicPackages = HS.fromList ["websocket-server"],
      getDevDeps = not noDev,
      connectionManager = manager
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

getPkg :: Name -- ^ Name of package to get.
       -> PackageMap PreExistingPackage -- ^ Set of pre-existing packages.
       -> Maybe ByteString -- ^ A possible github token.
       -> Bool -- ^ Whether to not fetch dev dependencies.
       -> IO (PackageMap FullyDefinedPackage) -- ^ Set of fully defined packages.
getPkg name existing token noDev = do
  registries <- getRegistries
  manager <- newManager tlsManagerSettings
  let state = startState manager existing registries token noDev
  (_, finalState) <- runItWith state (resolveDep name anyVersion)
  return (resolved finalState)
