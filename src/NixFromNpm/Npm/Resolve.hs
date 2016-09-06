{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module NixFromNpm.Npm.Resolve where

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
import Data.Map (Map)
import qualified Data.Map as M
import Numeric (showHex)
import System.IO.Temp (openTempFile, createTempDirectory)
import Network.Curl (Long)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Text.Parsec as Parsec
import Shelly (shelly, run, run_, Sh, errExit, lastExitCode, lastStderr,
               silently)
import Network.URI (escapeURIString, isUnreserved)
import Nix.Expr
import Data.Digest.Pure.SHA (sha256, showDigest)
import Data.SemVer

import NixFromNpm.Common
import NixFromNpm.Git.Types as Git hiding (Tag)
import NixFromNpm.Conversion.ToNix (ResolvedPkg(..),
                                    fixName, nodePackagesDir, toDotNix,
                                    writeNix, resolvedPkgToNix)
import NixFromNpm.HttpTools (HttpError(..), getHttpWith)
import NixFromNpm.Npm.Types (VersionInfo(..), PossiblyCircularSemVer(..),
                             CircularSemVer(..),
                             PackageInfo(..), BrokenPackageReason(..),
                             DependencyType(..), ResolvedDependency(..),
                             DistInfo(..), Shasum(..))
import NixFromNpm.Npm.Version
import NixFromNpm.Npm.Version.Parser
import NixFromNpm.Npm.PackageMap

-- | Things which can be converted into nix expressions: either they
-- are actual nix expressions themselves (which can be either
-- existing in the output, or existing in an extension), or they are
-- new packages which we have discovered.
data FullyDefinedPackage
  = NewPackage ResolvedPkg
  | Existing PreExistingPackage
  deriving (Show, Eq)

-- | The type of pre-existing packages, which can either come from the
-- output path, or come from an extension
data PreExistingPackage
  = FromOutput NExpr
  | FromExtension Name NExpr
  deriving (Show, Eq)

toFullyDefined :: PreExistingPackage -> FullyDefinedPackage
toFullyDefined = Existing

-- | Settings that affect the behavior of the NPM fetcher.
data NpmFetcherSettings = NpmFetcherSettings {
  nfsRegistries :: [URI],
  -- ^ List of URIs that we can use to query for NPM packages, in order of
  -- preference.
  nfsNpmAuthTokens :: Record AuthToken,
  -- ^ Used for authorization when fetching a package from a private npm.
  -- The keys are namespaces.
  nfsGithubAuthToken :: Maybe AuthToken,
  -- ^ Used for authorization when fetching a package from github.
  nfsRequestTimeout :: Long,
  -- ^ Request timeout.
  nfsRetries :: Int,
  -- ^ Number of times to retry HTTP requests.
  nfsExtendPaths :: Record FilePath,
  -- ^ Libraries we're extending.
  nfsOutputPath :: FilePath,
  -- ^ Path we're outputting generated expressions to.
  nfsMaxDevDepth :: Int,
  -- ^ Maximum dev dependency fetch depth.
  nfsCacheDepth :: Int,
  -- ^ Depth at which to start using the cache. If this is 0, then we will
  -- always use the cache if we can. If it's 1, then the top-level package
  -- will not be cached, but lower-level packages will. Et cetera.
  nfsRealTimeWrite :: Bool,
  -- ^ Whether to write packages in real-time as the expressions are generated,
  -- rather than waiting until the end.
  nfsNpm3 :: Bool,
  -- ^ Whether generated packages should use npm3 by default.
  nfsOverwriteNixLibs :: Bool
  -- ^ If true, allow existing nix libraries in output to be overridden.
  } deriving (Show, Eq)

-- | The state of the NPM fetcher.
data NpmFetcherState = NpmFetcherState {
  resolved :: PackageMap FullyDefinedPackage,
  -- ^ Set of all of the packages that we have fully resolved.
  pkgInfos :: PRecord PackageInfo,
  -- ^ Set of all of the package info objects that we have fetched for
  -- a particular package.
  currentlyResolving :: PackageSet,
  -- ^ For cycle detection.
  packageStackTrace :: [(PackageName, SemVer)],
  -- ^ Stack of packages that we are resolving so we can get the path to the
  -- current package.
  brokenPackages :: HashMap PackageName
                    (Map NpmVersionRange BrokenPackageReport)
  -- ^ Record of broken packages.
  }

-- | The report of a broken package; stores why a package broke, and of what
-- package the package was a dependency.
data BrokenPackageReport = BrokenPackageReport {
  bprDependencyChains :: HashSet [(PackageName, SemVer)],
  bprDependencyOf :: PackageSet,
  -- ^ Which packages depend on this.
  bprReason :: BrokenPackageReason
  -- ^ Reason why it broke.
  } deriving (Eq, Show)

-- | The monad for fetching from NPM.
type NpmFetcher = RWST NpmFetcherSettings () NpmFetcherState IO

-- | Wraps the curl function to fetch from HTTP, setting some headers and
-- telling it to follow redirects.
npmGetHttp :: URI -- ^ URI to hit
        -> [(Text, ByteString)] -- ^ Additional headers to add
        -> NpmFetcher BL8.ByteString
npmGetHttp uri headers = do
  let defaultHeaders = [("Connection", "Close"),
                        ("User-Agent", "nixfromnpm-fetcher")]
  timeout <- asks nfsRequestTimeout
  retries <- asks nfsRetries
  putStrsLn ["Hitting URI ", uriToText uri]
  getHttpWith timeout retries (headers <> defaultHeaders) uri

-- | Insert a new, fully defined package into our state.
addPackage :: PackageName -> SemVer -> FullyDefinedPackage -> NpmFetcher ()
addPackage name version pkg = do
  modify $ \s -> s { resolved = pmInsert name version pkg (resolved s) }

rmPackage :: PackageName -> SemVer -> NpmFetcher ()
rmPackage name version = do
  modify $ \s -> s { resolved = pmDelete name version (resolved s)}

withoutPackage :: PackageName -> SemVer -> NpmFetcher a -> NpmFetcher a
withoutPackage name version action = do
  existing <- pmLookup name version <$> gets resolved
  rmPackage name version
  result <- action
  case existing of
    Nothing -> rmPackage name version
    Just pkg -> addPackage name version pkg
  return result

-- | Get the hex sha256 of a bytestring.
hexSha256 :: BL8.ByteString -> Shasum
hexSha256 = SHA256 . pack . showDigest . sha256

-- | Queries NPM for package information.
_getPackageInfo :: PackageName -> URI -> NpmFetcher PackageInfo
_getPackageInfo pkgName registryUri = do
  putStrsLn ["Querying ", uriToText registryUri,
             " for package ", tshow pkgName, "..."]
  authHeader <- case pkgName of
    PackageName name Nothing -> return [] -- no namespace, no auth
    PackageName name (Just namespace) -> do
      H.lookup namespace <$> asks nfsNpmAuthTokens >>= \case
        Nothing -> return []
        Just token -> do
          putStrsLn ["Using token ", tshow token, " for namespace ", namespace]
          return [("Authorization", "Bearer " <> token)]
  let route = case pkgName of
        PackageName name Nothing -> name
        PackageName name (Just namespace) ->
          concat ["@", namespace, "%2f", name]
  jsonStr <- npmGetHttp (registryUri // route) authHeader
             `catches` [
               Handler (\case
                HttpErrorWithCode 404 -> throw $ NoMatchingPackage pkgName
                err -> throw err)
               ]
  case eitherDecode jsonStr of
      Left err -> do
        let text = decodeUtf8 $ BL8.toStrict jsonStr
        throw $ InvalidPackageJson text err
      Right info -> do
        return info

-- | Same as _getPackageInfo, but caches results for speed.
getPackageInfo :: PackageName -> NpmFetcher PackageInfo
getPackageInfo name = do
  infos <- gets pkgInfos
  case H.lookup name infos of
    Just info -> do
      return info
    Nothing -> do
      let tryToFetch [] = throw $ NoMatchingPackage name
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

storePackageInfo :: PackageName -> PackageInfo -> NpmFetcher ()
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
  tarball <- npmGetHttp uri []
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
                   -> NpmFetcher VersionInfo -- ^ The version info object.
extractVersionInfo tarballPath = do
  temp <- tempDir "pkg-json-extract"
  putStrsLn ["Extracting ", pathToText tarballPath, " to tempdir ",
             pathToText temp]
  shelly $ run_ "tar" ["-xf", pathToText tarballPath, "-C", pathToText temp]
  result <- listDirFullPaths temp >>= \case
    -- Either the directory must have a single folder with a package.json
    [folder] -> extractPkgJson $ folder </> "package.json"
    -- Or it must itself contain a package.json.
    _ -> extractPkgJson $ temp </> "package.json"
  removeDirectoryRecursive temp
  return result

-- | Extract the version info out of a package.json file.
extractPkgJson :: FilePath -> NpmFetcher VersionInfo
extractPkgJson path = do
  putStrsLn ["Reading information from ", pathToText path]
  pkJson <- liftIO $ B.readFile (encodeString path)
  case eitherDecode $ BL8.fromStrict pkJson of
    Left err -> throw $ InvalidPackageJson (decodeUtf8 pkJson) err
    Right info -> return info

-- | Fetch a package over HTTP. Return the version of the fetched package,
-- and store the hash.
fetchHttp :: URI -- ^ The URI to fetch.
          -> NpmFetcher SemVer -- ^ The package at that URI.
fetchHttp uri = do
  -- Use nix-fetch to download and hash the tarball.
  (hash, tarballPath) <- prefetchSha256 uri
  -- Extract the tarball to a temp directory and parse the package.json.
  versionInfo <- extractVersionInfo tarballPath
  removeFile tarballPath
  -- Create the DistInfo.
  let dist = DistInfo {diUrl = uriToText uri,
                       diShasum = hash}
  -- Add the dist information to the version info and resolve it.
  versionInfoToSemVer (versionInfo {viDist = Just dist})

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
  jsonStr <- npmGetHttp uri headers
  case eitherDecode jsonStr of
    Left err -> throw $ InvalidJsonFromGithub (tshow err)
    Right info -> return info

makeGithubURI :: Name -> Name -> URI
makeGithubURI owner repo = do
  let baseUri = unsafeParseURI "https://api.github.com/repos/"
  baseUri // owner // repo

-- | Queries NPM for package information.
getDefaultBranch :: Name -> Name -> NpmFetcher GitRef
getDefaultBranch owner repo = do
  putStrs ["Querying github for default branch of ", repo, "..."]
  BranchName . rDefaultBranch <$> githubCurl (makeGithubURI owner repo)

-- | Attempts to convert an arbitrary github reference into a full
-- github commit hash. Tries the following in order:
-- * See if it's a branch name; if so get the hash of the branch head.
-- * See if it's a tag name; if so get the hash of the tagged commit.
-- * See if it's a commit hash; if so just return it.
gitRefToSha :: Name -- ^ Repo owner
            -> Name -- ^ Repo name
            -> GitRef -- ^ Github ref
            -> NpmFetcher Text -- ^ The hash of the branch
gitRefToSha owner repo ref = case ref of
  CommitHash hash -> return hash -- already done
  BranchName branch -> fromBranch branch
  TagName tag -> fromTag tag
  SomeRef whoknows -> tryAll whoknows
  where
    uri = makeGithubURI owner repo
    fromBranch ref = do
      let bSha = cSha . bCommit
      bSha <$> githubCurl (uri // "branches" // ref)
    fromTag tag = do
      tagMap <- tagListToMap <$> githubCurl (uri // "tags")
      case H.lookup tag tagMap of
        Just sha -> return sha
        Nothing -> throw (InvalidGitRef ref)
    fromCommit ref = do
      cSha <$> githubCurl (uri // "commits" // ref)
    catch404 action1 action2 = action1 `catch` \case
      HttpErrorWithCode 404 -> action2
      err -> throw err
    tryAll txt =
      fromBranch txt `catch404`
        fromTag txt `catch404`
          fromCommit txt `catch404`
            throw (InvalidGitRef ref)

-- | Fetches an arbitrary git repo from a uri.
fetchArbitraryGit :: URI -> NpmFetcher SemVer
fetchArbitraryGit uri = throw $
  NotYetImplemented $
    concat ["nixfromnpm can't fetch arbitrary git repos yet (",
            uriToString uri, ")"]

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

-- | Given the name of a tarball, convert it into a git ref, by stripping
-- off a trailing tar/zip extension.
tarballNameToRef :: Text -> GitRef
tarballNameToRef txt = do
  let drop ext = T.dropEnd (T.length ext)
  SomeRef $ case txt of
    _ | ".zip" `isSuffixOf` txt -> drop ".zip" txt
      | ".tgz" `isSuffixOf` txt -> drop ".tgz" txt
      | ".tar.gz" `isSuffixOf` txt -> drop ".tar.gz" txt
      | otherwise -> txt

-- | Fetch from a github URI. The URI must point to github.
fromGithubUri :: URI -> NpmFetcher SemVer
fromGithubUri uri = resolveUri `catch` \(e::GithubError) -> fetchHttp uri where
  resolveUri = do
    (owner, repo, sha) <- case T.split (=='/') $ pack (uriPath uri) of
      ["", owner, repo', "tarball", ref] -> do
        let repo = dropSuffix ".git" repo'
        sha <- gitRefToSha owner repo (SomeRef ref)
        return (owner, repo, sha)
      ["", owner, repo'] -> do
        let repo = dropSuffix ".git" repo'
        branch <- getDefaultBranch owner repo
        sha <- gitRefToSha owner repo branch
        return (owner, repo, sha)
      [_, owner, repo', "archive", ref] -> do
        let repo = dropSuffix ".git" repo'
        let ref' = tarballNameToRef ref
        hash <- gitRefToSha owner repo ref'
        return (owner, repo, hash)
      _ -> throw $ InvalidGithubUri uri
    fetchHttp $ makeGithubTarballURI owner repo sha

-- | Look up a name and version range to see if we have recorded this as being
-- broken.
getBroken :: PackageName
          -> NpmVersionRange
          -> NpmFetcher (Maybe BrokenPackageReport)
getBroken name range = H.lookup name <$> gets brokenPackages >>= \case
  Nothing -> return Nothing
  Just brokenMap -> return $ M.lookup range brokenMap

-- | Reports a new broken package.
addBroken :: PackageName
          -> NpmVersionRange
          -> BrokenPackageReason
          -> NpmFetcher ()
addBroken name range why = do
  broken <- gets brokenPackages
  let -- Get a map of version ranges to reports for this package.
      reports = H.lookupDefault mempty name broken
      -- Get the existing report if it's there; otherwise create one.
      report = case M.lookup range reports of
        Nothing -> BrokenPackageReport mempty mempty why
        Just rep -> rep
      newBroken = H.insert name (M.insert range report reports) broken
  modify $ \s -> s {brokenPackages = newBroken}

-- | Records a dependency for a broken package. Assumes the package already
-- exists in the broken package set; fails otherwise.
addReport :: PackageName -- ^ Name of the broken package.
          -> NpmVersionRange -- ^ Version range of the broken package.
          -> PackageName -- ^ Name of the depending package.
          -> SemVer -- ^ Version of the depending package.
          -> NpmFetcher () -- ^ Insert it into the broken package set.
addReport name range depOfName depOfVersion = do
  update <- H.lookup name <$> gets brokenPackages >>= \case
     Nothing -> fatal "No report exists for this package"
     Just reports -> case M.lookup range reports of
       Nothing -> fatal "No report exists for this package"
       Just rep -> do
         currentTrace <- gets packageStackTrace
         let depSet = psInsert depOfName depOfVersion (bprDependencyOf rep)
             chainSet = HS.insert currentTrace (bprDependencyChains rep)
             rep' = rep {bprDependencyOf = depSet,
                         bprDependencyChains = chainSet}
             reports' = M.insert range rep' reports
         return $ H.insert name reports'
  modify $ \s -> s {brokenPackages = update (brokenPackages s)}

-- | Given two (name, version) pairs, determine if there is
-- circularity between them. Circularity between A and B means that A
-- depends on B and A also appears somewhere in B's dependency closure.
isCircular :: PackageName -> SemVer -> PackageName -> SemVer -> NpmFetcher Bool
isCircular = undefined


-- | Resolve a dependency. Takes the name and version of a package,
-- and the name and version *range* of one of its dependencies.
resolveDependency :: PackageName -- ^ Name of the package.
                  -> SemVer      -- ^ Version of the package.
                  -> PackageName -- ^ Name of the dependency.
                  -> NpmVersionRange -- ^ Version bounds of the dependency.
                  -> NpmFetcher ResolvedDependency
                  -- ^ Resolved version, or an error.
resolveDependency pkgName pkgVersion depName depRange = do
  map Resolved resolver `catches` handleError where
    resolver = do
      depVersion <- resolveNpmVersionRange depName depRange
      -- So now we have a version, but it might be circular. To
      -- determine this, check if there's a circularity from this
      -- package, version pair to the other.
      isCircular pkgName pkgVersion depName depVersion >>= \case
        True -> return $ Circular $ CircularSemVer depVersion
        False -> return $ NotCircular depVersion
    handle reason = do
      warns ["Failed to fetch dependency ", tshow depName, " version ",
             tshow depRange, ": ", tshow reason]
      addBroken depName depRange reason
      addReport depName depRange pkgName pkgVersion
      return $ Broken reason
    handleError = [
      (Handler handle),
      (Handler $ handle . GithubError)
      ]

-- | Resolve a version range. Figures out if it should query NPM, or download
-- a package from git or http, etc.
resolveNpmVersionRange :: PackageName
                       -> NpmVersionRange
                       -> NpmFetcher SemVer
resolveNpmVersionRange name range = case range of
  SemVerRange svr -> resolveDep name svr
  NpmUri uri | isGithubUri uri -> fromGithubUri uri
             | otherwise -> case uriScheme uri of
      s | "http" `isPrefixOf` s -> fetchHttp uri
        | "git" `isPrefixOf` s -> fetchArbitraryGit uri
        | otherwise -> throw $ UnsupportedUriScheme s
  GitId src owner repo rev -> case src of
    Github -> do
      sha <- gitRefToSha owner repo =<< case rev of
               Nothing -> getDefaultBranch owner repo
               Just r -> return r
      let path = fromText (repo <> "-" <> sha)
      fetchHttp $ makeGithubTarballURI owner repo sha
    _ -> throw $ UnsupportedGitSource src
  Tag tag -> resolveByTag tag name
  vr -> throw $ UnsupportedVersionType range
  where
    isGithubUri uri = case uriAuthority uri of
      Just auth -> "github.com" `isSuffixOf` uriRegName auth
      Nothing -> False

-- | Uses the set of downloaded packages as a cache to avoid unnecessary
-- duplication.
resolveDep :: PackageName -> SemVerRange -> NpmFetcher SemVer
resolveDep name range = H.lookup name <$> gets resolved >>= \case
  -- We've already defined some versions of this package.
  Just versions -> case filter (matches range) (M.keys versions) of
    [] -> _resolveDep name range -- No matching versions, need to fetch.
    vs -> do
      let bestVersion = maximum vs
          versionDots = tshow bestVersion
          package = fromJust $ M.lookup bestVersion versions
      putStrs ["Requirement ", tshow name, " version ",
               pack $ show range, " already satisfied: "]
      putStrsLn $ case package of
        NewPackage _ -> ["fetched package version ", versionDots]
        Existing (FromOutput _) -> ["already had version ", versionDots,
                                   " in output directory (use --no-cache",
                                   " to override)"]
        Existing (FromExtension name _) -> ["version ", versionDots,
                                            " provided by extension ", name]
      return bestVersion
  -- We haven't yet found any versions of this package.
  Nothing -> _resolveDep name range

-- | Start resolving a package, and add it to the stack.
startResolving :: PackageName -> SemVer -> NpmFetcher ()
startResolving name ver = do
  showTrace
  modify $ \s -> do
    s {currentlyResolving = pmInsert name ver () $ currentlyResolving s,
       packageStackTrace = (name, ver) : packageStackTrace s}

-- | Mark a package as being finished, and pop it off the stack.
finishResolving :: PackageName -> SemVer -> NpmFetcher ()
finishResolving name ver = do
  modify $ \s ->
    s {currentlyResolving = pmDelete name ver $ currentlyResolving s,
       packageStackTrace = P.tail $ packageStackTrace s}
  putStrsLn ["Finished resolving ", tshow name, " ", tshow ver]

-- | Print the current package stack trace.
showTrace :: NpmFetcher ()
showTrace = do
  trace <- gets packageStackTrace
  putStrLn $ mapJoinBy " -> " (uncurry showPair) (reverse trace)

-- | Return whether a particular version of a package is being resolved.
isBeingResolved :: PackageName -> SemVer -> NpmFetcher Bool
isBeingResolved name version =
  pmMember name version <$> gets currentlyResolving

-- | Recur the fetch on a list of dependencies, at a decremented
-- development dependency depth.
recurOn :: PackageName -- ^ Name of the package whose dependencies these are.
        -> SemVer -- ^ Version of the package whose dependencies these are.
        -> DependencyType -- ^ Type of the dependency being fetched.
        -> PRecord NpmVersionRange -- ^ The dependency map.
        -> NpmFetcher (PRecord ResolvedDependency) -- ^ The result.
recurOn packageName version deptype deps = do
  map H.fromList $ do
    let depList = H.toList deps
        app txt (a, b) = (txt <> " " <> a, txt <> " " <> b)
        getDesc Dependency = ("dependency", "dependencies")
        getDesc DevDependency = app "development" (getDesc Dependency)
        getDesc PeerDependency = app "peer" (getDesc Dependency)
        getDesc OptionalDependency = app "optional" (getDesc Dependency)
        (desc, descPlural) = getDesc deptype
    when (length depList > 0) $ do
      putStrsLn [tshow packageName, " version ", tshow version, " has ",
                 descPlural, ": ", showDeps depList]
    forM depList $ \(depName, depRange) -> do
      putStrsLn ["Resolving ", showRangePair depName depRange, ", ", desc,
                 " of ", showPair packageName version]
      (,) depName <$> resolveDependency packageName version depName depRange

-- | Current depth, which is 0 if we're at the top level (in which case the
-- package stack trace would be length 1)
currentDepth :: NpmFetcher Int
currentDepth = map (\trace -> length trace - 1) $ gets packageStackTrace

-- | Tells us whether we should fetch development dependencies.
shouldFetchDevs :: NpmFetcher Bool
shouldFetchDevs = (<) <$> currentDepth <*> asks nfsMaxDevDepth

-- | A VersionInfo is an abstraction of an NPM package. This will resolve
-- the version info into an actual package (recurring on the dependencies)
-- and add it to the resolved package map.
resolveVersionInfo :: VersionInfo -- ^ Info about a package at a version.
                   -> NpmFetcher (ResolvedPkg, SemVer)
resolveVersionInfo VersionInfo{..} = do
  let recurOn' = recurOn viName viVersion
  startResolving viName viVersion
  deps :: PRecord ResolvedDependency <- recurOn' Dependency viDependencies
  peerDeps <- recurOn' PeerDependency viPeerDependencies
  optDeps <- recurOn' OptionalDependency viOptionalDependencies
  devDeps <- do
    shouldFetch <- shouldFetchDevs
    -- If the package doesn't declare any dev dependencies, we can "fetch"
    -- them for free, so we might as well here.
    case shouldFetch || H.null viDevDependencies of
      True -> Just <$> recurOn' DevDependency viDevDependencies
      False -> return Nothing
  finishResolving viName viVersion
  let rPkg = ResolvedPkg {
      rpName = viName,
      rpVersion = viVersion,
      rpDistInfo = viDist,
      rpMeta = viMeta,
      rpDependencies = deps,
      rpPeerDependencies = peerDeps,
      rpOptionalDependencies = optDeps,
      rpDevDependencies = devDeps
      }
  -- Write to disk if real-time is enabled.
  whenM (asks nfsRealTimeWrite) $ do
    writePackage viName viVersion $ resolvedPkgToNix rPkg
  -- Store this version's info.
  addPackage viName viVersion $ NewPackage rPkg
  return (rPkg, viVersion)

-- | Given the name of a package, return the path within the output
-- folder into which the package will be placed.
outputDirOf :: PackageName -- ^ Name of the package
            -> NpmFetcher FilePath -- ^ Path to where that package's
                                   -- nix expressions will be stored
outputDirOf (PackageName pkgName namespace) = do
  outputDir <- asks nfsOutputPath
  let folderName = case namespace of
        Nothing -> fixName pkgName
        Just namespace -> concat ["@", namespace, "/", fixName pkgName]
  return $ outputDir </> nodePackagesDir </> fromText folderName

-- | Given the name and version of a package, return the path within
-- the output folder into which the version file will be placed.
dotNixPathOf :: PackageName -- ^ Name of package
             -> SemVer -- ^ Version of package
             -> NpmFetcher FilePath
dotNixPathOf name version = do
  dir <- outputDirOf name
  return $ dir </> toDotNix version

-- | Looks at all nix files in a folder, finds the one with the most
-- recent version, and creates a `latest.nix` symlink to that file.
updateLatestNix :: MonadIO io
                => Maybe PackageName -- ^ Package which we're updating
                -> FilePath -- ^ Directory containing .nix files
                -> io ()
updateLatestNix maybePkgName dir = do
  -- Remove the `latest.nix` symlink if it exists.
  whenM (doesFileExist $ dir </> "latest.nix") $
    removeFile (dir </> "latest.nix")
  -- Grab the latest version and create a symlink `latest.nix`
  -- to that.
  let convert fname = case parseSemVer (getBaseName fname) of
        Left _ -> Nothing -- not a semver, so we don't consider it
        Right ver -> Just ver -- return the version
  catMaybes . map convert <$> listDirFullPaths dir >>= \case
    [] -> return ()
    versions -> do
      let latest = maximum versions
          label = case maybePkgName of
            Nothing -> getFilename dir
            Just pkgName -> tshow pkgName
      putStrsLn ["Latest version of ", label, " is ", tshow latest]
      createSymbolicLink (toDotNix latest) (dir </> "latest.nix")

-- | Where we're deriving the name from the path.
updateLatestNix' :: MonadIO io => FilePath -> io ()
updateLatestNix' = updateLatestNix Nothing

-- | Write a resolved package to disk.
writePackage :: PackageName -> SemVer -> NExpr -> NpmFetcher ()
writePackage name version expr = do
  dirPath <- outputDirOf name
  dotNixPath <- dotNixPathOf name version
  createDirectoryIfMissing dirPath
  putStrsLn ["Writing package file at ", pathToText dotNixPath]
  writeNix dotNixPath expr
  updateLatestNix (Just name) dirPath

-- | Resolve a @VersionInfo@ and get the resulting @ResolvedPkg@.
versionInfoToResolved :: VersionInfo -> NpmFetcher ResolvedPkg
versionInfoToResolved = map fst . resolveVersionInfo

-- | Resolve a @VersionInfo@ and get the resulting @SemVer@, which
-- might be circular.
versionInfoToSemVer :: VersionInfo
                    -> NpmFetcher SemVer
versionInfoToSemVer vInfo@VersionInfo{..} =
  isBeingResolved viName viVersion >>= \case
    True -> do
      -- This is a cycle. We don't want to loop infinitely
      -- so we just return.
      warns ["Circular package detected: ", tshow viName, "@",
              tshow viVersion]
      return viVersion
    False -> snd <$> resolveVersionInfo vInfo

-- | Resolves a dependency given a name and version range.
_resolveDep :: PackageName -> SemVerRange
            -> NpmFetcher SemVer
_resolveDep name range = do
  putStrsLn ["Resolving ", tshow name, " (", tshow range, ")"]
  pInfo <- getPackageInfo name
  current <- gets currentlyResolving
  -- Choose the entry with the highest version that matches the range.
  case filter (matches range) $ H.keys (piVersions pInfo) of
    [] -> throw $ NoMatchingVersion $ SemVerRange range
    matches -> do
      let versionInfo = piVersions pInfo H.! maximum matches
      versionInfoToSemVer versionInfo

-- | Resolve a dependency by tag name (e.g. a release tag).
resolveByTag :: Name -- ^ Tag name.
             -> PackageName -- ^ Name of the package.
             -> NpmFetcher SemVer -- ^ A resolved dependency.
resolveByTag tag pkgName = do
  pInfo <- getPackageInfo pkgName
  case H.lookup tag $ piTags pInfo of
    Nothing -> throw $ NoSuchTag tag
    Just version -> case H.lookup version $ piVersions pInfo of
      Nothing -> errorC ["Tag ", tag, " points to version ",
                         tshow version, ", but no such version of ",
                         tshow pkgName, " exists."]
      Just versionInfo -> versionInfoToSemVer versionInfo

-- | Default settings. The one caveat here is that there's no good way
-- to default the output path, so that will throw an error if it's not
-- set.
defaultSettings :: NpmFetcherSettings
defaultSettings = NpmFetcherSettings {
  nfsRequestTimeout = 10,
  nfsNpmAuthTokens = mempty,
  nfsGithubAuthToken = Nothing,
  nfsRegistries = [fromJust $ parseURI "https://registry.npmjs.org"],
  nfsOutputPath = error "default setings provide no output path",
  nfsExtendPaths = mempty,
  nfsMaxDevDepth = 1,
  nfsCacheDepth = 0,
  nfsRetries = 1,
  nfsRealTimeWrite = False,
  nfsNpm3 = True,
  nfsOverwriteNixLibs = False
  }

-- | Pull a ':'-separated list of tokens from the environment and parse
-- them. Return empty if the environment variable isn't set.
getNpmTokens :: MonadIO io => io (Record AuthToken)
getNpmTokens = getEnv "NPM_AUTH_TOKENS" >>= \case
  Nothing -> return mempty
  Just tokens -> parseNpmTokens (T.split (==':') tokens)

-- | The tokens should come in the form of NAMESPACE=TOKEN. This will
-- take all of the strings of that shape and stick them into a record.
parseNpmTokens :: MonadIO io => [Text] -> io (Record AuthToken)
parseNpmTokens = foldM step mempty where
  step tokenMap token = case T.split (=='=') token of
    [namespace, tokenText] -> do
      let tok :: AuthToken
          tok = encodeUtf8 tokenText
      return $ H.insert namespace (encodeUtf8 tokenText) tokenMap
    _ -> do
      warns ["Invalid NPM token: ", token]
      return tokenMap

-- | Use the environment variables to provide better default settings.
settingsFromEnv :: IO NpmFetcherSettings
settingsFromEnv = do
  npmTokensEnv <- getNpmTokens
  githubTokenEnv <- map encodeUtf8 <$> getEnv "GITHUB_TOKEN"
  output <- getEnv "NIXFROMNPM_OUTPUT" >>= \case
    Nothing -> (</> "nixfromnpm_output") <$> getCurrentDirectory
    Just dir -> return $ fromText dir
  return $ defaultSettings {
        nfsNpmAuthTokens = npmTokensEnv,
        nfsGithubAuthToken = githubTokenEnv,
        nfsOutputPath = output
        }

-- | Starting state, with all of the collections empty.
startState :: NpmFetcherState
startState = NpmFetcherState {
  resolved = mempty,
  packageStackTrace = mempty,
  pkgInfos = H.empty,
  currentlyResolving = mempty,
  brokenPackages = mempty
  }

-- | Run an npmfetcher action with given settings and state.
runNpmFetchWith :: NpmFetcherSettings
                -> NpmFetcherState
                -> NpmFetcher a
                -> IO (a, NpmFetcherState)
runNpmFetchWith settings state action = do
  (result, state', _) <- runRWST action settings state
  return (result, state')

-- | Same as @runIt@, but throws away the state at the end.
evalIt :: NpmFetcher a -> IO a
evalIt = map fst . runIt

-- | Use the environment to get the settings, and run with that.
runIt :: NpmFetcher a -> IO (a, NpmFetcherState)
runIt action = do
  settings <- settingsFromEnv
  runNpmFetchWith settings startState action
