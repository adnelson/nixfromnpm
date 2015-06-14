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

import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams hiding (mapM, map, filter, lines)
import System.IO.Streams.Attoparsec
import System.IO.Streams.HTTP
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Text.Parsec as Parsec
import Shelly hiding (get)

import NixFromNpm.Common
import NixFromNpm.NpmTypes
import NixFromNpm.SemVer
import NixFromNpm.Parsers.Common hiding (Parser, Error, lines)
import NixFromNpm.Parsers.SemVer
import NixFromNpm.NpmVersion
import NixFromNpm.Parsers.NpmVersion
--------------------------------------------------------------------------

data NpmFetcherState = NpmFetcherState {
  registry :: URI,
  githubAuthToken :: Maybe Text,
  resolved :: Record (HashMap SemVer ResolvedPkg),
  pkgInfos :: Record PackageInfo,
  -- For cycle detection.
  currentlyResolving :: HashSet (Name, SemVer)
} deriving (Show, Eq)

type NpmFetcher = StateT NpmFetcherState IO

addResolvedPkg :: Name -> SemVer -> ResolvedPkg -> NpmFetcher ()
addResolvedPkg name version rpkg = do
  pkgSet <- H.lookupDefault mempty name <$> gets resolved
  modify $ \s -> s {
    resolved = H.insert name (H.insert version rpkg pkgSet) (resolved s)
    }

-- | Queries NPM for package information.
getPackageInfo :: URI -> Name -> IO PackageInfo
getPackageInfo registryUri pkgName = do
  let mkErr m = cerror ["When attempting to get the information for ",
                        unpack pkgName, ": ", m]
  let uri = registryUri `slash` pkgName
  putStr ("Querying NPM for package " <> pkgName <> "...")
  j <- withOpenSSL $ do
    req <- parseUrl $ uriToString uri
    withManager (opensslManagerSettings context) $ \mgr ->
      withHTTP req mgr $ \response -> do
        let body = responseBody response
        jsonBlob <- parseFromStream json body
        return $ fromJSON jsonBlob
  case j of
    Error e -> putStrLn "" >> mkErr ("Couldn't parse JSON: " <> e)
    Success s -> putStrLn "OK" >> return s

-- | Same as getPackageInfo, but caches results for speed.
getPackageInfoCached :: Name -> NpmFetcher PackageInfo
getPackageInfoCached name = lookup name . pkgInfos <$> get >>= \case
  Just info -> return info
  Nothing -> do
    reg <- gets registry
    info <- liftIO (getPackageInfo reg name)
    modify $ \s -> s {pkgInfos = H.insert name info (pkgInfos s)}
    return info

bestMatchFromRecord :: SemVerRange -> Record a -> Either String (SemVer, a)
bestMatchFromRecord range rec = do
  -- Pairings of parsed semvers (or errors) to values.
  let parsePair (k, v) = (parseSemVer k, v)
      pairs = map parsePair $ H.toList rec
  case filter (\(k, _) -> isRight k) pairs of
    [] -> Left "No correctly-formatted versions strings found"
    okPairs -> do
      -- Filter to the ones which satisfy the range, and which are not
      -- currently being resolved.
      let pairs' = map (\(Right k, v) -> (k, v)) okPairs
      case filter (matches range . fst) pairs' of
        [] -> Left "No versions satisfy given range"
        matches -> Right $ L.maximumBy (compare `on` fst) matches

silentShell :: MonadIO m => Sh a -> m a
silentShell = shelly . silently

-- | Returns the SHA1 hash of the result of fetching the URI, and the path
-- in which the tarball is stored.
nixPrefetchSha1 :: URI -> NpmFetcher (Text, FilePath)
nixPrefetchSha1 uri = silentShell $ do
  hashAndPath <- sub $ setenv "PRINT_PATH" "1" >> do
    run "nix-prefetch-url" ["--type", "sha1", uriToText uri]
  if length (lines hashAndPath) /= 2
  then error "Expected two lines from nix-prefetch-url"
  else do
    let [hashBase32, path] = lines hashAndPath
    -- Convert the hash to base16, which is the format NPM uses.
    hash <- run "nix-hash" ["--type", "sha1", "--to-base16", hashBase32]
    return (T.strip hash, fromString $ unpack path)

extractVersionInfo :: FilePath -> Maybe Text -> NpmFetcher VersionInfo
extractVersionInfo tarballPath subpath = silentShell $ withTmpDir $ \dir -> do
  chdir dir $ do
    putStrsLn ["Extracting ", pathToText tarballPath, " to tempdir"]
    run_ "tar" ["-xf", pathToText tarballPath]
    let subpath' = maybe "" (<> "/") subpath <> "package.json"
    pkJson <- readBinary $ fromString $ unpack subpath'
    case eitherDecode $ BL8.fromChunks [pkJson] of
      Left err -> error $ "couldn't parse JSON as VersionInfo: " <> err
      Right info -> return info

-- | Fetch a package over HTTP. Return the version of the fetched package,
-- and store the hash.
fetchHttp :: Maybe Text -- | Subpath in which to find the package.json.
          -> URI -- | The URI to fetch.
          -> NpmFetcher SemVer -- | The version of the package at that URI.
fetchHttp subpath uri = do
  -- Use nix-fetch to download and hash the tarball.
  (hash, tarballPath) <- nixPrefetchSha1 uri
  -- Extract the tarball to a temp directory and parse the package.json.
  versionInfo <- extractVersionInfo tarballPath subpath
  -- Create the DistInfo.
  let dist = DistInfo {tiUrl = uriToText uri, tiShasum = hash}
  -- Add the dist information to the version info and resolve it.
  resolveVersionInfo $ versionInfo {viDist = Just dist}

githubCurl :: Text -> NpmFetcher Value
githubCurl uri = do
  curlArgs <- gets githubAuthToken >>= \case
    Nothing -> return []
    Just token -> return ["-H", "Authorization: token " <> token]
  jsonStr <- silentShell $ run "curl" $ curlArgs <> [uri]
  case eitherDecode $ BL8.fromChunks [T.encodeUtf8 jsonStr] of
    Left err -> cerror ["couldn't parse JSON from github: ", err]
    Right info -> return info

type RepoPath = String

-- | Queries NPM for package information.
getDefaultBranch :: RepoPath -- | Repo path
                 -> NpmFetcher Name -- | The default branch
getDefaultBranch repoPath = do
  let rpath = pack repoPath
  let uri = concat ["https://api.github.com/repos/", rpath]
  putStrs ["Querying github for default branch of ", rpath]
  githubCurl uri >>= \case
    Object o -> case H.lookup "default_branch" o of
      Just (String b) -> putStrLn "OK." >> return b
      Nothing -> putStrLn "" >> error "No default branch, or not a string"
    _ -> error "Expected an object back from github"

getShaOfBranch :: RepoPath -- | Repo path
               -> Name -- | Name of the branch to get
               -> NpmFetcher Text -- | The hash of the default branch
getShaOfBranch repoPath branchName = do
  let rpath = pack repoPath
  let uri = T.intercalate "/" ["https://api.github.com/repos", rpath,
                               "branches", branchName]
  putStrs ["Querying github for sha of ", rpath, "/", branchName]
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
  let repoPath = uriPath uri
  hash <- case uriFragment uri of
    -- if there isn't a ref or a tag, use the default branch.
    "" -> getShaOfBranch repoPath =<< getDefaultBranch repoPath
    -- otherwise, use that as a tag.
    frag -> error "Can't yet fetch tags"
  -- Use the hash to pull down a zip.
  let uri = concat ["https://github.com/", repoPath, "/archive/",
                    unpack hash, ".tar.gz"]
  fetchHttp Nothing (fromJust $ parseURI uri)

resolveNpmVersionRange :: Name -> NpmVersionRange -> NpmFetcher SemVer
resolveNpmVersionRange name range = case range of
  SemVerRange svr -> resolveDepCached name svr
  NpmUri uri -> case uriScheme uri of
    "git:" -> fetchGithub uri
    "http:" -> fetchHttp (Just "package") uri
    "https:" -> fetchHttp  (Just "package") uri
    scheme -> cerror ["Unknown uri scheme ", scheme]
  GitId src owner repo rev -> case src of
    Github -> do
      let frag = case rev of
            Nothing -> ""
            Just r -> "#" <> r
      let uri = concat ["https://github.com/", owner, "/", repo, frag]
      fetchGithub $ fromJust $ parseURI $ unpack uri
    _ -> cerror ["Can't handle git source ", show src]
  vr -> cerror ["Don't know how to resolve dependency '", show vr, "'"]

-- | Uses the set of downloaded packages as a cache to avoid unnecessary
-- duplication.
resolveDepCached :: Name -> SemVerRange -> NpmFetcher SemVer
resolveDepCached name range = H.lookup name <$> gets resolved >>= \case
  Just versions -> case filter (matches range) (H.keys versions) of
    [] -> resolveDep name range -- No matching versions, need to fetch.
    vs -> return $ L.maximum vs
  Nothing -> resolveDep name range

startResolving :: Name -> SemVer -> NpmFetcher ()
startResolving name ver = modify $ \s ->
  s {currentlyResolving = HS.insert (name, ver) $ currentlyResolving s}

finishResolving :: Name -> SemVer -> NpmFetcher ()
finishResolving name ver = modify $ \s ->
  s {currentlyResolving = HS.delete (name, ver) $ currentlyResolving s}

resolveVersionInfo :: VersionInfo -> NpmFetcher SemVer
resolveVersionInfo versionInfo = do
  let name = viName versionInfo
      version = case parseSemVer $ viVersion versionInfo of
        Left err -> cerror ["Invalid semver in versionInfo object ",
                            unpack $ viVersion versionInfo,
                            " Due to: ", show err]
        Right v -> v
  putStrsLn ["Resolving ", name, " version ", renderSV version]
  HS.member (name, version) <$> gets currentlyResolving >>= \case
    True -> do putStrsLn ["Warning: cycle detected"]
               return version
    False -> do
      let recurOn deptype deps = fmap H.fromList $ do
            let depList = H.toList $ deps versionInfo
            putStrsLn ["Found ", deptype, ": ", pack (show depList)]
            forM depList $ \(depName, depRange) -> do
              putStrsLn ["Resolving ", name, " dependency ", depName]
              depVersion <- resolveNpmVersionRange depName depRange
              return (depName, depVersion)
      -- We need to recur into the package's dependencies.
      -- To prevent the cycles, we store which packages we're currently
      -- resolving.
      startResolving name version
      deps <- recurOn "Dependencies" viDependencies
      devDeps <- recurOn "Dev dependencies" viDevDependencies
      finishResolving name version
      let dist = case viDist versionInfo of
            Nothing -> error "Version information did not include dist"
            Just d -> d
      -- Store this version's info.
      addResolvedPkg name version $ ResolvedPkg {
          rpName = name,
          rpVersion = version,
          rpDistInfo = dist,
          rpDependencies = deps,
          rpDevDependencies = devDeps
        }
      return version

-- | Resolves a dependency given a name and version range.
resolveDep :: Name -> SemVerRange -> NpmFetcher SemVer
resolveDep name range = do
  let oops err = cerror ["When resolving dependency ", unpack name, " (",
                         show range, "): ", err]
  PackageInfo versions <- getPackageInfoCached name
  current <- gets currentlyResolving
  case bestMatchFromRecord range versions of
    Left err -> oops err
    Right (version, versionInfo) -> resolveVersionInfo versionInfo


startState :: Text -> Maybe Text -> NpmFetcherState
startState uriStr token = case parseURI $ unpack uriStr of
  Nothing -> cerror ["Invalid URI: ", unpack uriStr]
  Just uri -> NpmFetcherState {
      registry = uri,
      githubAuthToken = token,
      resolved = mempty,
      pkgInfos = mempty,
      currentlyResolving = mempty
    }

-- | Read NPM registry from env or use default.
getRegistry :: MonadIO m => m Text
getRegistry = do
  let npmreg = "https://registry.npmjs.org/"
  silentShell $ fromMaybe npmreg <$> get_env "NPM_REGISTRY"

-- | Read github auth token from env or use none.
getToken :: MonadIO m => m (Maybe Text)
getToken = silentShell $ get_env "GITHUB_TOKEN"

runIt :: NpmFetcher a -> IO (a, NpmFetcherState)
runIt x = do
  state <- startState <$> getRegistry <*> getToken
  runStateT x state

getPkg :: Name -> IO (Record (HashMap SemVer ResolvedPkg))
getPkg name = do
  let range = Gt (0, 0, 0)
  (_, finalState) <- runIt (resolveDep name range)
  return (resolved finalState)
