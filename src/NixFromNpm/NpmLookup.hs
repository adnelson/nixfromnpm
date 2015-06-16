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
  currentlyResolving :: HashSet (Name, SemVer),
  indentLevel :: Int,
  knownProblematicPackages :: HashSet Name,
  getDevDeps :: Bool
} deriving (Show, Eq)

type NpmFetcher = ExceptT ErrorList (StateT NpmFetcherState IO)

indent :: Text -> NpmFetcher Text
indent txt = do
  ind <- gets indentLevel
  return $ concat [txt, " (depth of ", pack $ show ind, ")"]

putStrLnI :: Text -> NpmFetcher ()
putStrLnI = indent >=> putStrLn

putStrsLnI :: [Text] -> NpmFetcher ()
putStrsLnI = putStrLnI . concat

putStrI :: Text -> NpmFetcher ()
putStrI = indent >=> putStr

putStrsI :: [Text] -> NpmFetcher ()
putStrsI = putStrI . concat

addResolvedPkg :: Name -> SemVer -> ResolvedPkg -> NpmFetcher ()
addResolvedPkg name version rpkg = do
  pkgSet <- H.lookupDefault mempty name <$> gets resolved
  modify $ \s -> s {
    resolved = H.insert name (H.insert version rpkg pkgSet) (resolved s)
    }



-- | Queries NPM for package information.
_getPackageInfo :: URI -> Name -> NpmFetcher PackageInfo
_getPackageInfo registryUri pkgName = do
  let uri = uriToText $ registryUri `slash` pkgName
  putStrsLn ["Querying NPM for package ", pkgName, "..."]
  putStrsLn ["Curling uri: ", uri]
  jsonStr <- silentShell $ run "curl" [uri]
  case eitherDecode $ BL8.fromChunks [T.encodeUtf8 jsonStr] of
    Left err -> throwErrorC ["couldn't parse JSON from NPM: ", pack err]
    Right info -> return info

-- | Same as _getPackageInfo, but caches results for speed.
getPackageInfo :: Name -> NpmFetcher PackageInfo
getPackageInfo name = lookup name . pkgInfos <$> get >>= \case
  Just info -> return info
  Nothing -> inFrontContext ctx $ do
    reg <- gets registry
    info <- _getPackageInfo reg name
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
toSemVerList rec = do
  -- Pairings of parsed semvers (or errors) to values.
  let parsePair (k, v) = (parseSemVer k, v)
      pairs = map parsePair $ H.toList rec
  case filter (\(k, _) -> isRight k) pairs of
    [] -> throwError1 "No correctly-formatted versions strings found"
    okPairs -> return $ map (\(Right k, v) -> (k, v)) okPairs

bestMatchFromRecord :: SemVerRange -> Record a -> NpmFetcher a
bestMatchFromRecord range rec = do
  pairs <- toSemVerList rec
  case filter (matches range . fst) pairs of
    [] -> throwError1 "No versions satisfy given range"
    matches -> return $ snd $ maximumBy (compare `on` fst) matches

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

-- | Returns the SHA1 hash of the result of fetching the URI, and the path
-- in which the tarball is stored.
nixPrefetchSha1 :: URI -> NpmFetcher (Text, FilePath)
nixPrefetchSha1 uri = do
  hashAndPath <- silentShell $ do
    setenv "PRINT_PATH" "1"
    run "nix-prefetch-url" ["--type", "sha1", uriToText uri]
  if length (lines hashAndPath) /= 2
  then error "Expected two lines from nix-prefetch-url"
  else do
    let [hashBase32, path] = lines hashAndPath
    -- Convert the hash to base16, which is the format NPM uses.
    hash <- silentShell $ do
       run "nix-hash" ["--type", "sha1", "--to-base16", hashBase32]
    return (T.strip hash, fromString $ unpack path)

extractVersionInfo :: FilePath -> Text -> NpmFetcher VersionInfo
extractVersionInfo tarballPath subpath = do
  ind <- gets indentLevel
  pkJson <- silentShell $ withTmpDir $ \dir -> do
    chdir dir $ do
      putStrs ["Extracting ", pathToText tarballPath, " to tempdir"]
      putStrsLn [" (depth of ", pack $ show ind, ")"]
      run_ "tar" ["-xf", pathToText tarballPath]
      curdir <- pathToText <$> pwd
      pth <- fmap T.strip $ run "find" [curdir, "-name", "package.json"]
                           -|- run "head" ["-n", "1"]
      when (pth == "") $ error "No package.json found"
      map decodeUtf8 $ readBinary $ fromString $ unpack $ pth
  case eitherDecode $ BL8.fromChunks [encodeUtf8 pkJson] of
    Left err -> error $ "couldn't parse JSON as VersionInfo: " <> err
    Right info -> return info

-- | Fetch a package over HTTP. Return the version of the fetched package,
-- and store the hash.
fetchHttp :: Text -- | Subpath in which to find the package.json.
          -> URI -- | The URI to fetch.
          -> NpmFetcher SemVer -- | The version of the package at that URI.
fetchHttp subpath uri = do
  -- Use nix-fetch to download and hash the tarball.
  (hash, tarballPath) <- nixPrefetchSha1 uri
  -- Extract the tarball to a temp directory and parse the package.json.
  versionInfo <- extractVersionInfo tarballPath subpath
  -- Create the DistInfo.
  let dist = DistInfo {diUrl = uriToText uri, diShasum = hash}
  -- Add the dist information to the version info and resolve it.
  resolveVersionInfo $ versionInfo {viDist = Just dist}

githubCurl :: Text -> NpmFetcher Value
githubCurl uri = do
  extraCurlArgs <- gets githubAuthToken >>= \case
    Nothing -> return []
    Just token -> return ["-H", "Authorization: token " <> token]
  let curlArgs = extraCurlArgs <> [
        "--fail", "-L",
        -- This accept header tells github to allow redirects.
        "-H", "Accept: application/vnd.github.quicksilver-preview+json",
        uri
        ]
  -- putStrsLn $ ["calling curl with args: ", T.intercalate " " curlArgs]
  jsonStr <- silentShell $ run "curl" curlArgs
  case eitherDecode $ BL8.fromChunks [T.encodeUtf8 jsonStr] of
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

getShaOfBranch :: Name -- | Repo owner
               -> Name -- | Repo name
               -> Name -- | Name of the branch to get
               -> NpmFetcher Text -- | The hash of the default branch
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
  Just versions -> case filter (matches range) (H.keys versions) of
    [] -> _resolveDep name range -- No matching versions, need to fetch.
    vs -> return $ maximum vs
  Nothing -> _resolveDep name range

startResolving :: Name -> SemVer -> NpmFetcher ()
startResolving name ver = modify $ \s ->
  s {currentlyResolving = HS.insert (name, ver) $ currentlyResolving s}

finishResolving :: Name -> SemVer -> NpmFetcher ()
finishResolving name ver = modify $ \s ->
  s {currentlyResolving = HS.delete (name, ver) $ currentlyResolving s}

resolveVersionInfo :: VersionInfo -> NpmFetcher SemVer
resolveVersionInfo versionInfo = do
  let name = viName versionInfo
      version = viVersion versionInfo
      ctx = concat ["When resolving package ", name, ", version ", version]
  inFrontContext ctx $ do
    version <- case parseSemVer $ viVersion versionInfo of
      Left err -> throwErrorC ["Invalid semver in versionInfo object ",
                                   viVersion versionInfo,
                                   " Due to: ", pack $ show err]
      Right v -> return v
    putStrsLn ["Resolving ", name, " version ", renderSV version]
    HS.member (name, version) <$> gets currentlyResolving >>= \case
      True -> do putStrsLn ["Warning: cycle detected"]
                 return version
      False -> do
        let recurOn deptype deps = map H.fromList $ do
              let depList = H.toList $ deps versionInfo
              when (length depList > 0) $
                putStrsLn ["Found ", deptype, ": ", pack (show depList)]
              res <- map catMaybes $ forM depList $ \(depName, depRange) -> do
                HS.member depName <$> gets knownProblematicPackages >>= \case
                  True -> do
                    putStrsLn ["WARNING: ", name, " is a broken package"]
                    return Nothing
                  False -> do
                    putStrsLn ["Resolving ", name, " dependency ", depName]
                    depVersion <- resolveNpmVersionRange depName depRange
                    return $ Just (depName, depVersion)
              return res
        -- We need to recur into the package's dependencies.
        -- To prevent the cycles, we store which packages we're currently
        -- resolving.
        startResolving name version
        deps <- recurOn "Dependencies" viDependencies
        devDeps <- gets getDevDeps >>= \case
          True -> recurOn "Dev dependencies" viDevDependencies
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
            rpDependencies = deps,
            rpDevDependencies = devDeps
          }
        return version

-- | Resolves a dependency given a name and version range.
_resolveDep :: Name -> SemVerRange -> NpmFetcher SemVer
_resolveDep name range = do
  let ctx = concat ["When resolving dependency ", name, " (",
                    pack $ show range, ")"]
  inFrontContext ctx $ do
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

startState :: Text -> Maybe Text -> NpmFetcherState
startState uriStr token = case parseURI $ unpack uriStr of
  Nothing -> errorC ["Invalid URI: ", uriStr]
  Just uri -> NpmFetcherState {
      registry = uri,
      githubAuthToken = token,
      resolved = mempty,
      pkgInfos = mempty,
      currentlyResolving = mempty,
      knownProblematicPackages = HS.fromList ["websocket-server"],
      indentLevel = 0,
      getDevDeps = False
    }

-- | Read NPM registry from env or use default.
getRegistry :: IO Text
getRegistry = do
  let npmreg = "https://registry.npmjs.org/"
  shelly $ silently $ fromMaybe npmreg <$> get_env "NPM_REGISTRY"

-- | Read github auth token from env or use none.
getToken :: IO (Maybe Text)
getToken = shelly $ silently $ get_env "GITHUB_TOKEN"

runIt :: NpmFetcher a -> IO (a, NpmFetcherState)
runIt x = do
  state <- startState <$> getRegistry <*> getToken
  runStateT (runExceptT x) state >>= \case
    (Left elist, _) -> error $ "\n" <> (unpack $ render elist)
    (Right x, state) -> return (x, state)

getPkg :: Name -> IO (Record (HashMap SemVer ResolvedPkg))
getPkg name = do
  let range = Gt (0, 0, 0)
  (_, finalState) <- runIt (_resolveDep name range)
  return (resolved finalState)
