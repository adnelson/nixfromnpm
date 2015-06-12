{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixFromNpm.NpmLookup where

--------------------------------------------------------------------------
import ClassyPrelude
import qualified Prelude as P
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams hiding (mapM, map, filter)
import System.IO.Streams.Attoparsec
import System.IO.Streams.HTTP
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Text.Parsec as Parsec

import NixFromNpm.Common
import NixFromNpm.SemVer
import NixFromNpm.Parsers.Common hiding (Parser, Error)
import NixFromNpm.Parsers.SemVer
import NixFromNpm.Parsers.NpmVersion
--------------------------------------------------------------------------

newtype PackageInfo = PackageInfo {
  piVersions :: Record VersionInfo
} deriving (Show, Eq)

data VersionInfo = VersionInfo {
  viDependencies :: Record NpmVersionRange,
  viDevDependencies :: Record NpmVersionRange,
  viDist :: DistInfo,
  viMain :: Maybe Text,
  viName :: Text,
  viVersion :: Text
} deriving (Show, Eq)

-- | Distribution info from NPM. Tells us the URL and hash of a tarball.
data DistInfo = DistInfo {
  tiUrl :: Text,
  tiShasum :: Text
} deriving (Show, Eq)

data ResolvedPkg = ResolvedPkg {
  rpName :: Name,
  rpVersion :: SemVer,
  rpDistInfo :: DistInfo,
  rpDependencies :: Record SemVer,
  rpDevDependencies :: Record SemVer
} deriving (Show, Eq)

data MyState = MyState {
  registry :: URI,
  resolved :: Record (HashMap SemVer ResolvedPkg),
  pkgInfos :: Record PackageInfo,
  -- For cycle detection.
  currentlyResolving :: HashSet (Name, SemVer)
} deriving (Show, Eq)

type MyStateIO = StateT MyState IO


instance FromJSON DistInfo where
  parseJSON = getObject "dist info" >=> \o -> do
    tarball <- o .: "tarball"
    shasum <- o .: "shasum"
    return $ DistInfo tarball shasum

-- | Gets a hashmap from an object, or otherwise returns an empty hashmap.
getDict :: (FromJSON a) => Text -> Object -> Parser (HashMap Text a)
getDict key o = mapM parseJSON =<< (o .:? key .!= mempty)

getObject :: String -> Value -> Parser (HashMap Text Value)
getObject _ (Object o) = return o
getObject msg v =
  typeMismatch ("object (got " <> show v <> ", message " <> msg <> ")") v

instance FromJSON VersionInfo where
  parseJSON = getObject "version info" >=> \o -> do
    dependencies <- getDict "dependencies" o
    devDependencies <- getDict "devDependencies" o
    dist <- o .: "dist"
    name <- o .: "name"
    main <- o .:? "main"
    version <- o .: "version"
    return $ VersionInfo {
      viDependencies = dependencies,
      viDevDependencies = devDependencies,
      viDist = dist,
      viMain = main,
      viName = name,
      viVersion = version
    }

instance FromJSON SemVerRange where
  parseJSON v = case v of
    String s -> case parseSemVerRange s of
      Left err -> typeMismatch ("valid semantic version (got " <> show v <> ")") v
      Right range -> return range
    _ -> typeMismatch "string" v

instance FromJSON PackageInfo where
  parseJSON = getObject "package info" >=> \o -> do
    vs <- getDict "versions" o
    return $ PackageInfo vs

addResolvedPkg :: Name -> SemVer -> ResolvedPkg -> MyStateIO ()
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
    req <- parseUrl $ uriToString id uri ""
    withManager (opensslManagerSettings context) $ \mgr ->
      withHTTP req mgr $ \response -> do
        let body = responseBody response
        jsonBlob <- parseFromStream json body
        return $ fromJSON jsonBlob
  case j of
    Error e -> putStrLn "" >> mkErr ("Couldn't parse JSON: " <> e)
    Success s -> putStrLn "OK" >> return s

-- | Same as getPackageInfo, but caches results for speed.
getPackageInfoCached :: Name -> MyStateIO PackageInfo
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

fetchGit = undefined
fetchHttp = undefined
fetchHttps = undefined

resolveNpmVersionRange :: Name -> NpmVersionRange -> MyStateIO SemVer
resolveNpmVersionRange name range = case range of
  SemVerRange svr -> resolveDepCached name svr
  NpmUri uri -> case uriScheme uri of
    "git:" -> fetchGit uri
    "http:" -> fetchHttp uri
    "https:" -> fetchHttps uri
    scheme -> cerror ["Invalid uri scheme ", scheme]
  vr -> cerror ["Don't know how to resolve dependency '", show vr, "'"]

-- | Uses the set of downloaded packages as a cache to avoid unnecessary
-- duplication.
resolveDepCached :: Name -> SemVerRange -> MyStateIO SemVer
resolveDepCached name range = H.lookup name <$> gets resolved >>= \case
  Just versions -> case filter (matches range) (H.keys versions) of
    [] -> resolveDep name range -- No matching versions, need to fetch.
    vs -> return $ L.maximum vs
  Nothing -> resolveDep name range

startResolving :: Name -> SemVer -> MyStateIO ()
startResolving name ver = modify $ \s ->
  s {currentlyResolving = HS.insert (name, ver) $ currentlyResolving s}

finishResolving :: Name -> SemVer -> MyStateIO ()
finishResolving name ver = modify $ \s ->
  s {currentlyResolving = HS.delete (name, ver) $ currentlyResolving s}

-- | Resolves a dependency given a name and version range.
resolveDep :: Name -> SemVerRange -> MyStateIO SemVer
resolveDep name range = do
  let oops err = cerror ["When resolving dependency ", unpack name, " (",
                         show range, "): ", err]
  PackageInfo versions <- getPackageInfoCached name
  current <- gets currentlyResolving
  case bestMatchFromRecord range versions of
    Left err -> oops err
    Right (version, versionInfo) -> do
      putStrsLn ["Resolving version ", renderSV version]
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
          let rpkg = ResolvedPkg {
                rpName = name,
                rpVersion = version,
                rpDistInfo = viDist versionInfo,
                rpDependencies = deps,
                rpDevDependencies = mempty
              }
          -- Store this version's dist info
          addResolvedPkg name version rpkg
          return version

startState :: String -> MyState
startState uriStr = case parseURI uriStr of
  Nothing -> cerror ["Invalid URI: ", uriStr]
  Just uri -> MyState {
      registry = uri,
      resolved = mempty,
      pkgInfos = mempty,
      currentlyResolving = mempty
    }

getPkg :: Name -> IO (Record (HashMap SemVer ResolvedPkg))
getPkg name = do
  let npmreg = "https://registry.npmjs.org/"
      range = Gt (0, 0, 0)
  (_, finalState) <- runStateT (resolveDep name range) (startState npmreg)
  return (resolved finalState)
