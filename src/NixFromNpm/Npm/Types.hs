{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module NixFromNpm.Npm.Types where

import qualified ClassyPrelude as CP
import Data.Aeson
import Data.Aeson.Types as Aeson (Parser, typeMismatch, withObject)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.SemVer (SemVer, SemVerRange)
import Data.SemVer (parseSemVer, parseSemVerRange, anyVersion)

import NixFromNpm.Common
import NixFromNpm.Git.Types (getObject, GithubError)
import NixFromNpm.Npm.Version
import NixFromNpm.Npm.PackageMap

-- | Package information; specifically all of the different versions.
data PackageInfo = PackageInfo {
  piVersions :: HashMap SemVer VersionInfo,
  piTags :: Record SemVer
  } deriving (Show, Eq)

-- | Taken from https://nodejs.org/api/process.html#process_process_platform,
-- and filtered to those that correspond to nixpkgs platforms.
data NodePlatform
  = Darwin
  | FreeBSD
  | OpenBSD
  | Linux
  | SunOS
  deriving (Show, Eq)

-- | Convert a NodePlatform into text
nodePlatformToText :: IsString t => NodePlatform -> t
nodePlatformToText = \case
  Darwin -> "darwin"
  FreeBSD -> "freebsd"
  OpenBSD -> "openbsd"
  Linux -> "linux"
  SunOS -> "solaris"

-- | Parse a node platform from a string.
parseNodePlatform :: Alternative f => Text -> f NodePlatform
parseNodePlatform = \case
  "linux" -> pure Linux
  "darwin" -> pure Darwin
  "freebsd" -> pure FreeBSD
  "openbsd" -> pure OpenBSD
  "sunos" -> pure SunOS
  _ -> empty

-- | Metadata about a package.
data PackageMeta = PackageMeta {
  pmDescription :: Maybe Text,
  pmAuthor :: Maybe Text,
  pmHomepage :: Maybe URI,
  pmKeywords :: Vector Text,
  pmPlatforms :: Vector NodePlatform
  } deriving (Show, Eq)

-- | Default (empty) package metadata.
emptyPackageMeta :: PackageMeta
emptyPackageMeta = PackageMeta Nothing Nothing Nothing mempty mempty

instance FromJSON PackageMeta where
  parseJSON = withObject "PackageMeta" $ \o -> do
    let getString = \case {String s -> Just s; _ -> Nothing}
    description <- o .:? "description"
    author <- o .:? "author" <|> pure Nothing
    maybePlatforms :: Maybe (Vector Text) <- o .:? "os" <|> pure Nothing
    let platforms = maybe mempty (catMaybes . map parseNodePlatform) maybePlatforms
    homepage <- o .:? "homepage" >>= \case
      Nothing -> return Nothing
      Just (String txt) -> return $ parseURIText txt
      Just (Array stuff) -> case toList $ catMaybes (getString <$> stuff) of
        [] -> return Nothing
        (uri:_) -> return $ parseURIText uri
    let
      -- If keywords are a string, split on commas and strip whitespace.
      getKeywords (String s) = fromList $ T.strip <$> T.split (==',') s
      -- If an array, just take the array.
      getKeywords (Array a) = catMaybes $ map getString a
      -- Otherwise, this is an error, but just return an empty array.
      getKeywords _ = mempty
    keywords <- map getKeywords $ o .:? "keywords" .!= Null
    return $ PackageMeta description author homepage keywords platforms


-- | Expresses all of the information that a version of a package needs, in
-- the abstract (e.g. using version ranges instead of explicit versions).
-- This type can be used as an input to the Npm.Resolve stuff to produce a
-- `ResolvedPkg`.
data VersionInfo = VersionInfo {
  viName :: PackageName,
  viDependencies :: PRecord NpmVersionRange,
  viOptionalDependencies :: PRecord NpmVersionRange,
  viDevDependencies :: PRecord NpmVersionRange,
  viBundledDependencies :: [PackageName],
  viDist :: Maybe DistInfo, -- not present if in a package.json file.
  viMeta :: PackageMeta,
  viVersion :: SemVer
  } deriving (Show, Eq)

-- | SHA digest, combining an algorithm type with a digest.
data Shasum = SHA1 Text | SHA256 Text deriving (Show, Eq)

-- | Distribution info from NPM. Tells us the URL and hash of a tarball.
data DistInfo = DistInfo {
  diUrl :: Text,
  diShasum :: Shasum
  } deriving (Show, Eq)

-- | Flag for different types of dependencies.
data DependencyType
  = Dependency    -- ^ Required at runtime.
  | OptionalDependency
  | DevDependency -- ^ Only required for development.
  deriving (Show, Eq)

-- | Dependencies might be circular; this type lets us indicate if so.
data PossiblyCircularSemVer
  = NotCircular SemVer
  | Circular CircularSemVer
  deriving (Show, Eq, Ord, Generic)

instance Hashable PossiblyCircularSemVer

-- | Wrapper for SemVers so we can tell when they're circular.
newtype CircularSemVer = CircularSemVer {unCirc :: SemVer}
  deriving (Show, Eq, Ord, Generic)

instance Hashable CircularSemVer

-- | Convert a PossiblyCircularSemVer to a SemVer.
unpackPSC :: PossiblyCircularSemVer -> SemVer
unpackPSC (Circular (CircularSemVer sv)) = sv
unpackPSC (NotCircular sv) = sv

-- | Separate a list of PossiblyCircularSemVers into two lists.
-- The first element contains non-circular dependencies, and the
-- second contains circular ones.
sepCirculars :: [PossiblyCircularSemVer] -> ([SemVer], [CircularSemVer])
sepCirculars [] = ([], [])
sepCirculars (psc:rest) = do
  let (noncirculars, circulars) = sepCirculars rest
  case psc of
    NotCircular nc -> (nc:noncirculars, circulars)
    Circular c -> (noncirculars, c:circulars)

-- | Similar to @sepCirculars@ but takes in a HashMap and returns a
-- pair of HashMaps.
sepCircularMap :: (Hashable a, Eq a)
               => HashMap a PossiblyCircularSemVer
               -> (HashMap a SemVer, HashMap a CircularSemVer)
sepCircularMap m = go $ H.toList m where
  go [] = (mempty, mempty)
  go ((key, psc):rest) = do
    let (noncirculars, circulars) = go rest
    case psc of
      NotCircular nc -> (H.insert key nc noncirculars, circulars)
      Circular c -> (noncirculars, H.insert key c circulars)

-- | Reasons why an expression might not have been able to be built.
data BrokenPackageReason
  = NoMatchingPackage PackageName
  | NoMatchingVersion NpmVersionRange
  | InvalidNpmVersionRange Text
  | NoSuchTag Name
  | TagPointsToInvalidVersion Name SemVer
  | InvalidSemVerSyntax Text String
  | InvalidPackageJson Text String
  | NoDistributionInfo
  | Reason String
  | GithubError GithubError
  | NotYetImplemented String
  | UnsatisfiedDependency PackageName -- This should never happen, but in case
  | BrokenDependency PackageName BrokenPackageReason
  deriving (Show, Eq, Typeable)

instance Exception BrokenPackageReason

-- | We might not be able to resolve a dependency, in which case we record
-- it as a broken package.
data ResolvedDependency
  = Resolved PossiblyCircularSemVer -- ^ Package resolved at this version.
  | Broken BrokenPackageReason -- ^ Could not build the dependency.
  deriving (Show, Eq)

instance Semigroup PackageInfo where
  PackageInfo vs ts <> PackageInfo vs' ts' =
    PackageInfo (vs CP.<> vs') (ts CP.<> ts')

instance Monoid PackageInfo where
  mempty = PackageInfo mempty mempty
  mappend = (CP.<>)

instance FromJSON PackageName where
  parseJSON (String name) = case parsePackageName name of
    Left err -> fail $ unpack err
    Right pname -> return pname
  parseJSON v = typeMismatch "Expected a string for a package name" v

-- | Gets a hashmap from an object, or otherwise returns an empty hashmap.
getDict :: (FromJSON val, FromJSON key, Hashable key, Eq key)
        => Text -> Object -> Aeson.Parser (HashMap key val)
getDict key obj = case H.lookup key obj of
  Just (Object obj') -> map H.fromList $
    forM (H.toList obj') $ \(k, v) -> do
      key <- parseJSON (String k)
      val <- parseJSON v
      return (key, val)
  -- sometimes it's malformed, like humanize-number
  _ -> return mempty

instance FromJSON VersionInfo where
  parseJSON = withObject "version info" $ \o -> do
    listedDependencies <- getDict "dependencies" o
    devDependencies <- getDict "devDependencies" o
    optionalDependencies <- getDict "optionalDependencies" o
    bundledDependencies <- o .:? "bundledDependencies" .!= []
    -- Loop through the bundled dependencies. If any of them are missing
    -- from the dependencies record, add it here.
    let isMissingDep name = not $ H.member name listedDependencies
        missing = filter isMissingDep bundledDependencies
        missingDependencies = zip missing (repeat $ SemVerRange anyVersion)
        dependencies = listedDependencies <> H.fromList missingDependencies
    dist <- o .:? "dist"
    pkgName <- o .: "name"
    version <- o .: "version"
    packageMeta <- parseJSON (Object o)
    scripts :: Record Value <- getDict "scripts" o <|> fail "couldn't get scripts"
    -- Remove any keys which appear in `optionalDependencies` from
    -- the dependencies and devdependencies sets.
    let rmOptionals = flip H.difference optionalDependencies
    case parseSemVer version of
      Left _ -> throw $ VersionSyntaxError version
      Right semver -> return $ VersionInfo {
        viDependencies = rmOptionals dependencies,
        viDevDependencies = rmOptionals devDependencies,
        viOptionalDependencies = optionalDependencies,
        viBundledDependencies = bundledDependencies,
        viDist = dist,
        viName = pkgName,
        viMeta = packageMeta,
        viVersion = semver
      }

instance FromJSON SemVerRange where
  parseJSON v = case v of
    String s -> case parseSemVerRange s of
      Left err -> do
        let errorMessage = "valid semantic version (got " <> show v <> ")"
        typeMismatch errorMessage v
      Right range -> return range
    _ -> typeMismatch "string" v

instance FromJSON PackageInfo where
  parseJSON = getObject "package info" >=> \o -> do
    vs' :: Record VersionInfo <- getDict "versions" o
    tags' <- getDict "dist-tags" o
    let vs = H.fromList $ map (\vi -> (viVersion vi, vi)) $ H.elems vs'
        convert tags [] = return $ PackageInfo vs (H.fromList tags)
        convert tags ((tName, tVer):ts) = case parseSemVer tVer of
          Left err -> failC ["Tag ", tName, " refers to an invalid ",
                             "semver string ", tVer, ": ", tshow err]
          Right ver -> convert ((tName, ver):tags) ts
    convert [] $ H.toList tags'

instance FromJSON DistInfo where
  parseJSON = getObject "dist info" >=> \o -> do
    tarball <- o .: "tarball"
    shasum <- SHA1 <$> o .: "shasum"
    return $ DistInfo tarball shasum
