{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NixFromNpm.NpmTypes (
    module NixFromNpm.NpmVersion,
    PackageInfo(..), PackageMeta(..), VersionInfo(..),
    DistInfo(..), ResolvedPkg(..), DependencyType(..),
    BrokenPackageReason(..), ResolvedDependency(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import NixFromNpm.Common
import NixFromNpm.SemVer
import NixFromNpm.NpmVersion
import NixFromNpm.Parsers.NpmVersion
import NixFromNpm.Parsers.SemVer

-- | Package information; specifically all of the different versions.
data PackageInfo = PackageInfo {
  piVersions :: Record VersionInfo,
  piTags :: Record Name
  } deriving (Show, Eq)

-- | Metadata about a package.
data PackageMeta = PackageMeta {
  pmDescription :: Maybe Text
  } deriving (Show, Eq)

-- | Expresses all of the information that a version of a package needs, in
-- the abstract (e.g. using version ranges instead of explicit versions).
-- This type can be used as an input to the NpmLookup stuff to produce a
-- `ResolvedPkg`.
data VersionInfo = VersionInfo {
  viDependencies :: Record NpmVersionRange,
  viDevDependencies :: Record NpmVersionRange,
  viDist :: Maybe DistInfo, -- not present if in a package.json file.
  viMain :: Maybe Text,
  viName :: Text,
  viHasTest :: Bool,
  viMeta :: PackageMeta,
  viVersion :: Text
  } deriving (Show, Eq)

-- | Distribution info from NPM. Tells us the URL and hash of a tarball.
data DistInfo = DistInfo {
  diUrl :: Text,
  diShasum :: Text
  } deriving (Show, Eq)

-- | This contains the same information as the .nix file that corresponds
-- to the package. More or less it tells us everything that we need to build
-- the package.
data ResolvedPkg = ResolvedPkg {
  rpName :: Name,
  rpVersion :: SemVer,
  rpDistInfo :: DistInfo,
  rpMeta :: PackageMeta,
  rpDependencies :: Record ResolvedDependency,
  rpDevDependencies :: Record ResolvedDependency
  } deriving (Show, Eq)

-- | Flag for different types of dependencies.
data DependencyType
  = Dependency    -- ^ Required at runtime.
  | DevDependency -- ^ Only required for development.
  deriving (Show, Eq)

-- | Reasons why an expression might not have been able to be built.
data BrokenPackageReason
  = NoMatchingVersion NpmVersionRange
  | InvalidNpmVersionRange Text
  | NoSuchTag Name
  | TagPointsToInvalidVersion Name Name
  | InvalidSemVerSyntax Text String
  | NoDistributionInfo
  deriving (Show, Eq)

-- | We might not be able to resolve a dependency, in which case we record
-- it as a broken package.
data ResolvedDependency
  = Resolved SemVer -- ^ Package has been resolved at this version.
  | Broken BrokenPackageReason -- ^ Could not build the dependency.
  deriving (Show, Eq)

instance Semigroup PackageInfo where
  PackageInfo vs ts <> PackageInfo vs' ts' =
    PackageInfo (vs <> vs') (ts <> ts')

instance Monoid PackageInfo where
  mempty = PackageInfo mempty mempty
  mappend = (<>)

instance FromJSON VersionInfo where
  parseJSON = getObject "version info" >=> \o -> do
    dependencies <- getDict "dependencies" o
    devDependencies <- getDict "devDependencies" o
    dist <- o .:? "dist"
    name <- o .: "name"
    main <- o .:? "main"
    version <- o .: "version"
    packageMeta <- fmap PackageMeta $ o .:? "description"
    scripts :: Record Value <- getDict "scripts" o
    return $ VersionInfo {
      viDependencies = dependencies,
      viDevDependencies = devDependencies,
      viDist = dist,
      viMain = main,
      viName = name,
      viHasTest = H.member "test" scripts,
      viMeta = packageMeta,
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
    tags <- getDict "dist-tags" o
    return $ PackageInfo vs tags

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


patchIfMatches :: (a -> Bool) -- ^ Predicate function
               -> (a -> a) -- ^ Modification function
               -> a -- ^ Input object
               -> a -- ^ Patched object
patchIfMatches pred mod input | pred input = mod input
                              | otherwise  = input
