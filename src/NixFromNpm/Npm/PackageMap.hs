{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module NixFromNpm.Npm.PackageMap where

import Data.HashMap.Strict (HashMap)
import Text.Regex.PCRE.Heavy (re, scan)
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Data.SemVer (SemVer)
import NixFromNpm.Common

-- | The name of a package, which optionally includes a namespace.
data PackageName = PackageName {
  pnBasicName :: !Name,
  pnNamespace :: !(Maybe Name)
  } deriving (Eq, Ord)

instance IsString PackageName where
  fromString s = PackageName (pack s) Nothing

instance Show PackageName where
  show (PackageName name Nothing) = unpack name
  show (PackageName name (Just namespace)) =
    concat ["@", unpack namespace, "/", unpack name]

instance Hashable PackageName where
  hashWithSalt salt (PackageName name namespace) =
    hashWithSalt salt (name, namespace)

-- | True if the package name has a namespace.
isNamespaced :: PackageName -> Bool
isNamespaced = isJust . pnNamespace

-- | Create a package name without a namespace.
simpleName :: Name -> PackageName
simpleName = flip PackageName Nothing

-- | Parse a PackageName from raw text.
--
-- This is more loose than the actual set of requirements for package
-- naming. All we require is that namespaces (if specified) and
-- package names have at least one character which is not whitespace
-- or a '@', '%' or '/' character.
--
-- See the complete rules at:
-- https://github.com/npm/validate-npm-package-name#naming-rules
--
parsePackageName :: Text -> Either Text PackageName
parsePackageName name = case scan [re|^(?:@([^\s@/%]+)/)?([^\s@/%]+)$|] name of
  [(_, ["", package])] -> pure $ PackageName package Nothing
  [(_, [namespace, package])] -> pure $ PackageName package (Just namespace)
  _ -> Left $ "Invalid package name " <> tshow name

-- | A record keyed on PackageNames.
type PRecord = HashMap PackageName

-- | We use this data structure a lot: a mapping of package names to
-- a mapping of versions to fully defined packages. We use a map for
-- the versions so that we can quickly get the latest or oldest version.
type PackageMap pkg = PRecord (M.Map SemVer pkg)

-- | Same thing, but the keys don't map to anything.
type PackageSet = PackageMap ()

-- | Map a function across a PackageMap.
pmMap :: (a -> b) -> PackageMap a -> PackageMap b
pmMap f = H.map (M.map f)

-- | Insert a value into a package map under a given name and version.
pmInsert :: PackageName -> SemVer -> a -> PackageMap a -> PackageMap a
pmInsert name version val pmap = do
  let existing = H.lookupDefault mempty name pmap
  H.insert name (M.insert version val existing) pmap

-- | Insert into a PackageSet, same as inserting () into a map.
psInsert :: PackageName -> SemVer -> PackageSet -> PackageSet
psInsert name version = pmInsert name version ()

-- | Create a singleton package map.
pmSingleton :: PackageName -> SemVer -> a -> PackageMap a
pmSingleton name version x = H.singleton name (M.singleton version x)

-- | Create a singleton package set.
psSingleton :: PackageName -> SemVer -> PackageSet
psSingleton name version = pmSingleton name version ()

-- | Make a list of package pairs.
psToList :: PackageMap a -> [(PackageName, SemVer)]
psToList packageSet = do
  let keysOnly = H.map M.keys packageSet
      toPairs (name, versions) = zip (repeat name) versions
  concatMap toPairs $ H.toList keysOnly

-- | Given a PackageSet, create a map of the packages to their versions.
psToMap :: PackageMap a -> Map PackageName (Map SemVer a)
psToMap pMap = foldl' step mempty $ H.toList pMap where
  step result (pkgName, versionSet) = M.insert pkgName versionSet result

-- | Remove a value from a package map under the given name and version.
pmDelete :: PackageName -> SemVer -> PackageMap a -> PackageMap a
pmDelete name version pmap = case H.lookup name pmap of
  -- If it's not in the map, we don't have to do anything
  Nothing -> pmap
  Just vmap -> case M.delete version vmap of
    -- If the map is empty after we remove the key, we can remove the map
    -- entirely. Otherwise, just remove the key.
    vmap' | M.null vmap' -> H.delete name pmap
          | otherwise -> H.insert name vmap' pmap

-- | Check for membership of a package name and version
pmMember :: PackageName -> SemVer -> PackageMap a -> Bool
pmMember name version pmap = case H.lookup name pmap of
  Nothing -> False
  Just vmap -> M.member version vmap

pmFromList :: [(PackageName, SemVer, a)] -> PackageMap a
pmFromList = foldl' step mempty where
  step pmap (name, ver, x) = pmInsert name ver x pmap

pmLookup :: PackageName -> SemVer -> PackageMap a -> Maybe a
pmLookup name version pmap = case H.lookup name pmap of
  Nothing -> Nothing
  Just vmap -> M.lookup version vmap

pmLookupDefault :: a -> PackageName -> SemVer -> PackageMap a -> a
pmLookupDefault def n v pmap = case pmLookup n v pmap of
  Nothing -> def
  Just x -> x

pmDiff :: PackageMap a -> PackageMap b -> PackageMap a
pmDiff pmap1 pmap2 = foldl' step pmap1 $ H.toList pmap2 where
  step result (pName, verMap) = case H.lookup pName result of
    Nothing -> result
    Just verMap' -> case M.difference verMap' verMap of
      m | M.null m -> H.delete pName result
        | otherwise -> H.insert pName m result

-- | Given a package map and a package name, find the latest version
-- of that package defined in the map, if it exists.
pmLatestVersion :: PackageName -> PackageMap a ->  Maybe SemVer
pmLatestVersion pName pMap = case H.lookup pName pMap of
  Nothing -> Nothing
  Just versions -> case M.toDescList versions of
    [] -> Nothing -- empty map
    (v, _):vs -> Just v

-- | Join two packagemaps.
pmJoin :: PackageMap a -> PackageMap a -> PackageMap a
pmJoin pm1 pm2 = foldl' addVersions pm1 $ H.toList pm2 where
  addVersions result (pkgName, pkgVersions) = case H.lookup pkgName result of
    -- If there's no package with this name in the result we can just add it.
    Nothing -> H.insert pkgName pkgVersions result
    -- If there already is, we join the versions.
    Just versions -> H.insert pkgName (versions <> pkgVersions) result

-- | Join a list of packagemaps.
pmConcat :: [PackageMap a] -> PackageMap a
pmConcat = foldl' pmJoin mempty

-- | The number of packages defined.
pmNumPackages :: PackageMap a -> Int
pmNumPackages = H.size

-- | The total number of versions of all packages defined.
pmNumVersions :: PackageMap a -> Int
pmNumVersions = sum . map M.size . map snd . H.toList
