{-# LANGUAGE NoImplicitPrelude #-}
module NixFromNpm.PackageMap where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import Data.SemVer
import NixFromNpm.Common

-- | We use this data structure a lot: a mapping of package names to
-- a mapping of versions to fully defined packages.
type PackageMap pkg = Record (HashMap SemVer pkg)

-- | Same thing, but the keys don't map to anything.
type PackageSet = PackageMap ()

-- | Map a function across a PackageMap.
pmMap :: (a -> b) -> PackageMap a -> PackageMap b
pmMap f = H.map (H.map f)

-- | Insert a value into a package map under a given name and version.
pmInsert :: Name -> SemVer -> a -> PackageMap a -> PackageMap a
pmInsert name version val pmap = do
  let existing = H.lookupDefault mempty name pmap
  H.insert name (H.insert version val existing) pmap

-- | Insert into a PackageSet, same as inserting () into a map.
psInsert :: Name -> SemVer -> PackageSet -> PackageSet
psInsert name version = pmInsert name version ()

-- | Create a singleton package map.
pmSingleton :: Name -> SemVer -> a -> PackageMap a
pmSingleton name version x = H.singleton name (H.singleton version x)

-- | Create a singleton package set.
psSingleton :: Name -> SemVer -> PackageSet
psSingleton name version = pmSingleton name version ()

-- | Make a list of package pairs.
psToList :: PackageSet -> [(Name, SemVer)]
psToList packageSet = do
  let keysOnly :: Record [SemVer]
      keysOnly = H.map H.keys packageSet
      toPairs :: (Name, [SemVer]) -> [(Name, SemVer)]
      toPairs (name, versions) = zip (repeat name) versions
  concatMap toPairs $ H.toList keysOnly

-- | Remove a value from a package map under the given name and version.
pmDelete :: Name -> SemVer -> PackageMap a -> PackageMap a
pmDelete name version pmap = case H.lookup name pmap of
  -- If it's not in the map, we don't have to do anything
  Nothing -> pmap
  Just vmap -> case H.delete version vmap of
    -- If the map is empty after we remove the key, we can remove the map
    -- entirely. Otherwise, just remove the key.
    vmap' | H.null vmap' -> H.delete name pmap
          | otherwise -> H.insert name vmap' pmap

-- | Check for membership of a package name and version
pmMember :: Name -> SemVer -> PackageMap a -> Bool
pmMember name version pmap = case H.lookup name pmap of
  Nothing -> False
  Just vmap -> H.member version vmap

pmFromList :: [(Name, SemVer, a)] -> PackageMap a
pmFromList = foldl' step mempty where
  step pmap (name, ver, x) = pmInsert name ver x pmap

pmLookup :: Name -> SemVer -> PackageMap a -> Maybe a
pmLookup name version pmap = case H.lookup name pmap of
  Nothing -> Nothing
  Just vmap -> H.lookup version vmap

pmDiff :: PackageMap a -> PackageMap b -> PackageMap a
pmDiff pmap1 pmap2 = foldl' step pmap1 $ H.toList pmap2 where
  step result (pName, verMap) = case H.lookup pName result of
    Nothing -> result
    Just verMap' -> case H.difference verMap' verMap of
      m | H.null m -> H.delete pName result
        | otherwise -> H.insert pName m result
