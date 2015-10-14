{-# LANGUAGE NoImplicitPrelude #-}
module NixFromNpm.PackageMap where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

import NixFromNpm.Common
import NixFromNpm.SemVer

-- | We use this data structure a lot: a mapping of package names to
-- a mapping of versions to fully defined packages.
type PackageMap pkg = Record (HashMap SemVer pkg)

-- | Map a function across a PackageMap.
pmMap :: (a -> b) -> PackageMap a -> PackageMap b
pmMap f = H.map (H.map f)

-- | Insert a value into a package map under a given name and version.
pmInsert :: Name -> SemVer -> a -> PackageMap a -> PackageMap a
pmInsert name version val pmap = do
  let existing = H.lookupDefault mempty name pmap
  H.insert name (H.insert version val existing) pmap

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
