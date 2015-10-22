{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NixFromNpm.Conversion where

import qualified Prelude as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import NixFromNpm.Common
import Nix.Types
import Nix.Parser
import Nix.Pretty (prettyNix)
import NixFromNpm.ConvertToNix (toDotNix, mkTopDefaultNix, mkPkgJsonDefaultNix,
                                resolvedPkgToNix)
import NixFromNpm.Options
import NixFromNpm.NpmTypes
import NixFromNpm.SemVer
import NixFromNpm.Parsers.SemVer
import NixFromNpm.PackageMap (PackageMap, pmLookup, pmDelete, pmMap)
import NixFromNpm.NpmLookup (FullyDefinedPackage(..),
                             NpmFetcher(..),
                             NpmFetcherSettings(..),
                             NpmFetcherState(..),
                             addPackage,
                             toFullyDefined,
                             startState,
                             runNpmFetchWith,
                             resolveNpmVersionRange,
                             resolveVersionInfo,
                             extractPkgJson,
                             defaultSettings,
                             PreExistingPackage(..))

data ConversionError
  = NoPackagesGenerated
  | FolderDoesNotExist FilePath
  | FileDoesNotExist FilePath
  deriving (Show, Eq, Typeable)


-- | The npm lookup utilities will produce a bunch of fully defined packages.
-- However, the only packages that we want to write are the new ones; that
-- is, the ones that we've discovered and the ones that already exist. This
-- will perform the appropriate filter. It will also convert them to nix.
takeNewPackages :: PackageMap FullyDefinedPackage
                -> PackageMap NExpr
takeNewPackages startingRec = do
  let isNew (NewPackage rpkg) = Just $ resolvedPkgToNix rpkg
      isNew _ = Nothing
  H.filter (not . H.null) $ H.map (modifyMap isNew) startingRec

-- | Given the path to a package, finds all of the .nix files which parse
--   correctly.
parseVersion :: MonadIO io => Name -> FilePath -> io (Maybe (SemVer, NExpr))
parseVersion pkgName path = do
  let (versionTxt, ext) = splitExtension $ filename path
  case parseSemVer (pathToText versionTxt) of
    _ | ext /= Just "nix" -> return Nothing -- not a nix file
    Left _ -> return Nothing -- not a version file
    Right version -> parseNixString . pack <$> readFile path >>= \case
      Failure err -> do
        putStrsLn ["Warning: expression for ", pkgName, " version ",
                   pathToText versionTxt, " failed to parse:\n", pshow err]
        return Nothing -- invalid nix, should overwrite
      Success expr -> return $ Just (version, expr)

-- | Given the path to a file possibly containing nix expressions, finds all
--   expressions findable at that path and returns a map of them.
findExisting :: (MonadBaseControl IO io, MonadIO io)
             => Maybe Name -- ^ Is `Just` if this is an extension.
             -> FilePath       -- ^ The path to search.
             -> io (PackageMap PreExistingPackage) -- ^ Mapping of package
                                                   --   names to maps of
                                                   --   versions to nix
                                                   --   expressions.
findExisting maybeName path = do
  doesDirectoryExist path >>= \case
    False -> case maybeName of
               Just name -> errorC ["Extension ", pshow name, " at path ",
                                    pathToText path, " does not exist."]
               Nothing -> return mempty
    True -> withDir path $ do
      let wrapper :: NExpr -> PreExistingPackage
          wrapper = case maybeName of Nothing -> FromOutput
                                      Just name -> FromExtension name
      putStrsLn ["Searching for existing expressions in ", pathToText path,
                 "..."]
      contents <- getDirectoryContents path
      verMaps <- map catMaybes $ forM contents $ \dir -> do
        exprs <- doesDirectoryExist dir >>= \case
          True -> withDir dir $ do
            contents <- getDirectoryContents "."
            let files = filter (hasExt "nix") contents
            catMaybes <$> mapM (parseVersion $ pathToText dir) files
          False -> do
            return mempty -- not a directory
        case exprs of
          [] -> return Nothing
          vs -> return $ Just (pathToText dir, H.map wrapper $ H.fromList exprs)
      let total = sum $ map (H.size . snd) verMaps
      putStrsLn ["Found ", render total, " existing expressions:"]
      forM verMaps $ \(name, vers) -> do
        putStrs ["  ", name, ": "]
        putStrLn $ mapJoinBy ", " renderSV $ H.keys vers
      return $ H.fromList verMaps

-- | Given the output directory and any number of extensions to load,
-- finds any existing packages.
preloadPackages :: NpmFetcher () -- ^ Add the existing packages to the state.
preloadPackages = do
  existing <- asks nfsCacheDepth >>= \case
    n | n < 0 -> return mempty
      | otherwise -> findExisting Nothing =<< asks nfsOutputPath
  toExtend <- asks nfsExtendPaths
  libraries <- fmap concat $ forM (H.toList toExtend) $ \(name, path) -> do
    findExisting (Just name) path
  let all = existing <> libraries
  modify $ \s -> s {
    resolved = pmMap toFullyDefined all <> resolved s
    }

-- | Actually writes the packages to disk. Takes in the new packages to write,
--   and the names/paths to the libraries being extended.
dumpPkgs :: (MonadIO m, MonadBaseControl IO m)
         => FilePath               -- ^ Path to output directory.
         -> PackageMap NExpr -- ^ New packages being written.
         -> Record FilePath        -- ^ Libraries being extended.
         -> m ()   -- ^ Generated expressions.
dumpPkgs path newPackages extensions = do
  -- If there aren't any new packages, we can stop here.
  if H.null newPackages
    then putStrLn "No new packages created." >> return ()
    else do
      putStrsLn ["Creating new packages at ", pathToText path]
      createDirectoryIfMissing path
      withDir path $ do
        -- Write the .nix file for each version of this package.
        forM_ (H.toList newPackages) $ \(pkgName, pkgVers) -> do
          let subdir = path </> fromText pkgName
          createDirectoryIfMissing subdir
          withDir subdir $ do
            -- Write all of the versions we have generated.
            forM_ (H.toList pkgVers) $ \(ver, expr) -> do
              let fullPath = subdir </> toDotNix ver
              putStrsLn ["Writing package file at ", pathToText fullPath]
              writeFile (toDotNix ver) $ show $ prettyNix expr
            -- Remove the `latest.nix` symlink if it exists.
            whenM (doesFileExist "latest.nix") $ removeFile "latest.nix"
            -- Grab the latest version and create a symlink `latest.nix`
            -- to that.
            let convert fname =
                  case parseSemVer (T.dropEnd 4 $ pathToText fname) of
                    Left _ -> Nothing -- not a semver, so we don't consider it
                    Right ver -> Just ver -- return the version
            allVersions <- catMaybes . map convert <$> getDirectoryContents "."
            createSymbolicLink (toDotNix $ maximum allVersions) "latest.nix"
        -- Write the default.nix file for the library.
        -- We need to build up a record mapping package names to the list of
        -- versions being defined in this library.
        let defaultNix = mkTopDefaultNix extensions
        writeFile "default.nix" $ show $ prettyNix defaultNix

-- | Given a set of fetched packages, generates the expressions needed to
-- build that package and writes them to disk.
dumpPackages :: NpmFetcher ()       -- ^ Writes files to a folder.
dumpPackages = do
  path <- asks nfsOutputPath
  packages <- gets resolved
  let new = takeNewPackages packages
  dumpPkgs path new =<< asks nfsExtendPaths
  -- All of the packages we dumped are no longer new, so we can update them.
  forM_ (H.toList new) $ \(name, versionMap) ->
    forM (H.toList versionMap) $ \(version, expr) ->
       addPackage name version (FromExistingInOutput expr)


dumpFromPkgJson :: FilePath -- ^ Path to folder containing package.json.
                -> NpmFetcher ()
dumpFromPkgJson path = do
  doesDirectoryExist path >>= \case
    False -> errorC ["No such directory ", pathToText path]
    True -> withDir path $ do
      doesFileExist "package.json" >>= \case
        False -> errorC ["No package.json found in ", pathToText path]
        True -> do
          verinfo <- extractPkgJson "package.json"
          let (name, version) = (viName verinfo, viVersion verinfo)
          putStrsLn ["Generating expression for package ", name,
                     ", version ", renderSV version]
          resolveVersionInfo verinfo
          basePath <- pmLookup name version <$> gets resolved >>= \case
            Nothing -> errorC ["FATAL: could not build nix file"]
            Just (FromExistingInOutput _) -> asks nfsOutputPath
            Just (NewPackage _) -> asks nfsOutputPath
            Just (FromExistingInExtension extName _) -> do
              extendPaths <- asks nfsExtendPaths
              return (extendPaths H.! extName)
          let fullPath = basePath </> fromText name </> toDotNix version
              nixexpr = mkPkgJsonDefaultNix fullPath
          defNixPath <- map (</> "default.nix") getCurrentDirectory
          putStrsLn ["Writing package nix file at ", pathToText defNixPath]
          writeFile "default.nix" $ show $ prettyNix nixexpr

dumpPkgFromOptions :: NixFromNpmOptions -> IO ()
dumpPkgFromOptions (opts@NixFromNpmOptions{..}) = do
  let settings = defaultSettings {
    nfsGithubAuthToken = nfnoGithubToken,
    nfsRegistries = nfnoRegistries,
    nfsRequestTimeout = fromIntegral nfnoTimeout,
    nfsOutputPath = nfnoOutputPath,
    nfsExtendPaths = nfnoExtendPaths,
    nfsMaxDevDepth = nfnoDevDepth,
    nfsCacheDepth = nfnoCacheDepth
    }
  runNpmFetchWith settings startState $ do
    preloadPackages
    forM nfnoPkgNames $ \(name, range) -> do
      resolveNpmVersionRange name range
        `catch` \(e :: SomeException) -> do
          putStrsLn ["Failed to build ", name, "@", pshow range, ": ", pshow e]
          return (0, 0, 0, [])
      dumpPackages
    forM nfnoPkgPaths $ \path -> do
      dumpFromPkgJson path
      dumpPackages
  return ()
