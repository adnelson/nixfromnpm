{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NixFromNpm.Conversion.ToDisk where

import qualified Prelude as P
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Shelly (shelly, cp_r)
import System.Exit

import Data.SemVer
import Data.SemVer.Parser

import qualified Paths_nixfromnpm
import NixFromNpm.Common
import Nix.Types
import Nix.Parser
import Nix.Pretty (prettyNix)
import NixFromNpm.Conversion.ToNix (toDotNix,
                                    writeNix,
                                    rootDefaultNix,
                                    defaultNixExtending,
                                    packageJsonDefaultNix,
                                    resolvedPkgToNix,
                                    nodePackagesDir)
import NixFromNpm.Options
import NixFromNpm.Npm.Types
import NixFromNpm.PackageMap (PackageMap, pmLookup, pmDelete, pmMap, psToList)
import NixFromNpm.NpmLookup

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
  doesDirectoryExist (path </> nodePackagesDir) >>= \case
    False -> case maybeName of
      Just name -> errorC [
        "Path ", pathToText path, " does not exist or does not ",
        "contain a `", pathToText nodePackagesDir, "` folder"]
      Nothing -> return mempty
    True -> withDir (path </> nodePackagesDir) $ do
      let wrapper = maybe FromOutput FromExtension maybeName
      putStrsLn ["Searching for existing expressions in ", pathToText path,
                 "..."]
      contents <- getDirectoryContents "."
      verMaps <- map catMaybes $ forM contents $ \dir -> do
        exprs <- doesDirectoryExist dir >>= \case
          True -> withDir dir $ do
            contents <- getDirectoryContents "."
            let files = filter (hasExt "nix") contents
            catMaybes <$> mapM (parseVersion $ pathToText dir) files
          False -> do
            return mempty -- not a directory
        return $ case exprs of
          [] -> Nothing
          vs -> Just (pathToText dir, H.map wrapper $ H.fromList exprs)
      let total = sum $ map (H.size . snd) verMaps
      putStrsLn ["Found ", render total, " expressions in ", pathToText path]
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

-- | Initialize an output directory from scratch. This means:
-- * Creating a .nixfromnpm-version file storing the current version.
-- * Copying over the nix node libraries that nixfromnpm provides.
-- * Creating a default.nix file.
-- * Creating a nodePackages folder.
initializeOutput :: NpmFetcher ()
initializeOutput = do
  outputPath <- asks nfsOutputPath
  extensions <- asks nfsExtendPaths
  createDirectoryIfMissing outputPath
  withDir outputPath $ do
    putStrsLn ["Initializing  ", pathToText outputPath]
    createDirectoryIfMissing nodePackagesDir
    version <- case fromHaskellVersion Paths_nixfromnpm.version of
      Left err -> fatal err
      Right v -> return v
    writeFile ".nixfromnpm-version" $ renderSV version
    case H.keys extensions of
      [] -> do -- Then we are creating a new root.
        writeNix "default.nix" rootDefaultNix
        putStrsLn ["Generating node libraries in ", pathToText outputPath]
        nodeLibPath <- (</> "nodeLib") <$> getDataFileName "nix-libs"
        createDirectoryIfMissing (outputPath </> "nodeLib")
        contents <- getDirectoryContents nodeLibPath
        forM_ contents $ \file ->
          copyFile (nodeLibPath </> file) (outputPath </> "nodeLib" </> file)
      (extName:_) -> do -- Then we are extending things.
        writeNix "default.nix" $ defaultNixExtending extName extensions

updateLatestNix :: MonadIO io => io ()
updateLatestNix = do
  pwd <- getCurrentDirectory
  -- Remove the `latest.nix` symlink if it exists.
  whenM (doesFileExist "latest.nix") $ removeFile "latest.nix"
  -- Grab the latest version and create a symlink `latest.nix`
  -- to that.
  let convert fname =
        case parseSemVer (T.dropEnd 4 $ pathToText fname) of
          Left _ -> Nothing -- not a semver, so we don't consider it
          Right ver -> Just ver -- return the version
  catMaybes . map convert <$> getDirectoryContents "." >>= \case
    [] -> return ()
    versions -> do
      createSymbolicLink (toDotNix $ maximum versions) "latest.nix"


mergeInto :: (MonadIO io, MonadBaseControl IO io)
          => FilePath -- ^ Source path, containing store objects
          -> FilePath -- ^ Target path, also containing store objects
          -> io ()
mergeInto source target = withDir (source </> nodePackagesDir) $ do
  packageDirs <- getDirectoryContents "."
  forM_ packageDirs $ \dir -> do
    let targetDir = target </> nodePackagesDir </> dir
    whenM (not <$> doesDirectoryExist targetDir) $ do
      putStrsLn ["Creating directory ", pathToText targetDir]
      createDirectory targetDir
    withDir dir $ do
      dotNixFiles <- filter (hasExt "nix") <$> getDirectoryContents "."
      forM_ dotNixFiles $ \f -> do
        whenM (not <$> doesFileExist (targetDir </> f)) $ do
          putStrsLn ["Copying ", pathToText f, " to ",
                     pathToText (targetDir </> f)]
          copyFile f (targetDir </> f)
      withDir targetDir updateLatestNix


-- | Actually writes the packages to disk. Takes in the new packages to write,
--   and the names/paths to the libraries being extended.
writeNewPackages :: NpmFetcher ()
writeNewPackages = takeNewPackages <$> gets resolved >>= \case
  newPackages
    | H.null newPackages -> putStrLn "No new packages created."
    | otherwise -> forM_ (H.toList newPackages) $ \(pkgName, pkgVers) -> do
        forM_ (H.toList pkgVers) $ \(ver, expr) -> do
          writePackage pkgName ver expr
        dir <- outputDirOf pkgName
        withDir dir updateLatestNix

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
          rPkg <- withoutPackage name version $ versionInfoToResolved verinfo
          writeNix "project.nix" $ resolvedPkgToNix rPkg
          outputPath <- asks nfsOutputPath
          writeNix "default.nix" $ packageJsonDefaultNix outputPath

-- | Show all of the broken packages.
showBrokens :: NpmFetcher ()
showBrokens = H.toList <$> gets brokenPackages >>= \case
  [] -> return ()
  brokens -> do
    putStrsLn ["Failed to generate expressions for ", pshow (length brokens),
               " downstream dependencies."]
    forM_ brokens $ \(name, rangeMap) -> do
      forM_ (M.toList rangeMap) $ \(range, report) -> do
        putStrLn $ showRangePair name range
        let deps = bprDependencyOf report
        when (H.size deps > 0) $ do
          putStrsLn ["Dependency of: ", showPairs $ psToList deps]
        putStrsLn ["Failed to build because: ", pshow (bprReason report)]

-- | See if any of the top-level packages failed to build, and return a
-- non-zero status if they did.
checkForBroken :: [(Name, NpmVersionRange)] -> NpmFetcher ExitCode
checkForBroken inputs = do
  let findBrokens [] = return []
      findBrokens ((name, range):others) = getBroken name range >>= \case
        Nothing -> findBrokens others
        Just report -> ((name, range, report) :) <$> findBrokens others
  findBrokens inputs >>= \case
    [] -> putStrLn "Top-level packages built successfully." >> return ExitSuccess
    pkgs -> do
      putStrLn "The following packages failed to build:"
      forM_ pkgs $ \(name, range, report) -> do
        putStrLn $ showRangePair name range
        putStrsLn ["Failed because: ", pshow $ bprReason report]
      return $ ExitFailure 1

-- | Traverses down a dependency tree, seeing if the dependencies of a package
-- are broken.
checkPackage :: ResolvedPkg -> NpmFetcher (Maybe BrokenPackageReason)
checkPackage ResolvedPkg{..} = go (H.toList rpDependencies) where
  go [] = return Nothing -- all done!
  go ((name, Broken reason):_) = return $ Just (BrokenDependency name reason)
  go ((name, Resolved ver):rest) = do
    pmLookup name ver <$> gets resolved >>= \case
      Nothing -> return $ Just (UnsatisfiedDependency name)
      Just (NewPackage rpkg) -> checkPackage rpkg >>= \case
        Nothing -> go rest
        Just err -> return $ Just err
      Just _ -> go rest

dumpPkgFromOptions :: NixFromNpmOptions -> IO ExitCode
dumpPkgFromOptions (opts@NixFromNpmOptions{..}) = do
  let settings = defaultSettings {
    nfsGithubAuthToken = nfnoGithubToken,
    nfsNpmAuthToken = nfnoNpmToken,
    nfsRegistries = nfnoRegistries,
    nfsRequestTimeout = fromIntegral nfnoTimeout,
    nfsOutputPath = nfnoOutputPath,
    nfsExtendPaths = nfnoExtendPaths,
    nfsMaxDevDepth = nfnoDevDepth,
    nfsCacheDepth = nfnoCacheDepth,
    nfsRealTimeWrite = nfnoRealTime
    }
  (status, _) <- runNpmFetchWith settings startState $ do
    preloadPackages
    initializeOutput
    forM nfnoPkgNames $ \(name, range) -> do
      _resolveNpmVersionRange name range
        `catch` \(e :: SomeException) -> do
          warns ["Failed to build ", name, "@", pshow range, ": ", pshow e]
          addBroken name range (Reason $ show e)
          return (0, 0, 0, [])
      writeNewPackages
    forM nfnoPkgPaths $ \path -> do
      dumpFromPkgJson path
      writeNewPackages
    showBrokens
    checkForBroken nfnoPkgNames
  return status
