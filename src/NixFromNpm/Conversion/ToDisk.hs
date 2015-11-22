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
import qualified Data.HashSet as HS
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
import NixFromNpm.Conversion.ToNix (ResolvedPkg(..),
                                    toDotNix,
                                    writeNix,
                                    rootDefaultNix,
                                    defaultNixExtending,
                                    packageJsonDefaultNix,
                                    resolvedPkgToNix,
                                    nodePackagesDir)
import NixFromNpm.Options
import NixFromNpm.Npm.Types
import NixFromNpm.Npm.PackageMap (PackageMap, PackageName(..),
                                  pmLookup, pmDelete, pmMap,
                                  psToList)
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
parseVersionFiles :: MonadIO io
                  => PackageName -- ^ Name of the package this is a version of.
                  -> FilePath    -- ^ Folder containing .nix files for this
                                 --   package.
                  -> io (PackageMap NExpr) -- ^ Version and expression.
parseVersionFiles pkgName folder = do
  maybeExprs <- forItemsInDir folder $ \path -> do
    let (versionTxt, ext) = splitExtension $ filename path
    case parseSemVer (pathToText versionTxt) of
      _ | ext /= Just "nix" -> return Nothing -- not a nix file
      Left _ -> return Nothing -- not a version file
      Right version -> parseNixString . pack <$> readFile path >>= \case
        Failure err -> do
          putStrsLn ["Warning: expression for ", pshow pkgName, " version ",
                     pathToText versionTxt, " failed to parse:\n", pshow err]
          return Nothing -- invalid nix, should overwrite
        Success expr -> return $ Just (version, expr)
  return $ H.singleton pkgName (H.fromList $ catMaybes maybeExprs)

-- | Given the path to a file possibly containing nix expressions, finds all
--   expressions findable at that path and returns a map of them.
findExisting :: (MonadBaseControl IO io, MonadIO io)
             => Maybe Name -- ^ Is `Just` if this is an extension.
             -> FilePath       -- ^ The path to search.
             -> io (PackageMap PreExistingPackage)
             -- ^ Mapping of package names to maps of versions to nix
             --   expressions.
findExisting maybeName path = do
  doesDirectoryExist (path </> nodePackagesDir) >>= \case
    False -> case maybeName of
      Just name -> errorC [
        "Path ", pathToText path, " does not exist or does not ",
        "contain a `", pathToText nodePackagesDir, "` folder"]
      Nothing -> return mempty
    True -> do
      let wrapper = maybe FromOutput FromExtension maybeName
      putStrsLn ["Searching for existing expressions in ", pathToText path]
      verMapLists <- forItemsInDir (path </> nodePackagesDir) $ \dir -> do
        doesDirectoryExist dir >>= \case
          False -> return mempty -- not a directory
          True -> case T.split (=='@') $ getFilename dir of
            -- Check if the directory starts with "@", in which case it's
            -- a namespace.
            ["", namespace] -> do
              forItemsInDir dir $ \dir' -> do
                let pkgName = PackageName (getFilename dir') (Just namespace)
                parseVersionFiles pkgName dir'
            [name] -> do
              let pkgName = simpleName $ getFilename dir
              singleton <$> parseVersionFiles pkgName dir
            _ -> return mempty
      let verMaps = concat $ concat verMapLists
          total = sum $ map (sum . map H.size) verMapLists
      putStrsLn ["Found ", render total, " expressions in ", pathToText path]
      return $ pmMap wrapper verMaps

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
  putStrsLn ["Initializing  ", pathToText outputPath]
  createDirectoryIfMissing (outputPath </> nodePackagesDir)
  version <- case fromHaskellVersion Paths_nixfromnpm.version of
    Left err -> fatal err
    Right v -> return v
  writeFile (outputPath </> ".nixfromnpm-version") $ pshow version
  case H.keys extensions of
    [] -> do -- Then we are creating a new root.
      writeNix (outputPath </> "default.nix") rootDefaultNix
      putStrsLn ["Generating node libraries in ", pathToText outputPath]
      nodeLibPath <- (</> "nodeLib") <$> getDataFileName "nix-libs"
      createDirectoryIfMissing (outputPath </> "nodeLib")
      contents <- getDirectoryContents nodeLibPath
      forItemsInDir_ nodeLibPath $ \path -> do
        whenM (isFile path) $ do
          copyFile path (outputPath </> "nodeLib" </> filename path)
    (extName:_) -> do -- Then we are extending things.
      writeNix (outputPath </> "default.nix") $
        defaultNixExtending extName extensions

-- | Merges one folder containing expressions into another.
mergeInto :: (MonadIO io, MonadBaseControl IO io)
          => FilePath -- ^ Source path, containing store objects
          -> FilePath -- ^ Target path, also containing store objects
          -> io ()
mergeInto source target = do
  -- Go through all of the packages in the source directory.
  forItemsInDir_ (source </> nodePackagesDir) $ \srcDir -> do
    let targetDir = target </> nodePackagesDir </> filename srcDir
    -- Create a directory for that package, if it doesn't exist.
    whenM (not <$> doesDirectoryExist targetDir) $ do
      putStrsLn ["Creating directory ", pathToText targetDir]
      createDirectory targetDir
    -- Copy every version file found in that directory as well.
    dotNixFiles <- filter (hasExt "nix") <$> listDirFullPaths srcDir
    forM_ dotNixFiles $ \versionFile -> do
      let targetVersionFile = targetDir </> filename versionFile
      whenM (not <$> doesFileExist targetVersionFile) $ do
        putStrsLn ["Copying ", pathToText versionFile, " to ",
                   pathToText targetVersionFile]
        copyFile versionFile targetVersionFile
    updateLatestNix targetDir

-- | Update all of the latest.nix symlinks in an output folder.
updateLatestNixes :: MonadIO io => FilePath -> io ()
updateLatestNixes outputDir = do
  whenM (doesDirectoryExist (outputDir </> nodePackagesDir)) $ do
    forItemsInDir_ (outputDir </> nodePackagesDir) $ \pkgDir -> do
      if "@" `isPrefixOf` getFilename pkgDir
      -- If the directory starts with '@', then it's a namespace
      -- directory and we should recur on its contents.
      then forItemsInDir_ pkgDir updateLatestNix
      -- Otherwise, just update the latest.nix in this directory.
      else updateLatestNix pkgDir

-- | Actually writes the packages to disk. Takes in the new packages to write,
-- and the names/paths to the libraries being extended.
writeNewPackages :: NpmFetcher ()
writeNewPackages = takeNewPackages <$> gets resolved >>= \case
  newPackages
    | H.null newPackages -> putStrLn "No new packages created."
    | otherwise -> forM_ (H.toList newPackages) $ \(pkgName, pkgVers) -> do
        forM_ (H.toList pkgVers) $ \(ver, expr) -> do
          writePackage pkgName ver expr
        updateLatestNix =<< outputDirOf pkgName

dumpFromPkgJson :: FilePath -- ^ Path to folder containing package.json.
                -> NpmFetcher ()
dumpFromPkgJson path = do
  doesDirectoryExist path >>= \case
    False -> errorC ["No such directory ", pathToText path]
    True -> doesFileExist (path </> "package.json") >>= \case
      False -> errorC ["No package.json found in ", pathToText path]
      True -> do
        verinfo <- extractPkgJson (path </> "package.json")
        let (name, version) = (viName verinfo, viVersion verinfo)
        putStrsLn ["Generating expression for package ", pshow name,
                   ", version ", pshow version]
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
        putStrsLn ["  ", showRangePair name range]
        let chains = bprDependencyChains report
        when (HS.size chains > 0) $ do
          putStrLn "   Dependency of:"
          forM_ (HS.toList chains) $ \chain ->
            putStrsLn ["    ",
                       mapJoinBy " -> " (uncurry showPair) (reverse chain)]
        putStrsLn ["Failed to build because: ", pshow (bprReason report)]

-- | See if any of the top-level packages failed to build, and return a
-- non-zero status if they did.
checkForBroken :: [(PackageName, NpmVersionRange)] -> NpmFetcher ExitCode
checkForBroken inputs = do
  -- findBrokens will look for any of the packages in
  let findBrokens [] = return []
      findBrokens ((name, range):others) = getBroken name range >>= \case
        Nothing -> findBrokens others
        Just report -> ((name, range, report) :) <$> findBrokens others
  findBrokens inputs >>= \case
    [] -> do
      putStrsLn $ case inputs of
        [] -> ["No packages to build."]
        [(p,v)] -> ["Package ", showRangePair p v, " built successfully."]
        pkgs -> ["Packages ", mapJoinBy ", " (uncurry showRangePair) inputs,
                 " built successfully."]
      return ExitSuccess
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
dumpPkgFromOptions opts
  | nfnoPkgNames opts == [] &&
    nfnoPkgPaths opts == [] &&
    nfnoExtendPaths opts == mempty = do
      putStrLn "No packages given, nothing to do..."
      return $ ExitFailure 1
dumpPkgFromOptions (opts@NixFromNpmOptions{..}) = do
  let settings = defaultSettings {
    nfsGithubAuthToken = nfnoGithubToken,
    nfsNpmAuthTokens = nfnoNpmTokens,
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
          warns ["Failed to build ", pshow name, "@", pshow range,
                 ": ", pshow e]
          addBroken name range (Reason $ show e)
          return $ semver 0 0 0
      whenM (not <$> asks nfsRealTimeWrite) writeNewPackages
    forM nfnoPkgPaths $ \path -> do
      dumpFromPkgJson path
      whenM (not <$> asks nfsRealTimeWrite) writeNewPackages
    showBrokens
    checkForBroken nfnoPkgNames
  return status
