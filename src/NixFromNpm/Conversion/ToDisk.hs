{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module NixFromNpm.Conversion.ToDisk where

import Data.Either (either)
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.SemVer (semver, anyVersion, parseSemVer, fromHaskellVersion)
import Data.Text (Text)
import Shelly (shelly, cp_r, rm_rf, run_)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import Text.Printf (printf)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Prelude as P

import qualified Paths_nixfromnpm
import NixFromNpm.Common
import Nix.Expr (NExpr)
import Nix.Parser (Result(..), parseNixFile)
import Nix.Pretty (prettyNix)
#if MIN_VERSION_hnix(0,6,0)
import Nix.Render (MonadFile)
#endif
import NixFromNpm.Conversion.ToNix (ResolvedPkg(..),
                                    toDotNix,
                                    writeNix,
                                    rootDefaultNix,
                                    packageJsonDefaultNix,
                                    packageMapToNix,
                                    resolvedPkgToNix,
                                    nodePackagesDir)
import NixFromNpm.Options (NixFromNpmOptions(..))
import NixFromNpm.Npm.Types (BrokenPackageReason(..))
import NixFromNpm.Npm.Types (ResolvedDependency(..), unpackPSC, VersionInfo(..))
import NixFromNpm.Npm.Version (NpmVersionRange(..), showPair, showRangePair)
import NixFromNpm.Npm.PackageMap (simpleName, pmNumVersions, pmConcat)
import NixFromNpm.Npm.PackageMap (PackageMap, PackageName(..),
                                  pmLookup, pmDelete, pmMap,
                                  psToList)
import NixFromNpm.Npm.Resolve

-- | The npm lookup utilities will produce a bunch of fully defined packages.
-- However, the only packages that we want to write are the new ones; that
-- is, the ones that we've discovered and the ones that already exist. This
-- will perform the appropriate filter. It will also convert them to nix.
takeNewPackages :: PackageMap FullyDefinedPackage
                -> PackageMap NExpr
takeNewPackages startingRec = do
  let isNew (NewPackage rpkg) = Just $ resolvedPkgToNix rpkg
      isNew _ = Nothing
  H.filter (not . M.null) $ H.map (modifyMap isNew) startingRec

-- | Given the path to a package, finds all of the .nix files which parse
--   correctly.
parseVersionFiles ::
#if MIN_VERSION_hnix(0,6,0)
                     (MonadIO io, MonadFile io)
#else
                     MonadIO io
#endif
                  => Bool        -- ^ Verbose output.
                  -> PackageName -- ^ Name of the package this is a version of.
                  -> FilePath    -- ^ Folder with .nix files for this package.
                  -> io (PackageMap NExpr) -- ^ Version and expression.
parseVersionFiles verbose pkgName folder = do
  maybeExprs <- forItemsInDir folder $ \path -> do
    let (versionTxt, ext) = splitExtension $ filename path
    case parseSemVer (pathToText versionTxt) of
      _ | ext /= Just "nix" -> return Nothing -- not a nix file
      Left _ -> return Nothing -- not a version file
      Right version -> parseNixFile (pathToString path) >>= \case
        Failure err -> do
          putStrsLn ["Warning: expression at path ", tshow path,
                     " failed to parse:\n", tshow err]
          return Nothing -- invalid nix, should overwrite
        Success expr -> do
          when verbose $
            putStrsLn ["Discovered ", tshow pkgName, " at version ",
                       tshow version]
          return $ Just (version, expr)
  return $ H.singleton pkgName (M.fromList $ catMaybes maybeExprs)

-- | Given a directory containing npm nix expressions, parse it into a
-- packagemap of parsed nix expressions.
scanNodePackagesDir ::
#if MIN_VERSION_hnix(0,6,0)
  (MonadIO io, MonadFile io)
#else
  MonadIO io
#endif
  => Bool -> FilePath -> io (PackageMap NExpr)
scanNodePackagesDir verbose nodePackagesDir = pmConcat <$> do
  forItemsInDir nodePackagesDir $ \dir -> do
    doesDirectoryExist dir >>= \case
      False -> return mempty -- not a directory
      True -> case T.split (=='@') $ getFilename dir of
        -- Check if the directory starts with "@", in which case it's
        -- a namespace.
        ["", namespace] -> map pmConcat $ forItemsInDir dir $ \dir' -> do
          let pkgName = PackageName (getFilename dir') (Just namespace)
          parseVersionFiles verbose pkgName dir'
        [name] -> do
          parseVersionFiles verbose (simpleName $ getFilename dir) dir
        _ -> return mempty

-- | Given a nodePackages folder, create a default.nix which contains all
-- of the packages in that folder.
writeNodePackagesNix ::
#if MIN_VERSION_hnix(0,6,0)
  (MonadIO io, MonadFile io)
#else
  MonadIO io
#endif
  => Bool -> FilePath -> io ()
writeNodePackagesNix verbose path' = do
  path <- absPath path'
  whenM (not <$> doesDirectoryExist (path </> nodePackagesDir)) $ do
    failC ["No node packages folder in ", pathToText path]
  let defaultNix = path </> nodePackagesDir </> "default.nix"
  putStrsLn ["Generating package definition object in ", pathToText defaultNix]
  packages <- scanNodePackagesDir verbose (path </> nodePackagesDir)
  writeNix defaultNix $ packageMapToNix packages

-- | Given the path to a file possibly containing nix expressions, finds all
-- expressions findable at that path and returns a map of them.
findExisting ::
#if MIN_VERSION_hnix(0,6,0)
                (MonadBaseControl IO io, MonadIO io, MonadFile io)
#else
                (MonadBaseControl IO io, MonadIO io)
#endif
             => Bool -- ^ Verbose
             -> FilePath       -- ^ The path to search.
             -> io (PackageMap PreExistingPackage)
             -- ^ Mapping of package names to maps of versions to nix
             --   expressions.
findExisting verbose path = do
  doesDirectoryExist (path </> nodePackagesDir) >>= \case
    False -> pure mempty
    True -> do
      putStrsLn ["Searching for existing expressions in ", pathToText path]
      verMaps <- scanNodePackagesDir verbose (path </> nodePackagesDir)
      let total = pmNumVersions verMaps
      putStrsLn ["Found ", render total, " expressions in ", pathToText path]
      return $ pmMap FromOutput verMaps

-- | Given the output directory, finds any existing packages.
preloadPackages :: NpmFetcher ()
preloadPackages = do
  verbose <- asks nfsVerbose
  existing <- asks nfsCacheDepth >>= \case
    n | n < 0 -> return mempty
      | otherwise -> findExisting verbose =<< asks nfsOutputPath
  modify $ \s -> s {
    resolved = pmMap toFullyDefined existing <> resolved s
    }

-- | Initialize an output directory from scratch. This means:
-- * Creating a default.nix file.
-- * Creating a nodePackages folder.
initializeOutput :: NpmFetcher ()
initializeOutput = do
  outputPath <- asks nfsOutputPath
  version <- case fromHaskellVersion Paths_nixfromnpm.version of
    Left err -> fatal err
    Right v -> return v
  let defaultNixPath = outputPath </> "default.nix"
      -- skip the action if the path exists and overwrite is disabled.
      unlessExists path action = asks nfsOverwriteNixLibs >>= \case
        True -> action
        False -> doesFileExist path >>= \case
          True -> return ()
          False -> action
  putStrsLn ["Initializing  ", pathToText outputPath]
  createDirectoryIfMissing outputPath
  createDirectoryIfMissing (outputPath </> nodePackagesDir)

  unlessExists defaultNixPath $ do
    writeNix defaultNixPath rootDefaultNix

  -- Get the path to the files bundled with nixfromnpm which
  -- contain nix libraries.
  nixlibs <- liftIO (lookupEnv "NIX_LIBS_DIR") >>= \case
    Nothing -> getDataFileName "nix-libs"
    Just libPath -> pure (fromString libPath)

  let inputNodeLib = nixlibs </> "nodeLib"
  let outputNodeLib = outputPath </> "nodeLib"

  putStrsLn ["Generating node libraries in ", pathToText outputPath]

  shelly $ do
    rm_rf outputNodeLib
    cp_r inputNodeLib outputNodeLib

    let tools = outputNodeLib </> "tools"
    run_ "chmod" [ "-R", "+x", (pathToText tools) ]

-- | Actually writes the packages to disk.
writeNewPackages :: NpmFetcher ()
writeNewPackages = takeNewPackages <$> gets resolved >>= \case
  newPackages
    | H.null newPackages -> putStrLn "No new packages created."
    | otherwise -> forM_ (H.toList newPackages) $ \(pkgName, pkgVers) -> do
        forM_ (M.toList pkgVers) $ \(ver, expr) -> do
          writePackage pkgName ver expr

dumpFromPkgJson :: FilePath -- ^ Path to folder containing package.json.
                -> NpmFetcher ()
dumpFromPkgJson path = do
  doesDirectoryExist path >>= \case
    False -> errorC ["No such directory ", pathToText path]
    True -> doesFileExist (path </> "package.json") >>= \case
      False -> errorC ["No package.json found in ", pathToText path]
      True -> do
        -- Parse a VersionInfo object from the package.json file.
        verinfo <- extractPkgJson (path </> "package.json")
        let (name, version) = (viName verinfo, viVersion verinfo)
        putStrsLn ["Generating expression for package ", tshow name,
                   ", version ", tshow version]
        -- Convert this to a ResolvedPkg by resolving its dependencies.
        rPkg <- withoutPackage name version $ versionInfoToResolved verinfo
        writeNix (path </> "project.nix") $ resolvedPkgToNix rPkg
        outputPath <- asks nfsOutputPath
        writeNix (path </> "default.nix") $
          packageJsonDefaultNix outputPath

-- | Show all of the broken packages.
showBrokens :: NpmFetcher ()
showBrokens = H.toList <$> gets brokenPackages >>= \case
  [] -> return ()
  brokens -> do
    putStrsLn ["Failed to generate expressions for ", tshow (length brokens),
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
        putStrsLn ["Failed to build because: ", tshow (bprReason report)]

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
        putStrsLn ["Failed because: ", tshow $ bprReason report]
      return $ ExitFailure 1

-- | Traverses down a dependency tree, seeing if the dependencies of a package
-- are broken.
checkPackage :: ResolvedPkg -> NpmFetcher (Maybe BrokenPackageReason)
checkPackage ResolvedPkg{..} = go (H.toList rpDependencies) where
  go [] = return Nothing -- all done!
  go ((name, Broken reason):_) = return $ Just (BrokenDependency name reason)
  go ((name, Resolved (unpackPSC -> ver)):rest) = do
    pmLookup name ver <$> gets resolved >>= \case
      Nothing -> return $ Just (UnsatisfiedDependency name)
      Just (NewPackage rpkg) -> checkPackage rpkg >>= \case
        Nothing -> go rest
        Just err -> return $ Just err
      Just _ -> go rest

dumpPkgFromOptions :: NixFromNpmOptions -> IO ExitCode
dumpPkgFromOptions opts
  | nfnoPkgNames opts == [] && nfnoPkgPaths opts == [] = do
      putStrLn "No packages given, nothing to do..."
      return $ ExitFailure 1
dumpPkgFromOptions (opts@NixFromNpmOptions{..}) = do
  let settings = defaultSettings {
    nfsGithubAuthToken = nfnoGithubToken,
    nfsNpmAuthTokens = nfnoNpmTokens,
    nfsRegistries = nfnoRegistries,
    nfsRequestTimeout = fromIntegral nfnoTimeout,
    nfsOutputPath = nfnoOutputPath,
    nfsMaxDevDepth = nfnoDevDepth,
    nfsCacheDepth = nfnoCacheDepth,
    nfsRealTimeWrite = nfnoRealTime,
    nfsOverwriteNixLibs = nfnoOverwriteNixLibs
    }
  (status, _) <- runNpmFetchWith settings startState $ do
    preloadPackages
    initializeOutput
    forM nfnoPkgNames $ \(name, range) -> do
      resolveNpmVersionRange name range
        `catch` \(e :: SomeException) -> do
          warns ["Failed to build ", tshow name, "@", tshow range,
                 ": ", tshow e]
          addBroken name range (Reason $ show e)
          return $ semver 0 0 0
      whenM (not <$> asks nfsRealTimeWrite) writeNewPackages
    forM nfnoPkgPaths $ \path -> do
      dumpFromPkgJson path
      whenM (not <$> asks nfsRealTimeWrite) writeNewPackages
    writeNodePackagesNix False =<< asks nfsOutputPath
    showBrokens
    checkForBroken nfnoPkgNames
  return status
