{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module NixFromNpm.Merge where

import NixFromNpm.Common
import NixFromNpm.Conversion.ToDisk (writeNodePackagesNix)
import NixFromNpm.Conversion.ToNix (nodePackagesDir)

-- Some types which are more expressive than their raw counterparts.
-- Hey, if you have a cool type system, why not leverage it...
data MergeType = DryRun | DoIt deriving (Eq)
newtype Source = Source FilePath
newtype Dest = Dest FilePath

-- | Merges one folder containing expressions into another. After the merge,
-- generates a new nodePackages/default.nix in the target directory.
mergeInto :: (MonadIO io, MonadBaseControl IO io)
          => MergeType -- ^ If DryRun, it will just report what it would have
                       -- otherwise done.
          -> Source -- ^ Source path, containing store objects
          -> Dest -- ^ Target path, also containing store objects
          -> io ()
mergeInto mergeType (Source source) (Dest target) = do
  let dryRun = mergeType == DryRun
  whenM (not <$> doesDirectoryExist (source </> nodePackagesDir)) $ do
    failC ["No node packages folder in source ", pathToText source]
  whenM (not <$> doesDirectoryExist (target </> nodePackagesDir)) $ do
    failC ["No node packages folder in target ", pathToText target]
  -- Go through all of the packages in the source directory.
  forItemsInDir_ (source </> nodePackagesDir) $ \srcDir -> do
    let targetDir = target </> nodePackagesDir </> filename srcDir
    -- Create a directory for that package, if it doesn't exist.
    whenM (not <$> doesDirectoryExist targetDir) $ do
      putStrsLn ["Creating directory ", pathToText targetDir]
      if dryRun then putStrLn "  (Skipped due to dry run)" else
        createDirectory targetDir
    -- Copy every version file found in that directory as well.
    dotNixFiles <- filter (hasExt "nix") <$> listDirFullPaths srcDir
    forM_ dotNixFiles $ \versionFile -> do
      let targetVersionFile = targetDir </> filename versionFile
      whenM (not <$> doesFileExist targetVersionFile) $ do
        putStrsLn ["Copying ", pathToText versionFile, " to ",
                   pathToText targetVersionFile]
        if dryRun then putStrLn "  (Skipped due to dry run)" else
          copyFile versionFile targetVersionFile
  writeNodePackagesNix True target
