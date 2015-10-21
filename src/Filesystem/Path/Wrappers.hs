{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
module Filesystem.Path.Wrappers where

import ClassyPrelude hiding (FilePath, unpack)
import Data.Text hiding (map)
import qualified System.Directory as Dir
import Filesystem.Path.CurrentOS
import Control.Monad.Trans.Control
import Control.Exception.Lifted

-- | Convert a FilePath into Text.
pathToText :: FilePath -> Text
pathToText pth = case toText pth of
  Left p -> p
  Right p -> p

-- | Convert a FilePath into a string.
pathToString :: FilePath -> String
pathToString = unpack . pathToText

-- | Perform an IO action inside of the given directory. Catches exceptions.
withDir :: (MonadBaseControl IO io, MonadIO io)
        => FilePath -> io a -> io a
withDir directory action = do
  cur <- getCurrentDirectory
  bracket_ (setCurrentDirectory directory)
           (setCurrentDirectory cur)
           action

takeBaseName :: FilePath -> Text
takeBaseName = pathToText . basename

createDirectoryIfMissing :: MonadIO m => FilePath -> m ()
createDirectoryIfMissing = liftIO . Dir.createDirectoryIfMissing True .
                             pathToString

doesDirectoryExist :: MonadIO m => FilePath -> m Bool
doesDirectoryExist = liftIO . Dir.doesDirectoryExist . pathToString

doesFileExist :: MonadIO m => FilePath -> m Bool
doesFileExist = liftIO . Dir.doesFileExist . pathToString

getCurrentDirectory :: MonadIO m => m FilePath
getCurrentDirectory = decodeString <$> liftIO Dir.getCurrentDirectory

removeDirectoryRecursive :: MonadIO m => FilePath -> m ()
removeDirectoryRecursive = liftIO . Dir.removeDirectoryRecursive . pathToString

removeFile :: MonadIO m => FilePath -> m ()
removeFile = liftIO . Dir.removeFile . pathToString

getDirectoryContents :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContents dir = do
  contents <- liftIO $ Dir.getDirectoryContents $ pathToString dir
  return $ map decodeString contents

hasExt :: Text -> FilePath -> Bool
hasExt ext path = case extension path of
  Just ext' | ext == ext' -> True
  otherwise -> False

setCurrentDirectory :: MonadIO io => FilePath -> io ()
setCurrentDirectory = liftIO . Dir.setCurrentDirectory . pathToString
