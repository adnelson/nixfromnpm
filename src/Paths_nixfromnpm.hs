{-# LANGUAGE NoImplicitPrelude #-}
module Paths_nixfromnpm where

import qualified Prelude as P
import ClassyPrelude
import System.Directory (getCurrentDirectory)

getDataFileName :: P.FilePath -> IO P.FilePath
getDataFileName filename = do
  pwd <- getCurrentDirectory
  return $ pwd <> "/" <> filename
