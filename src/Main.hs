{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

----------------------------------------------------------------------------
import NixFromNpm
import Data.Text (Text)
import qualified Data.Text as T
----------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= \case
  pkgName:path:_ -> dumpPkgNamed False pkgName path
  _ -> error "Incorrect number of arguments"
