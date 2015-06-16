{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

----------------------------------------------------------------------------
import NixFromNpm
----------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= \case
  pkgName:path:_ -> dumpPkgNamed pkgName path
  _ -> error "Incorrect number of arguments"
