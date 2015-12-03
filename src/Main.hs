{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Encoding as T
import Options.Applicative
import System.Exit

import NixFromNpm.Common hiding (getArgs, (<>))
import NixFromNpm.Options (NixFromNpmOptions, parseOptions,
                           validateOptions)
import NixFromNpm.Conversion.ToDisk (dumpPkgFromOptions,
                                     mergeInto, MergeType(..),
                                     Source(..), Dest(..))

main :: IO ()
main = do
  let opts = info (helper <*> parseOptions)
             (fullDesc <> progDesc description <> header headerText)
  parsedOpts <- execParser opts
  validatedOpts <- validateOptions parsedOpts
  exitWith =<< dumpPkgFromOptions validatedOpts
  where
    description = concat ["nixfromnpm allows you to generate nix expressions ",
                          "automatically from npm packages. It provides ",
                          "features such as de-duplication of shared ",
                          "dependencies and advanced customization."]
    headerText = "nixfromnpm - Create nix expressions from NPM"
