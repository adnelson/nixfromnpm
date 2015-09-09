{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative

import NixFromNpm hiding (getArgs, (<>))


main :: IO ()
main = do
  maybeToken <- getEnv "GITHUB_TOKEN"
  let opts = info (helper <*> pOptions maybeToken)
             (fullDesc <> progDesc description <> header headerText)
  parsedOpts <- execParser opts
  dumpPkgFromOptions parsedOpts
  where
    description = concat ["nixfromnpm allows you to generate nix expressions ",
                          "automatically from npm packages. It provides ",
                          "features such as de-duplication of shared ",
                          "dependencies and advanced customization."]
    headerText = "nixfromnpm - Create nix expressions from NPM"
