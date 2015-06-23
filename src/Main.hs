{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import System.Environment (getArgs)
import System.Console.Docopt.NoTH

import NixFromNpm hiding (getArgs)

usageString :: String
usageString = "\
  \Usage:\n\
  \  nixfromnpm <packagename> [-o PATH] [options]\n\
  \Options:\n\
  \  -o, --output PATH                 Output files to this path\n\
  \  --extend PATHS...                 Use nix expressions existing at these\n\
  \                                    paths\n\                 
  \  --registries REGISTRIES...        Use only these registries\n\
  \  --extra-registries REGISTRIES..   Use these in addition to default registry\n\
  \  --no-cache                        Don't use existing cache\n\
  \  --test                            Don't write expressions, just fetch\n\
  \  --timeout SECONDS                 Seconds after which to fail fetching a\n\
  \                                    package\n\
  \  --node-version VERSION            Use this version of node\n"

main :: IO ()
main = do
  patterns <- parseUsageOrExit usageString
  args <- parseArgsOrExit patterns =<< getArgs
  let getText = map pack . getArgOrExitWith patterns args
      getFlag = isPresent args . longOption
  pkgName <- getText (argument "packagename")
  path <- getText (longOption "output")
  dumpPkgFromOptions $ (defaultOptions pkgName path) {
        nfnoNoCache = getFlag "no-cache"
      }
