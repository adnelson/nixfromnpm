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

getFlag args f = isPresent args (longOption f)

main :: IO ()
main = do
  patterns <- parseUsageOrExit usageString
  args <- parseArgsOrExit patterns =<< getArgs
  let args ?? arg = getArgOrExitWith patterns args (argument arg)
  pkgName <- map pack $ args ?? "packagename"
  path <- map pack $ args ?? "outputpath"
  dumpPkgFromOptions $ (defaultOptions pkgName path) {
        nfnoNoCache = args `getFlag` "no-cache"
      }
