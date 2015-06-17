{-# LANGUAGE QuasiQuotes #-}
module Main where

import System.Environment (getArgs)
import System.Console.Docopt

import NixFromNpm hiding (getArgs)

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

args ?? arg = getArgOrExitWith patterns args (argument arg)

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  pkgName <- args ?? "packagename"
  path <- args ?? "outputpath"
  let noCache = isPresent args (longOption "no-cache")
  dumpPkgNamed noCache (pack pkgName) (pack path)
