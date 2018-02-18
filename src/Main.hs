module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)

import NixFromNpm.Cli (runWithArgs)

main :: IO ()
main = exitWith =<< runWithArgs =<< getArgs
