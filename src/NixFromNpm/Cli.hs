-- | The nixfromnpm command-line interface
{-# LANGUAGE NoImplicitPrelude #-}
module NixFromNpm.Cli (runWithArgs) where

import qualified Options.Applicative as O
import System.Environment (getArgs)
import System.Exit (ExitCode)

import NixFromNpm.Common hiding (getArgs)
import NixFromNpm.Options (NixFromNpmOptions, parseOptions,
                           validateOptions)
import NixFromNpm.Conversion.ToDisk (dumpPkgFromOptions)
import NixFromNpm.Merge (mergeInto, MergeType(..), Source(..), Dest(..))

-- | Execute an argument parser with a list of arguments.
customExecParser_ :: O.ParserInfo a -> [String] -> IO a
customExecParser_ pinfo args = do
  let result = O.execParserPure O.defaultPrefs pinfo args
  O.handleParseResult result

-- | Execute the CLI with an argument list, returning an exit code.
runWithArgs :: [String] -> IO ExitCode
runWithArgs args = do
  let pInfo = O.info (O.helper <*> parseOptions)
                (O.fullDesc <> O.progDesc description <> O.header headerText)

  parsedOpts <- customExecParser_ pInfo args
  validatedOpts <- validateOptions parsedOpts
  dumpPkgFromOptions validatedOpts
  where
    description = concat ["nixfromnpm allows you to generate nix expressions ",
                          "automatically from npm packages. It provides ",
                          "features such as de-duplication of shared ",
                          "dependencies and advanced customization."]
    headerText = "nixfromnpm - Create nix expressions from NPM"
