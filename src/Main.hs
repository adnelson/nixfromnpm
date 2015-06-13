{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

----------------------------------------------------------------------------
import ClassyPrelude hiding (FilePath)
import qualified Prelude as P

import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.Attoparsec
import System.IO.Streams.HTTP
import Data.Aeson.Parser
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8

import NixFromNpm
----------------------------------------------------------------------------

main :: IO ()
main = do
  x :: Maybe Value <- decode <$> BL8.readFile "foo.json"
  print x

------------------------------------------------------------------------------
-- | GET test (openssl)
getJsonUrl :: String -> IO ()
getJsonUrl url = do
  j <- withOpenSSL $ do
    req <- parseUrl url
    withManager (opensslManagerSettings context) $ \mgr ->
      withHTTP req mgr $ parseFromStream json . responseBody
  print j

getJsonFile :: FromJSON a => String -> IO (Either String a)
getJsonFile = fmap eitherDecode . BL8.readFile

getPkgInfo :: String -> IO (Either String PackageInfo)
getPkgInfo = getJsonFile
