{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module NixFromNpm.HttpTools (
  module Network.Curl,
  Long, HttpResult(..), HttpError(..), getHttpWith, makeHeaders, curlGetBS
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Curl

import NixFromNpm.Common

data HttpResult a
  = HttpSuccess a
  | HttpError HttpError
  deriving (Show, Eq, Typeable)

data HttpError
  = HttpErrorWithCode Int
  | HttpTimedOut Long
  | CurlError CurlCode
  deriving (Show, Eq, Typeable)

instance Exception HttpError

-- | Given a URL and some options, perform a curl request and return the
-- resulting code, HTTP status, and response body.
curlGetBS :: (MonadBaseControl IO io, MonadIO io)
          => URLString
          -> [CurlOption]
          -> io (CurlCode, Int, BL8.ByteString)
curlGetBS url opts = liftIO $ initialize >>= \ h -> do
  (finalBody, gatherBody) <- newIncoming
  setopt h (CurlFailOnError True)
  setDefaultSSLOpts h url
  setopt h (CurlURL url)
  setopt h (CurlWriteFunction (gatherOutput_ gatherBody))
  mapM (setopt h) opts
  rc <- perform h
  bs <- finalBody
  status <- getResponseCode h
  return (rc, status, bs)

-- | Convert (key, value) pairs into a curl Headers option.
makeHeaders :: [(Text, ByteString)] -> CurlOption
makeHeaders headers = CurlHttpHeaders $ map mk headers where
  mk (key, val) = T.unpack key <> ": " <> B.unpack val

getHttpWith :: (MonadBaseControl IO io, MonadIO io)
            => Long -- ^ Timeout in seconds
            -> Int  -- ^ Number of retries
            -> [(Text, ByteString)] -- ^ Headers
            -> URI -- ^ URI to hit
            -> io BL8.ByteString -- ^ Response content
getHttpWith timeout retries headers uri = loop retries where
  opts = [makeHeaders headers, CurlFollowLocation True,
          CurlTimeout timeout]
  toErr status CurlHttpReturnedError = HttpErrorWithCode status
  toErr _ CurlOperationTimeout = HttpTimedOut timeout
  toErr _ err = CurlError err
  loop retries = do
    (code, status, content) <- curlGetBS (uriToString uri) opts
    case code of
      CurlOK -> return content
      code | retries <= 0 -> throw $ toErr status code
           | otherwise -> do
               putStrsLn ["Request failed. ", tshow retries, " retries left."]
               loop (retries - 1)
