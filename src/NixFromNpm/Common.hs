{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module NixFromNpm.Common (
    module ClassyPrelude,
    module Control.Applicative,
    module Control.Exception,
    module Control.Exception.ErrorList,
    module Control.Monad,
    module Control.Monad.Except,
    module Control.Monad.Identity,
    module Control.Monad.State.Strict,
    module Control.Monad.Reader,
    module Control.Monad.Trans,
    module Data.Char,
    module Data.Default,
    module Data.HashMap.Strict,
    module Data.Either,
    module Data.Maybe,
    module Data.List,
    module Data.String.Utils,
    module GHC.Exts,
    module Filesystem.Path.CurrentOS,
    module Network.URI,
    module GHC.IO.Exception,
    module System.Directory,
    module Text.Render,
    module System.FilePath.Posix,
    Name, Record, Path,
    tuple, tuple3, fromRight, cerror, cerror', uriToText, uriToString, slash,
    putStrsLn, pathToText, putStrs, dropSuffix, maybeIf, grab, withDir, failC,
    pathToString, joinBy, mapJoinBy, getEnv, modifyMap, hasSuffix, pshow
  ) where

import ClassyPrelude hiding (assert, asList, find, FilePath, bracket,
                             maximum, maximumBy,
                             minimum, minumumBy, try)
import qualified Prelude as P
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), (<=<), (>=>), ask,
                             asks)
import Control.Monad.State.Strict (MonadState, StateT, State, get, gets,
                                   modify, put, liftM, liftIO, runState,
                                   runStateT, execState, execStateT,
                                   evalState, evalStateT)
import Control.Monad.Except (ExceptT, MonadError(..), throwError, runExceptT)
import Control.Exception.ErrorList
import Control.Monad.Identity (Identity(..))
import Control.Applicative hiding (empty, optional)
import Data.Char (isDigit, isAlpha)
import Data.Default
import Data.List (maximum, maximumBy)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Either (isRight, isLeft)
import Data.String.Utils hiding (join)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS (FilePath, fromText, toText, collapse)
import GHC.Exts (IsList)
import GHC.IO.Exception
import Control.Exception (bracket)
import Text.Render hiding (renderParens)
import Network.URI (URI(..), URIAuth(..), parseURI, parseAbsoluteURI,
                    parseRelativeReference, relativeTo)
import qualified Network.URI as NU
import Shelly hiding (get, relativeTo)
import System.Directory
import System.FilePath.Posix hiding (FilePath)

-- | Indicates that the text is some identifier.
type Name = Text

-- | Indicates that the text is some path.
type Path = Text

-- | A record is a lookup table with string keys.
type Record = HashMap Name

-- | Takes two applicative actions and returns their result as a 2-tuple.
tuple :: Applicative f => f a -> f b -> f (a, b)
tuple action1 action2 = (,) <$> action1 <*> action2

-- | Takes three applicative actions and returns their result as a 3-tuple.
tuple3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
tuple3 action1 action2 action3 = (,,) <$> action1 <*> action2 <*> action3

-- | Creates a new hashmap by applying a function to every key in it.
alterKeys :: (Eq k, Hashable k, Eq k', Hashable k') =>
             (k -> k') -> HashMap k v -> HashMap k' v
alterKeys f mp = do
  let pairs = H.toList mp
      newPairs = P.map (\(k, v) -> (f k, v)) pairs
      newMap = H.fromList newPairs
  newMap

-- | Create a hashmap by applying a test to everything in the existing
-- map.
modifyMap :: (Eq k, Hashable k) => (a -> Maybe b) -> HashMap k a -> HashMap k b
modifyMap test inputMap = foldl' step mempty $ H.toList inputMap where
  step result (k, elem) = case test elem of
    Nothing -> result
    Just newElem -> H.insert k newElem result

-- | Concatenate Strings into an error exception.
cerror :: [String] -> a
cerror = error . concat

-- | Concatenate Texts into an error exception.
cerror' :: [Text] -> a
cerror' = cerror . map unpack

-- | Show as text
pshow = pack . show

-- | Like `fromJust` for Eithers. Partial function!
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left err) = error "Expected `Right` value"

-- | Convert a URI into Text.
uriToText :: URI -> Text
uriToText = pack . uriToString

-- | Convert a URI into String.
uriToString :: URI -> String
uriToString uri = NU.uriToString id uri ""

-- | Appends text to URI with a slash. Ex: foo.com `slash` bar == foo.com/bar.
slash :: URI -> Text -> URI
slash uri txt = case parseRelativeReference (unpack txt) of
  Nothing -> error ("Invalid appending URI: " <> show txt)
  Just uri' -> uri' `relativeTo` uri

infixl 6 `slash`

-- | Concatenate text and print it to stdout with a newline.
putStrsLn :: MonadIO m => [Text] -> m ()
putStrsLn = putStrLn . concat

-- | Concatenate text and print it to stdout.
putStrs :: MonadIO m => [Text] -> m ()
putStrs = putStr . concat

-- | Convert a FilePath into Text.
pathToText :: FilePath -> Text
pathToText pth = case toText pth of
  Left p -> p
  Right p -> p

-- | Convert a FilePath into a string.
pathToString :: FilePath -> String
pathToString = unpack . pathToText

-- | Strip the given suffix from the given String.
dropSuffix :: String -> String -> String
dropSuffix suffix s | s == suffix = ""
dropSuffix suffix (c:cs) = c : dropSuffix suffix cs
dropSuffix suffix "" = ""

-- | Return true if the given suffix is on the given String.
hasSuffix :: String -> String -> Bool
hasSuffix suffix s | s == suffix = True
hasSuffix suffix (_:cs) = hasSuffix suffix cs
hasSuffix suffix "" = False

-- | Return a Just value if the argument is True, else Nothing.
maybeIf :: Bool -> a -> Maybe a
maybeIf True x = Just x
maybeIf False _ = Nothing

-- | Unsafe lookup.
grab :: (Hashable k, Eq k) => k -> HashMap k v -> v
grab k = fromJust . H.lookup k

-- | Perform an IO action inside of the given directory. Catches exceptions.
withDir :: String -> IO a -> IO a
withDir directory action = do
  cur <- getCurrentDirectory
  bracket (setCurrentDirectory directory)
          (\_ -> setCurrentDirectory cur)
          (\_ -> action)

-- | Synonym for intercalate.
joinBy :: Text -> [Text] -> Text
joinBy = T.intercalate

-- | Map a function and intercalate the results.
mapJoinBy :: Text -> (a -> Text) -> [a] -> Text
mapJoinBy sep func = joinBy sep . map func

-- | Reads an environment variable.
getEnv :: MonadIO m => Text -> m (Maybe Text)
getEnv = shelly . silently . get_env

failC :: Monad m => [Text] -> m a
failC = fail . unpack . concat
