{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
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
    module Control.Exception.Lifted,
    module Control.Monad,
    module Control.Monad.Catch,
    module Control.Monad.Except,
    module Control.Monad.Identity,
    module Control.Monad.Reader,
    module Control.Monad.State.Strict,
    module Control.Monad.Trans,
    module Control.Monad.RWS.Strict,
    module Data.Char,
    module Data.Default,
    module Data.Either,
    module Data.HashMap.Strict,
    module Data.List,
    module Data.Map.Strict,
    module Data.Maybe,
    module Data.String.Utils,
    module Filesystem.Path.CurrentOS,
    module GHC.Exts,
    module Network.URI,
    module Filesystem.Path.Wrappers,
    module Text.Render,
    module Text.Printf,
    module Control.Monad.Trans.Control,
    module System.Console.ANSI,
    Name, AuthToken, Record, (//), (<>),
    uriToText, uriToString, putStrsLn, putStrs, maybeIf, failC,
    errorC, joinBy, mapJoinBy, getEnv, modifyMap, unsafeParseURI,
    parseURIText, withColor, withUL, warn, warns, assert, fatal, fatalC,
    partitionEither, throw, eitherToMaybe
#if !MIN_VERSION_mono_traversable(1,0,7)
    , dropSuffix
#endif
  ) where

import ClassyPrelude hiding (assert, asList, find, FilePath, bracket,
                             maximum, maximumBy, (</>), (<>),
                             minimum, try, stripPrefix, ioError,
                             mapM_, sequence_, foldM, forM_, throw, throwIO,
                             filterM, replicateM, writeFile, readFile,
                             writeFileUtf8, readFileUtf8, catch, catches,
                             Handler)
import Control.Exception (throw)
import Control.Monad.Catch (catch, catches, Handler(..))
import qualified Prelude as P
import Control.Monad.RWS.Strict hiding (Any, (<>))
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), (<=<), (>=>), ask,
                             asks)
import Control.Monad.State.Strict (MonadState, StateT, State, get, gets,
                                   modify, put, liftM, liftIO, runState,
                                   runStateT, execState, execStateT,
                                   evalState, evalStateT)
import Control.Monad.Except (ExceptT, MonadError(..), throwError, runExceptT)
import Control.Exception.Lifted () -- hiding (assert, )
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans.Control
import Control.Applicative hiding (empty, optional)
import Data.Char (isDigit, isAlpha)
import Data.Default
import Data.List (maximum, maximumBy)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as H
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Either (isRight, isLeft)
import Data.String.Utils hiding (join)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (concat, null, (<.>), empty)
import GHC.Exts (IsList)
import Text.Render hiding (renderParens)
import Text.Printf (printf)
import Network.URI (URI(..), URIAuth(..), parseURI, parseAbsoluteURI,
                    parseRelativeReference, relativeTo)
import qualified Network.URI as NU
import Shelly hiding (get, relativeTo)
import System.Console.ANSI
import Filesystem.Path.Wrappers

-- | Indicates that the text is some identifier.
type Name = Text

-- | Used to indicate something is meant for authentication.
type AuthToken = ByteString

-- | A record is a lookup table with string keys.
type Record = HashMap Name

newtype FatalError = Fatal Text deriving (Show, Eq, Typeable)
instance Exception FatalError

-- | Creates a new hashmap by applying a function to every key in it.
alterKeys :: (Eq k, Hashable k, Eq k', Hashable k') =>
             (k -> k') -> HashMap k v -> HashMap k' v
alterKeys f mp = do
  let pairs = H.toList mp
      newPairs = P.map (\(k, v) -> (f k, v)) pairs
      newMap = H.fromList newPairs
  newMap

-- | Create a hashmap by applying a test to everything in the existing
-- map. If the test returns Just, put it in the result, and otherwise leave
-- it out.
modifyHashMap :: (Eq k, Hashable k)
              => (a -> Maybe b) -> HashMap k a -> HashMap k b
modifyHashMap test inputMap = foldl' step mempty $ H.toList inputMap where
  step result (k, elem) = case test elem of
    Nothing -> result
    Just newElem -> H.insert k newElem result

-- | Same as modifyHashMap, but for Data.Maps.
modifyMap :: Ord k => (a -> Maybe b) -> Map k a -> Map k b
modifyMap test inputMap = foldl' step mempty $ M.toList inputMap where
  step result (k, elem) = case test elem of
    Nothing -> result
    Just newElem -> M.insert k newElem result


-- | Convert a URI into Text.
uriToText :: URI -> Text
uriToText = pack . uriToString

-- | Convert a URI into String.
uriToString :: URI -> String
uriToString uri = NU.uriToString id uri ""

-- | Concatenate text and print it to stdout with a newline.
putStrsLn :: MonadIO m => [Text] -> m ()
putStrsLn = putStrLn . concat

-- | Concatenate text and print it to stdout.
putStrs :: MonadIO m => [Text] -> m ()
putStrs = putStr . concat

#if !MIN_VERSION_mono_traversable(1,0,7)
-- | Strip the given suffix from the given string.
dropSuffix :: Text -> Text -> Text
dropSuffix suffix input = case T.stripSuffix suffix input of
  Nothing -> input
  Just stripped -> stripped
#endif

-- | Return a Just value if the argument is True, else Nothing.
maybeIf :: Bool -> a -> Maybe a
maybeIf True x = Just x
maybeIf False _ = Nothing

-- | Synonym for intercalate.
joinBy :: Text -> [Text] -> Text
joinBy = T.intercalate

-- | Map a function and intercalate the results.
mapJoinBy :: Text -> (a -> Text) -> [a] -> Text
mapJoinBy sep func = joinBy sep . map func

-- | Reads an environment variable.
getEnv :: MonadIO m => Text -> m (Maybe Text)
getEnv = shelly . silently . get_env

-- | Call the monadic fail function, concatenating a list of Text.
failC :: Monad m => [Text] -> m a
failC = fail . unpack . concat

-- | Throw an error after concatenation a list of Text.
errorC :: [Text] -> a
errorC = error . unpack . concat

-- | Appends text to URI with a slash. Ex: foo.com // bar ==
-- foo.com/bar.
(//) :: URI -> Text -> URI
uri // txt = do
  let fixedUri = unsafeParseURI $ case T.last (uriToText uri) of
        '/' -> uriToText uri
        _ -> uriToText uri <> "/"
  case parseRelativeReference (unpack txt) of
    Nothing -> errorC ["Invalid appending URI: ", tshow txt]
    Just uri' -> uri' `relativeTo` fixedUri

unsafeParseURI :: Text -> URI
unsafeParseURI txt = case parseURIText txt of
  Nothing -> errorC ["Invalid URI text: ", tshow txt]
  Just uri -> uri

parseURIText :: Text -> Maybe URI
parseURIText = parseURI . unpack

withColor :: MonadIO io => Color -> io a -> io a
withColor color action = do
  liftIO $ setSGR [SetColor Foreground Vivid color]
  result <- action
  liftIO $ setSGR [Reset]
  return result

withUL :: MonadIO io => io a -> io a
withUL action = do
  liftIO $ setSGR [SetUnderlining SingleUnderline]
  result <- action
  liftIO $ setSGR [SetUnderlining NoUnderline]
  return result

-- | Print a warning string in red.
warn :: MonadIO io => Text -> io ()
warn msg = withColor Red $ putStrsLn ["WARNING: ", msg]

-- | Print a warning string by concatenating strings.
warns :: MonadIO io => [Text] -> io ()
warns = warn . concat

-- | Throws the given exception if the test fails.
assert :: (Monad m, Exception e) => m Bool -> e -> m ()
assert test err = test >>= \case
  True -> return ()
  False -> throw err

-- | Throw a fatal error.
fatal :: Text -> a
fatal = throw . Fatal

-- | Like `fatal` but takes a list which it concatenates.
fatalC :: [Text] -> a
fatalC = fatal . concat

-- | Split up a list based on a predicate.
partitionEither :: (a -> Either b c) -> [a] -> ([b], [c])
partitionEither f = partitionEithers . map f

-- | Convert an `Either` to a `Maybe`.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x
