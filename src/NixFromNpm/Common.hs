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
    module GHC.Exts,
    module Filesystem.Path.CurrentOS,
    module Network.URI,
    Name, Record,
    tuple, tuple3, fromRight, cerror, uriToText, slash,
    putStrsLn
  ) where

import ClassyPrelude hiding (assert, asList, find, FilePath)
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
import Control.Monad.Identity (Identity(..))
import Control.Applicative hiding (empty, optional)
import Data.Char (isDigit)
import Data.Default
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Either (isRight, isLeft)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS (FilePath, fromText)
import GHC.Exts (IsList)
import Network.URI (URI(..), parseURI, parseAbsoluteURI,
                    parseRelativeReference, uriToString, relativeTo)

-- | Indicates that the text is some identifier.
type Name = Text

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
  let newPairs = P.map (\(k, v) -> (f k, v)) pairs
  let newMap = H.fromList newPairs
  newMap

cerror :: [String] -> a
cerror = error . concat

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left err) = error "Expected `Right` value"

uriToText :: URI -> Text
uriToText uri = pack $ uriToString id uri ""

-- | Appends text to URI with a slash. Ex: foo.com `slash` bar == foo.com/bar.
slash :: URI -> Text -> URI
slash uri txt = case parseRelativeReference (unpack txt) of
  Nothing -> error ("Invalid appending URI: " <> show txt)
  Just uri' -> uri' `relativeTo` uri

infixl 6 `slash`

putStrsLn :: MonadIO m => [Text] -> m ()
putStrsLn = putStrLn . concat
