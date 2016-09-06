module Main (main) where

import ClassyPrelude
import Data.Either (isRight, isLeft)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..), oneof)
import qualified Data.Text as T

import NixFromNpm

main :: IO ()
main = hspec $ do
  describe "tests" $ do
    it "should work" $ do
      True `shouldBe` True
