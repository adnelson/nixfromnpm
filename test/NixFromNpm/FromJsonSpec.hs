{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module NixFromNpm.FromJsonSpec (spec) where

import SpecHelper
import NixFromNpm.NpmLookup
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

spec :: Spec
spec = describe "truth" $ do
  it "should be true" $ do
    foo <- B.readFile "foo.txt"
    print foo
    True `shouldBe` True
