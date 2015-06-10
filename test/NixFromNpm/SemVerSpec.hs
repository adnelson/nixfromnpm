{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module NixFromNpm.SemVerSpec (spec) where

import SpecHelper

spec :: Spec
spec = describe "truth" $ do
  it "should be true" $ do
    True `shouldBe` True
