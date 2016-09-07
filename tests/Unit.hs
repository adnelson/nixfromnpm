module Unit (main) where

import ClassyPrelude
import Data.Either (isRight, isLeft)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..), oneof)
import qualified Data.Text as T

import NixFromNpm
import NixFromNpm.Git.Types (GitRef(..), refText)
import NixFromNpm.Npm.Version.Parser

shouldBeR :: (Eq a, Eq b, Show a, Show b) => Either a b -> b -> Expectation
shouldBeR a b = a `shouldBe` Right b

npmVersionParserSpec :: Spec
npmVersionParserSpec = describe "npm version parser" $ do
  describe "semvers" $ do
    it "should parse a semver" $ do
      parseNpmVersionRange "0.0.0" `shouldBeR` SemVerRange (Eq (semver 0 0 0))

    it "should parse a tag" $ do
      parseNpmVersionRange "xyz" `shouldBeR` Tag "xyz"

    it "should parse a github uri" $ do
      let owner = "holidaycheck"
          repo = "reactautosuggest"
          ref = CommitHash "43074a439d26243ea07110f0c5752a6fc8aebe4d"
      let uri = joinBy "/" ["https://github.com", owner, repo, refText ref]
      parseNpmVersionRange uri `shouldBeR` GitId Github owner repo (Just ref)

main :: IO ()
main = hspec $ do
  npmVersionParserSpec
