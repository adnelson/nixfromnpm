module Unit (main) where

import ClassyPrelude hiding ((<>))
import Data.Either (isRight, isLeft)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..), oneof)
import qualified Data.Text as T

import NixFromNpm
import NixFromNpm.Git.Types as Git
import NixFromNpm.Npm.Version.Parser as Npm

shouldBeR :: (Eq a, Eq b, Show a, Show b) => Either a b -> b -> Expectation
shouldBeR a b = a `shouldBe` Right b

shouldBeJ :: (Eq a, Show a) => Maybe a -> a -> Expectation
shouldBeJ a b = a `shouldBe` Just b

-- | Ability to parse git identifiers from URIs.
gitIdParsingSpec :: Spec
gitIdParsingSpec = describe "parse git identifiers" $ do
  describe "parse from URIs" $ do
    it "should parse a github repo" $ do
      let uri = unsafeParseURI "https://github.com/foo/bar"
      parseGitId uri `shouldBeJ` GitId Github "foo" "bar" Nothing
    it "should parse a github repo with a ref" $ do
      let uri = unsafeParseURI "https://github.com/foo/bar#baz"
      parseGitId uri `shouldBeJ` GitId Github "foo" "bar" (Just "baz")
    it "should parse a bitbucket repo" $ do
      let uri = unsafeParseURI "https://bitbucket.com/foo/bar"
      parseGitId uri `shouldBeJ` GitId Bitbucket "foo" "bar" Nothing

  describe "parse from strings" $ do
    it "should parse owner/repo as a github id" $ do
      parseGitId ("foo/bar"::String) `shouldBeJ`
        GitId Github "foo" "bar" Nothing
    it "should parse owner/repo with a tag as a github id" $ do
      parseGitId ("foo/bar#baz"::String) `shouldBeJ`
        GitId Github "foo" "bar" (Just "baz")
    it "should parse anything else as a URI" $ do
      parseGitId ("http://github.com/foo/bar#baz"::String) `shouldBeJ`
        GitId Github "foo" "bar" (Just "baz")


npmVersionParserSpec :: Spec
npmVersionParserSpec = describe "npm version parser" $ do
  describe "semvers" $ do
    it "should parse a semver" $ do
      parseNpmVersionRange "0.0.0" `shouldBeJ` SemVerRange (Eq (semver 0 0 0))

    it "should parse a tag" $ do
      parseNpmVersionRange "xyz" `shouldBeJ` NixFromNpm.Tag "xyz"

    it "should parse a github uri" $ do
      let owner = "holidaycheck"
          repo = "reactautosuggest"
          ref = SomeRef "43074a439d26243ea07110f0c5752a6fc8aebe4d"
      let uri = joinBy "/" ["https://github.com", owner, repo]
                  <> "#" <> refText ref
      parseNpmVersionRange uri `shouldBeJ`
        GitIdentifier (GitId Github owner repo (Just ref))

main :: IO ()
main = hspec $ do
  npmVersionParserSpec
  gitIdParsingSpec
