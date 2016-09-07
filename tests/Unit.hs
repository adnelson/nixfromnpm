{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude hiding ((<>))
import Data.Either (isRight, isLeft)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..), oneof)
import qualified Data.Text as T

import NixFromNpm
import NixFromNpm.Git.Types as Git
import NixFromNpm.Npm.Version as Npm

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
    let owner = "holidaycheck" :: String
        repo = "react-autosuggest"
        ref = "43074a439d26243ea07110f0c5752a6fc8aebe4d"
    it "should parse owner/repo as a github id" $ do
      parseGitId (owner <> "/" <> repo) `shouldBeJ`
        GitId Github (pack owner) (pack repo) Nothing
    it "should parse owner/repo with a tag as a github id" $ do
      parseGitId (owner <> "/" <> repo <> "#" <> ref) `shouldBeJ`
        GitId Github (pack owner) (pack repo) (Just $ fromString ref)
    it "should allow anything except whitespace in a ref" $ do
      let ref' = "haha_!_wow%$*%_:D"
      parseGitId (owner <> "/" <> repo <> "#" <> ref') `shouldBeJ`
        GitId Github (pack owner) (pack repo) (Just $ fromString ref')
    it "should parse anything else as a URI" $ do
      parseGitId ("http://github.com/foo/bar#baz"::String) `shouldBeJ`
        GitId Github "foo" "bar" (Just "baz")


npmVersionParserSpec :: Spec
npmVersionParserSpec = describe "npm version parser" $ do
  it "should parse a semver" $ do
    parseNpmVersionRange "0.0.0" `shouldBeJ` SemVerRange (Eq (semver 0 0 0))

  it "should parse a tag" $ do
    parseNpmVersionRange "xyz" `shouldBeJ` NixFromNpm.Tag "xyz"

  it "should parse a git uri" $ do
    let owner = "holidaycheck"
        repo = "react-autosuggest"
        ref = "43074a439d26243ea07110f0c5752a6fc8aebe4d"
    let uri = joinBy "/" ["https://github.com", owner, repo]
                <> "#" <> ref
    parseNpmVersionRange uri `shouldBeJ`
      GitIdentifier (GitId Github owner repo (Just $ SomeRef ref))

  it "should parse a partial git uri" $ do
    let uri = "holidaycheck/react-autosuggest#" <>
              "43074a439d26243ea07110f0c5752a6fc8aebe4d"
    parseNpmVersionRange uri `shouldBeJ`
      GitIdentifier (GitId Github "holidaycheck" "react-autosuggest"
                     (Just "43074a439d26243ea07110f0c5752a6fc8aebe4d"))


  it "should parse a local file path" $ do
    parseNpmVersionRange "/foo/bar" `shouldBeJ` LocalPath "/foo/bar"
    parseNpmVersionRange "./foo/bar" `shouldBeJ` LocalPath "./foo/bar"
    parseNpmVersionRange "../foo/bar" `shouldBeJ` LocalPath "../foo/bar"
    parseNpmVersionRange "~/foo/bar" `shouldBeJ` LocalPath "~/foo/bar"

main :: IO ()
main = hspec $ do
--  npmVersionParserSpec
  gitIdParsingSpec
