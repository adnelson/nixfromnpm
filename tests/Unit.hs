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
import NixFromNpm.Npm.PackageMap (PackageName(..), parsePackageName)
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

npmNameParserSpec :: Spec
npmNameParserSpec = describe "npm package name parser" $ do
  it "should parse a name without a namespace" $ do
    parsePackageName "foo" `shouldBeR` PackageName "foo" Nothing

  it "should parse a name with a namespace" $ do
    parsePackageName "@foo/bar" `shouldBeR` PackageName "bar" (Just "foo")

  it "should not parse a name with an empty namespace" $ do
    parsePackageName "@/bar" `shouldSatisfy` isLeft

  it "should not parse a name with a multiple namespaces" $ do
    parsePackageName "@foo/@bar" `shouldSatisfy` isLeft
    parsePackageName "@foo/@bar/baz" `shouldSatisfy` isLeft

  describe "allow numbers" $ do
    it "in package name without a namespace" $ do
      parsePackageName "bar123" `shouldBeR` "bar123"
    it "in package name with a namespace" $ do
      parsePackageName "@foo/bar12" `shouldBeR` PackageName "bar12" (Just "foo")
    it "in a namespace" $ do
      parsePackageName "@foo12/bar" `shouldBeR` PackageName "bar" (Just "foo12")

  it "should allow certain special characters" $ do
    parsePackageName "@foo/bar!" `shouldBeR` PackageName "bar!" (Just "foo")

  it "should not allow other special characters" $ do
    parsePackageName "@foo/bar@/" `shouldSatisfy` isLeft

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

npmNameAndVersionParserSpec :: Spec
npmNameAndVersionParserSpec = describe "npm name@version parser" $ do
  it "should parse a name with no version range" $ do
    (name, range) <- parseNameAndRange "foo"
    name `shouldBe` "foo"
    range `shouldBe` SemVerRange anyVersion

  it "should parse a namespaced name with no version range" $ do
    (name, range) <- parseNameAndRange "@foo/bar"
    name `shouldBe` PackageName "bar" (Just "foo")
    range `shouldBe` SemVerRange anyVersion

  it "should parse a name and a version range" $ do
    (name, range) <- parseNameAndRange "foo@1.2.3"
    name `shouldBe` "foo"
    range `shouldBe` SemVerRange (Eq $ semver 1 2 3)

  it "should parse a namespaced name and a version range" $ do
    (name, range) <- parseNameAndRange "@foo/bar@1.2.3"
    name `shouldBe` PackageName "bar" (Just "foo")
    range `shouldBe` SemVerRange (Eq $ semver 1 2 3)

  it "should warn if the %-format is used" $ do
    parseNameAndRange "foo%1.2.3" `shouldThrow` \(UnrecognizedVersionFormat msg) -> do
      "use '@' instead" `isInfixOf` msg

main :: IO ()
main = hspec $ do
  npmVersionParserSpec
  npmNameParserSpec
  npmNameAndVersionParserSpec
  gitIdParsingSpec
