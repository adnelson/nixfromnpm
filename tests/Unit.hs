{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main (main) where

import ClassyPrelude hiding ((<>))
import Data.Either (isRight, isLeft)
import Data.Aeson (Value(..), decode)
import Nix.Expr
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..), oneof)
import NeatInterpolation (text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import NixFromNpm
import NixFromNpm.Common hiding (decode)
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
    parseNpmVersionRange "xyz" `shouldBeJ` Npm.Tag "xyz"

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

parsePackageMetadataSpec :: Spec
parsePackageMetadataSpec = describe "parse package metadata JSON" $ do
  let unsafeDecode = fromJust . decode . BL.fromStrict . T.encodeUtf8
  let meta = emptyPackageMeta
  it "should parse a description" $ do
    let pkgJSON = unsafeDecode [text|{"description": "hey there"}|]
    pkgJSON `shouldBe` meta {pmDescription = Just "hey there"}

  it "should parse keywords" $ do
    let pkgJSON = unsafeDecode [text|{"keywords": ["awesome", "amazing"]}|]
    pkgJSON `shouldBe` meta {pmKeywords = fromList ["awesome", "amazing"]}

  it "should parse keywords separated by commas" $ do
    let pkgJSON = unsafeDecode [text|{"keywords": "awesome, amazing"}|]
    pkgJSON `shouldBe` meta {pmKeywords = fromList ["awesome", "amazing"]}

  it "should parse platforms" $ do
    let pkgJSON = unsafeDecode [text|{"os": ["darwin", "linux"]}|]
    pkgJSON `shouldBe` meta {pmPlatforms = fromList [Darwin, Linux]}

  it "should ignore platforms it doesn't recognize" $ do
    let pkgJSON = unsafeDecode [text|{"os": ["darwin", "weird"]}|]
    pkgJSON `shouldBe` meta {pmPlatforms = fromList [Darwin]}

  it "should parse multiple keys" $ do
    let pkgJSON = unsafeDecode [text|{
      "description": "hey there",
      "keywords": ["awesome", "amazing"],
      "os": ["darwin", "weird"]
    }|]
    pkgJSON `shouldBe` meta {
      pmDescription = Just "hey there",
      pmKeywords = fromList ["awesome", "amazing"],
      pmPlatforms = fromList [Darwin]
      }


metaToNixSpec :: Spec
metaToNixSpec = describe "converting package meta to nix" $ do
  let meta = emptyPackageMeta
  it "should return nothing for an empty metadata" $ do
    metaToNix meta `shouldBe` Nothing

  it "should grab the description" $ do
    let description = "Some description"
    let converted = metaToNix (meta {pmDescription = Just description})
    fromJust converted `shouldBe` mkNonRecSet ["description" $= mkStr description]

  it "should grab the author" $ do
    let author = "Some author"
    let converted = metaToNix (meta {pmAuthor = Just author})
    fromJust converted `shouldBe` mkNonRecSet ["author" $= mkStr author]

  it "should grab the homepage" $ do
    let homepageStr = "http://example.com"
        mHomepage = parseURI (T.unpack homepageStr)
    let converted = metaToNix (meta {pmHomepage = mHomepage})
    fromJust converted `shouldBe` mkNonRecSet ["homepage" $= mkStr homepageStr]

  it "should grab keywords" $ do
    let keywords = ["keyword1", "keyword2"]
    let converted = metaToNix (meta {pmKeywords = fromList keywords})
    fromJust converted `shouldBe` mkNonRecSet ["keywords" $= mkList (mkStr <$> keywords)]

  describe "platforms" $ do
    let check ps expr = do
          let converted = metaToNix (meta {pmPlatforms = fromList ps})
          fromJust converted `shouldBe` mkNonRecSet ["platforms" $= expr]

    it "should convert a single platform" $ do
      let platforms = [Linux]
      check platforms (mkDots "pkgs" ["stdenv", "lib", "platforms"] !. "linux")

    it "should convert platforms" $ do
      let platforms = [Linux, OpenBSD]
      let withPlatforms = mkWith (mkDots "pkgs" ["stdenv", "lib", "platforms"])
      check platforms (withPlatforms ("linux" $++ "openbsd"))

main :: IO ()
main = hspec $ do
  npmVersionParserSpec
  npmNameParserSpec
  npmNameAndVersionParserSpec
  gitIdParsingSpec
  metaToNixSpec
  parsePackageMetadataSpec
