{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module NixFromNpm.SemVer where

import qualified Prelude as P
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson.Parser
import Data.Aeson
import Data.Aeson.Types (typeMismatch)

import NixFromNpm.Common hiding (Any)

type ReleaseTag = Text

-- A SemVer has major, minor and patch versions, and zero or more
-- pre-release version tags.
type SemVer = (Int, Int, Int, [ReleaseTag])

-- | A partially specified semantic version. Implicitly defines
-- a range of acceptable versions, as seen in @wildcardToRange@.
data Wildcard = Any
              | One Int
              | Two Int Int
              | Three Int Int Int [Text]
              deriving (Show, Eq)

-- | A range specifies bounds on a semver.
data SemVerRange
  = Eq SemVer                   -- ^ Exact equality
  | Gt SemVer                   -- ^ Greater than
  | Lt SemVer                   -- ^ Less than
  | Geq SemVer                  -- ^ Greater than or equal to
  | Leq SemVer                  -- ^ Less than or equal to
  | And SemVerRange SemVerRange -- ^ Conjunction
  | Or SemVerRange SemVerRange  -- ^ Disjunction
  deriving (Eq, Ord)

-- | Pull all of the concrete versions out of a range.
versionsOf :: SemVerRange -> [SemVer]
versionsOf = \case
  Eq sv -> [sv]
  Geq sv -> [sv]
  Leq sv -> [sv]
  Lt sv -> [sv]
  Gt sv -> [sv]
  And svr1 svr2 -> versionsOf svr1 <> versionsOf svr2
  Or svr1 svr2 -> versionsOf svr1 <> versionsOf svr2

-- | Create a SemVer with no version tags.
semver :: Int -> Int -> Int -> SemVer
semver a b c = (a, b, c, [])

releaseTags :: SemVer -> [ReleaseTag]
releaseTags (_, _, _, tags) = tags

toTuple :: SemVer -> (Int, Int, Int)
toTuple (a, b, c, _) = (a, b, c)

tuplesOf :: SemVerRange -> [(Int, Int, Int)]
tuplesOf = map toTuple . versionsOf

rangeReleaseTags :: SemVerRange -> [ReleaseTag]
rangeReleaseTags = concatMap releaseTags . versionsOf

-- | Satisfies any version.
anyVersion :: SemVerRange
anyVersion = Gt $ semver 0 0 0

-- | Render a semver as Text.
renderSV :: SemVer -> Text
renderSV = pack . renderSV'

-- | Render a semver as a String.
renderSV' :: SemVer -> String
renderSV' (x, y, z, []) = show x <> "." <> show y <> "." <> show z
renderSV' (x, y, z, tags) = renderSV' (x, y, z, []) <> "-" <>
                              (intercalate "." $ map unpack tags)

instance Show SemVerRange where
  show = \case
    Eq sv -> "=" <> renderSV' sv
    Gt sv -> ">" <> renderSV' sv
    Lt sv -> "<" <> renderSV' sv
    Geq sv -> ">=" <> renderSV' sv
    Leq sv -> "<=" <> renderSV' sv
    And svr1 svr2 -> show svr1 <> " " <> show svr2
    Or svr1 svr2 -> show svr1 <> " || " <> show svr2

-- | Returns whether a given semantic version matches a range.
-- Note that there are special cases when there are release tags. For detauls
-- see https://github.com/npm/node-semver#prerelease-tags.
matches :: SemVerRange -> SemVer -> Bool
matches range ver = case (rangeReleaseTags range, releaseTags ver) of
  -- This is the simple case, where neither the range nor the version has given
  -- any release tags. Then we can just do regular predicate calculus.
  ([], []) -> matchesSimple range ver
  -- If the version has release tags but the range doesn't, it's not a match.
  ([], _) -> False
  -- If the version has release tags but none of the comparators have the same
  -- (major, minor, patch) tuples, it's not a match. For now, we're being even
  -- more strict, saying that there has to be exactly one comparator as well.
  (_, _) | tuplesOf range /= [toTuple ver] -> False
  -- If they have the same tuples, we can compare the version tags.
  (rTags, vTags) -> matchesTags range rTags vTags

-- | Simple predicate calculus matching, doing AND and OR combination with
-- numerical comparison.
matchesSimple :: SemVerRange -> SemVer -> Bool
matchesSimple range ver = case range of
  Eq sv -> ver == sv
  Gt sv -> ver > sv
  Lt sv -> ver < sv
  Geq sv -> ver >= sv
  Leq sv -> ver <= sv
  And sv1 sv2 -> matchesSimple sv1 ver && matchesSimple sv2 ver
  Or sv1 sv2 -> matchesSimple sv1 ver || matchesSimple sv2 ver


matchesTags :: SemVerRange -> [ReleaseTag] -> [ReleaseTag] -> Bool
matchesTags range rangeTags verTags = case range of
  Gt _ -> verTags > rangeTags
  Lt _ -> verTags < rangeTags
  Geq _ -> verTags >= rangeTags
  Leq _ -> verTags <= rangeTags
  -- Note that as we're currently doing things, these cases won't get hit.
  And svr1 svr2 -> matchesTags svr1 verTags rangeTags &&
                     matchesTags svr2 verTags rangeTags
  Or svr1 svr2 -> matchesTags svr1 verTags rangeTags ||
                     matchesTags svr2 verTags rangeTags

weakenEquality :: SemVerRange -> SemVerRange
weakenEquality (Eq (maj, min, _, [])) = wildcardToRange (Two maj min)
weakenEquality rng = rng

-- | Gets the highest-matching semver in a range.
bestMatch :: SemVerRange -> [SemVer] -> Either String SemVer
bestMatch range vs = case filter (matches range) vs of
  [] -> Left "No matching versions"
  vs -> Right $ maximum vs

-- | Fills in zeros in a wildcard.
wildcardToSemver :: Wildcard -> SemVer
wildcardToSemver Any = (0, 0, 0, [])
wildcardToSemver (One n) = (n, 0, 0, [])
wildcardToSemver (Two n m) = (n, m, 0, [])
wildcardToSemver (Three n m o tags) = (n, m, o, tags)

-- | Translates a wildcard (partially specified version) to a range.
-- Ex: 2 := >=2.0.0 <3.0.0
-- Ex: 1.2.x := 1.2 := >=1.2.0 <1.3.0
wildcardToRange :: Wildcard -> SemVerRange
wildcardToRange = \case
  Any -> Geq (0, 0, 0, [])
  One n -> Geq (n, 0, 0, []) `And` Lt (n+1, 0, 0, [])
  Two n m -> Geq (n, m, 0, []) `And` Lt (n, m + 1, 0, [])
  Three n m o tags -> Eq (n, m, o, tags)

-- | Translates a ~wildcard to a range.
-- Ex: ~1.2.3 := >=1.2.3 <1.(2+1).0 := >=1.2.3 <1.3.0
tildeToRange :: Wildcard -> SemVerRange
tildeToRange = \case
  Any -> tildeToRange (Three 0 0 0 [])
  One n -> tildeToRange (Three n 0 0 [])
  Two n m -> tildeToRange (Three n m 0 [])
  Three n m o tags -> And (Geq (n, m, o, tags)) (Lt (n, m + 1, 0, tags))

-- | Translates a ^wildcard to a range.
-- Ex: ^1.2.x := >=1.2.0 <2.0.0
caratToRange :: Wildcard -> SemVerRange
caratToRange = \case
  One n -> And (Geq (n, 0, 0, [])) (Lt (n+1, 0, 0, []))
  Two n m -> And (Geq (n, m, 0, [])) (Lt (n+1, 0, 0, []))
  Three 0 0 n tags -> Eq (0, 0, n, tags)
  Three 0 n m tags -> And (Geq (0, n, m, tags)) (Lt (0, n + 1, 0, tags))
  Three n m o tags -> And (Geq (n, m, o, tags)) (Lt (n+1, 0, 0, tags))

-- | Translates two hyphenated wildcards to an actual range.
-- Ex: 1.2.3 - 2.3.4 := >=1.2.3 <=2.3.4
-- Ex: 1.2 - 2.3.4 := >=1.2.0 <=2.3.4
-- Ex: 1.2.3 - 2 := >=1.2.3 <3.0.0
hyphenatedRange :: Wildcard -> Wildcard -> SemVerRange
hyphenatedRange wc1 wc2 = And sv1 sv2 where
  sv1 = case wc1 of Any -> Geq (0, 0, 0, [])
                    One n -> Geq (n, 0, 0, [])
                    Two n m -> Geq (n, m, 0, [])
                    Three n m o tags -> Geq (n, m, o, tags)
  sv2 = case wc2 of Any -> Geq (0, 0, 0, []) -- Refers to "any version"
                    One n -> Lt (n+1, 0, 0, [])
                    Two n m -> Lt (n, m + 1, 0, [])
                    Three n m o tags -> Leq (n, m, o, tags)
