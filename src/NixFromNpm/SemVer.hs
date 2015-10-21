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

-- A SemVer has major, minor and patch versions, and zero or more
-- pre-release version tags.
type SemVer = (Int, Int, Int, [Text])

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
  deriving (Eq)

-- | Create a SemVer with no version tags.
semver :: Int -> Int -> Int -> SemVer
semver a b c = (a, b, c, [])

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
matches :: SemVerRange -> SemVer -> Bool
matches range ver = case range of
  Eq sv -> ver == sv
  Gt sv -> ver > sv
  Lt sv -> ver < sv
  Geq sv -> ver >= sv
  Leq sv -> ver <= sv
  And sv1 sv2 -> matches sv1 ver && matches sv2 ver
  Or sv1 sv2 -> matches sv1 ver || matches sv2 ver

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
