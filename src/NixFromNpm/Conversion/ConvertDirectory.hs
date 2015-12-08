-- Convert a directory full of nix files to a single expression. This is a
-- tradeoff with the other option, which is to discover the packages at the
-- time nix expressions are evaluated, using code written in nix. The problem
-- is that that process is very slow, so we front-load the task here.
--
-- Essentially, given a directory structure like this:
-- foo/
--   0.1.2.nix
--   0.2.3.nix
-- bar/
--   1.2.3.nix
-- @mynamespace/
--   qux/
--     3.4.5.nix
--     default.nix
--
-- We would generate a nix file that looks like this:
--
-- {pkgs, nodejsVersion, callPackage, npm3 ? true}:
--
-- {
--   foo_0-1-2 = callPackage ./foo/0.1.2.nix {};
--   foo_0-2-3 = callPackage ./foo/0.2.3.nix {};
--   foo = callPackage ./foo/0.2.3.nix {};
--   bar_1-2-3 = callPackage ./bar/1.2.3.nix {};
--   bar = callPackage ./bar/1.2.3.nix {};
--   namespaces' = {
--     mynamespace = {
--       qux_3-4-5 = callPackage ./@mynamespace/qux/3.4.5.nix {};
--       qux = callPackage ./@mynamespace/qux/3.4.5.nix {};
--     };
--   };
-- }
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module NixFromNpm.Conversion.ConvertDirectory where


import NixFromNpm.Common
import NixFromNpm.Conversion.ToDisk (parseVersionFiles)
import Data.SemVer

-- | Given a packagemap full of packages, convert it to a nix file.
packageMapToNix :: PackageMap a -> NExpr
packageMapToNix = undefined
