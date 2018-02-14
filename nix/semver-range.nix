{ mkDerivation, base, classy-prelude, fetchgit, hspec, parsec
, QuickCheck, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "semver-range";
  version = "0.2.6";
  src = fetchgit {
    url = "https://github.com/adnelson/semver-range";
    sha256 = "0zfj8l5jhbbsjxw851j1qh5csfvvyczp4djvl77jf9k0awhb2p8g";
    rev = "1da577c3d5648d52961e085d2901e464805e1dff";
  };
  libraryHaskellDepends = [
    base classy-prelude parsec text unordered-containers
  ];
  testHaskellDepends = [
    base classy-prelude hspec parsec QuickCheck text
    unordered-containers
  ];
  description = "An implementation of semver and semantic version ranges";
  license = stdenv.lib.licenses.mit;
}
