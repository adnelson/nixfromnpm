{ mkDerivation, base, classy-prelude, fetchgit, hspec, parsec
, QuickCheck, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "semver-range";
  version = "0.2.7";
  src = fetchgit {
    url = "https://github.com/adnelson/semver-range";
    sha256 = "02gyxd23689hs8ji6708ify0739dn6wiwqry1j3ajbk7wb3v5zr8";
    rev = "6c7073c31185ea974869dcc6d0d1f3b0335bb2d7";
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
