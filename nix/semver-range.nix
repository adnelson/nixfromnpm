{ mkDerivation, base, classy-prelude, fetchgit, hspec, parsec
, QuickCheck, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "semver-range";
  version = "0.2.6";
  src = fetchgit {
    url = "https://github.com/adnelson/semver-range";
    sha256 = "1nzfzyjy81n2x4zml4yng7vcbbxb4qyiqklc9fvfk7mgxabm1y75";
    rev = "cc94187eb2af1015cd07fb9215fe051de5841036";
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
