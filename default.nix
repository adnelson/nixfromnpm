# Expression to build nixfromnpm, wrapping the 'project.nix' which
# defines nixfromnpm itself. Note that we define custom expressions
# for hnix and semver-range, fixed at a particular version, so that we
# can ensure future changes to these packages will not break the
# build.

{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  haskellPackages = pkgs.pkgs.haskell.packages."${compiler}";

  hnix = with haskellPackages; mkDerivation rec {
    pname = "hnix";
    version = "0.3.1";
    src = pkgs.fetchurl {
      url = "https://api.github.com/repos/jwiegley/hnix/tarball/${version}";
      name = "hnix-${version}.tar.gz";
      sha256 = "1djb69ksnxp21y0m0b6gh950i58kl0c34j1zw5w2s4v6z1bfjmds";
    };
    isLibrary = true;
    isExecutable = true;
    buildDepends = [
      ansi-wl-pprint base containers data-fix parsers text transformers
      trifecta unordered-containers cabal-install criterion
    ];
    testDepends = [
      base containers data-fix tasty tasty-hunit tasty-th text
    ];
    homepage = "http://github.com/jwiegley/hnix";
    description = "Haskell implementation of the Nix language";
    license = pkgs.lib.licenses.bsd3;
  };

  semver-range = with haskellPackages; mkDerivation rec {
    pname = "semver-range";
    version = "0.2.5";
    src = pkgs.fetchurl {
      url = "https://api.github.com/repos/adnelson/semver-range/tarball/${version}";
      name = "semver-range-${version}.tar.gz";
      sha256 = "1hapz05mz1yfv3nw7ka0hdcx05qilp7ny65lybaf7hh5pv4nwlfb";
    };
    isLibrary = true;
    buildDepends = [ base classy-prelude parsec text cabal-install QuickCheck
                     unordered-containers hspec ];
    description = "An implementation of semver and semantic version ranges";
    license = pkgs.lib.licenses.mit;
  };

in

haskellPackages.callPackage ./project.nix {
  inherit pkgs hnix semver-range;
}
