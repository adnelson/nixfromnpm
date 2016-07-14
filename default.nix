{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let
  haskellPackages = pkgs.pkgs.haskell.packages."${compiler}";

  hnix = with haskellPackages; mkDerivation rec {
    pname = "hnix";
    version = "0.3.0";
    src = pkgs.fetchurl {
      url = "https://api.github.com/repos/jwiegley/hnix/tarball/${version}";
      name = "hnix-${version}.tar.gz";
      sha256 = "18331v0pf39l7brihjqqcc0krrd0iaw91r66ysmqnxgqjk0qliar";
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
    version = "0.2.0";
    src = pkgs.fetchurl {
      url = "https://api.github.com/repos/adnelson/semver-range/tarball/${version}";
      name = "semver-range-${version}.tar.gz";
      sha256 = "1h40ww6m8lx8nx73550dd0r4iig4nrlgsmbn6zc174kc6lsd3m3b";
    };
    isLibrary = true;
    buildDepends = [ base classy-prelude parsec text cabal-install
                     unordered-containers ];
    description = "An implementation of semver and semantic version ranges";
    license = pkgs.lib.licenses.mit;
  };

in

haskellPackages.callPackage ./project.nix {
  inherit pkgs hnix semver-range;
}
