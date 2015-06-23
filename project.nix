{ mkDerivation, aeson, base, bytestring, classy-prelude
, data-default, directory, error-list, filepath, github, hspec
, hspec-expectations, http-client-streams, io-streams, MissingH
, mtl, network-uri, parsec, shelly, simple-nix, stdenv
, system-filepath, text, text-render, unordered-containers, pkgs
, docopt
}:
mkDerivation {
  pname = "nixfromnpm";
  version = "0.1.0.6";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring classy-prelude data-default directory
    error-list filepath MissingH mtl network-uri parsec shelly
    simple-nix system-filepath text text-render unordered-containers
    pkgs.curl docopt
  ];
  testDepends = [
    aeson base bytestring classy-prelude data-default directory
    error-list filepath github hspec hspec-expectations
    http-client-streams io-streams MissingH mtl network-uri parsec
    shelly simple-nix system-filepath text text-render
    unordered-containers
  ];
  license = stdenv.lib.licenses.mit;
}
