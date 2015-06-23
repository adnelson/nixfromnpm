{ mkDerivation, aeson, base, bytestring, classy-prelude
, data-default, directory, error-list, filepath, github, hspec
, hspec-expectations, http-client-streams, io-streams, MissingH
, mtl, network-uri, parsec, shelly, hnix, stdenv
, system-filepath, text, text-render, unordered-containers, pkgs
, docopt, containers
}:
mkDerivation {
  pname = "nixfromnpm";
  version = "0.1.0.7";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring classy-prelude data-default directory
    error-list filepath MissingH mtl network-uri parsec shelly
    hnix system-filepath text text-render unordered-containers
    pkgs.curl docopt containers
  ];
  testDepends = [
    aeson base bytestring classy-prelude data-default directory
    error-list filepath github hspec hspec-expectations
    http-client-streams io-streams MissingH mtl network-uri parsec
    shelly hnix system-filepath text text-render containers
    unordered-containers
  ];
  license = stdenv.lib.licenses.mit;
}
