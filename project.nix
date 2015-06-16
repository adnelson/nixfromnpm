{ mkDerivation, aeson, base, bytestring, classy-prelude
, data-default, hspec, hspec-expectations, http-client-streams
, io-streams, MissingH, mtl, parsec, shelly, stdenv, text
, unordered-containers, pkgs, github
}:
mkDerivation {
  pname = "nixfromnpm";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring classy-prelude data-default
    http-client-streams io-streams MissingH mtl parsec shelly
    text unordered-containers pkgs.nix github pkgs.curl pkgs.cacert
  ];
  testDepends = [
    aeson base bytestring classy-prelude data-default hspec
    hspec-expectations http-client-streams io-streams MissingH mtl
    parsec shelly text unordered-containers github
  ];
  license = stdenv.lib.licenses.mit;
}
