{ mkDerivation, aeson, base, bytestring, classy-prelude, containers
, data-default, directory, docopt
, hnix, hspec, hspec-expectations
, MissingH, mtl, network-uri, parsec, shelly, stdenv
, system-filepath, text, text-render, unordered-containers
, optparse-applicative, curl, cabal-install
, temporary, SHA, pkgs, lifted-base, transformers
}:

mkDerivation {
  pname = "nixfromnpm";
  version = "0.2.3";
  src = pkgs.lib.cleanSource ./.;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring classy-prelude containers data-default
    directory docopt hnix MissingH mtl network-uri
    parsec shelly system-filepath text text-render unordered-containers
    optparse-applicative cabal-install curl SHA temporary lifted-base
    transformers
  ];
  executableHaskellDepends = with pkgs; [nix cacert];
  testDepends = [
    aeson base bytestring classy-prelude containers data-default
    directory hnix hspec hspec-expectations
    MissingH mtl network-uri parsec
    shelly system-filepath text text-render unordered-containers
  ];
  shellHook = ''
    export CURL_CA_BUNDLE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
  '';
  description = "Generate nix expressions from npm packages";
  license = stdenv.lib.licenses.mit;
}
