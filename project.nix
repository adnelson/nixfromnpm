{ mkDerivation, aeson, base, bytestring, classy-prelude, containers
, data-default, directory, docopt, error-list, filepath, github
, hnix, hspec, hspec-expectations, http-client-streams, io-streams
, MissingH, mtl, network-uri, parsec, shelly, stdenv
, system-filepath, text, text-render, unordered-containers
, optparse-applicative, curl, cacert, cabal-install
}:
mkDerivation {
  pname = "nixfromnpm";
  version = "0.2.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring classy-prelude containers data-default
    directory docopt error-list filepath hnix MissingH mtl network-uri
    parsec shelly system-filepath text text-render unordered-containers
    optparse-applicative cabal-install curl
  ];
  executableHaskellDepends = [curl cacert];
  testDepends = [
    aeson base bytestring classy-prelude containers data-default
    directory error-list filepath github hnix hspec hspec-expectations
    http-client-streams io-streams MissingH mtl network-uri parsec
    shelly system-filepath text text-render unordered-containers
  ];
  shellHook = ''
    export CURL_CA_BUNDLE=${cacert}/etc/ssl/certs/ca-bundle.crt
  '';
  description = "Generate nix expressions from npm packages";
  license = stdenv.lib.licenses.mit;
}
