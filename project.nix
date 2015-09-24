{ mkDerivation, aeson, base, bytestring, classy-prelude, containers
, data-default, directory, docopt, error-list, filepath, github
, hnix, hspec, hspec-expectations, http-client-streams, io-streams
, MissingH, mtl, network-uri, parsec, shelly, stdenv
, system-filepath, text, text-render, unordered-containers
, optparse-applicative, curl, cacert, cabal-install
, Cabal, safe, QuickCheck
}:

let
  dequeue = mkDerivation {
    pname = "dequeue";
    version = "0.1.12";
    sha256 = "c70aedbb1965affe07b7151f12e9a8e42f2cb54652bb0a0bbc5d0ba8e21721ab";
    doCheck = false;
    libraryHaskellDepends = [ base QuickCheck safe ];
    description = "A typeclass and an implementation for double-ended queues";
    license = stdenv.lib.licenses.bsd3;
    hydraPlatforms = stdenv.lib.platforms.none;
  };
in
mkDerivation {
  pname = "nixfromnpm";
  version = "0.2.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring classy-prelude containers data-default
    directory docopt error-list filepath hnix MissingH mtl network-uri
    parsec shelly system-filepath text text-render unordered-containers
    optparse-applicative cabal-install curl dequeue
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
