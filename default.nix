{ mkDerivation, aeson, ansi-terminal, base, bytestring
, classy-prelude, containers, curl, data-default, data-fix
, directory, exceptions, hnix, hspec, lifted-base, MissingH
, monad-control, mono-traversable, mtl, neat-interpolation
, network-uri, optparse-applicative, parsec, pcre-heavy, QuickCheck
, semver-range, SHA, shelly, stdenv, system-filepath, temporary
, text, text-render, transformers, unix, unordered-containers
}:
mkDerivation {
  pname = "nixfromnpm";
  version = "0.13.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring classy-prelude containers curl
    data-default data-fix directory exceptions hnix lifted-base
    MissingH monad-control mono-traversable mtl network-uri
    optparse-applicative parsec pcre-heavy semver-range SHA shelly
    system-filepath temporary text text-render transformers unix
    unordered-containers
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [
    aeson base bytestring classy-prelude hnix hspec mono-traversable
    neat-interpolation QuickCheck text
  ];
  description = "Generate nix expressions from npm packages";
  license = stdenv.lib.licenses.mit;
}
