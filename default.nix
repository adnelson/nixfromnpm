{ mkDerivation, aeson, ansi-terminal, base, bytestring
, classy-prelude, containers, curl, data-default, data-fix
, directory, hnix, hspec, lifted-base, MissingH, monad-control, mtl
, network-uri, optparse-applicative, parsec, pcre-heavy, QuickCheck
, semver-range, SHA, shelly, stdenv, system-filepath, temporary
, text, text-render, transformers, unix, unordered-containers
}:
mkDerivation {
  pname = "nixfromnpm";
  version = "0.12.4";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring classy-prelude containers curl
    data-default data-fix directory hnix lifted-base MissingH
    monad-control mtl network-uri optparse-applicative parsec
    pcre-heavy semver-range SHA shelly system-filepath temporary text
    text-render transformers unix unordered-containers
  ];
  executableHaskellDepends = [
    aeson ansi-terminal base bytestring classy-prelude containers curl
    data-default data-fix directory hnix lifted-base MissingH
    monad-control mtl network-uri optparse-applicative parsec
    pcre-heavy semver-range SHA shelly system-filepath temporary text
    text-render transformers unix unordered-containers
  ];
  testHaskellDepends = [
    aeson ansi-terminal base bytestring classy-prelude containers curl
    data-default data-fix directory hnix hspec lifted-base MissingH
    monad-control mtl network-uri optparse-applicative parsec
    pcre-heavy QuickCheck semver-range SHA shelly system-filepath
    temporary text text-render transformers unix unordered-containers
  ];
  description = "Generate nix expressions from npm packages";
  license = stdenv.lib.licenses.mit;
}
