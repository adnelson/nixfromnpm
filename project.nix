{ mkDerivation, aeson, base, bytestring, classy-prelude, containers
, data-default, directory, unix, hnix, hspec, hspec-expectations
, MissingH, mtl, network-uri, parsec, shelly, stdenv
, system-filepath, text, text-render, unordered-containers
, optparse-applicative, curl, cabal-install, ansi-terminal
, temporary, SHA, pkgs, lifted-base, transformers, semver-range
}:

let
  inherit (builtins) filterSource;
  inherit (pkgs.lib) elem;
in

mkDerivation {
  pname = "nixfromnpm";
  version = "0.5.0";
  # Filter .git and dist files from source
  src = filterSource (n: t: !(elem n [".git" "dist"])) ./.;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring classy-prelude containers data-default
    directory hnix MissingH mtl network-uri parsec shelly
    system-filepath text text-render unordered-containers
    optparse-applicative cabal-install curl SHA temporary lifted-base
    transformers ansi-terminal semver-range
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
    export semver=${semver-range}
    export hnix=${hnix}
  '';
  description = "Generate nix expressions from npm packages";
  license = stdenv.lib.licenses.mit;
}
