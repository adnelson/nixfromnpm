{ mkDerivation, aeson, base, bytestring, classy-prelude, containers
, data-default, directory, unix, hnix, hspec, hspec-expectations
, MissingH, mtl, network-uri, parsec, shelly, stdenv
, system-filepath, text, text-render, unordered-containers
, optparse-applicative, curl, cabal-install, ansi-terminal
, temporary, SHA, pkgs, lifted-base, transformers, semver-range
}:

let
  inherit (builtins) filterSource;
  inherit (pkgs.lib) any flip elem hasSuffix hasPrefix;
  # We'll typically have a lot of files in this directory; we only want
  # to take a few of them though.
  filesToExclude = ["node_modules" "shell.nix" "project.nix" "dist" "scripts"];
  suffixesToExclude = ["-test"];
  filter = baseName: (!elem baseName filesToExclude) &&
                     (!hasPrefix "." baseName) &&
                     (!any (flip hasSuffix baseName) suffixesToExclude);
  src = filterSource (path: _: filter (baseNameOf path)) ./.;
in

mkDerivation {
  inherit src;
  pname = "nixfromnpm";
  version = "0.10.1";
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
    export SRC=${src}
    export CURL_CA_BUNDLE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
  '';
  description = "Generate nix expressions from npm packages";
  license = stdenv.lib.licenses.mit;
}
