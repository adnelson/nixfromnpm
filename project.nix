{ mkDerivation, aeson, base, bytestring, classy-prelude, containers
, data-default, directory, docopt, error-list, filepath, github
, hnix, hspec, hspec-expectations, http-client-streams, io-streams
, MissingH, mtl, network-uri, parsec, shelly, stdenv
, system-filepath, text, text-render, unordered-containers
, optparse-applicative, curl, cabal-install, base32-bytestring
, temporary, SHA, pkgs
}:

mkDerivation {
  pname = "nixfromnpm";
  version = "0.2.2";
  src = pkgs.lib.cleanSource ./.;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring classy-prelude containers data-default
    directory docopt error-list filepath hnix MissingH mtl network-uri
    parsec shelly system-filepath text text-render unordered-containers
    optparse-applicative cabal-install curl SHA temporary
  ];
  executableHaskellDepends = with pkgs; [nix cacert];
  testDepends = [
    aeson base bytestring classy-prelude containers data-default
    directory error-list filepath github hnix hspec hspec-expectations
    http-client-streams io-streams MissingH mtl network-uri parsec
    shelly system-filepath text text-render unordered-containers
  ];
  shellHook = ''
    export CURL_CA_BUNDLE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    export BUILD_NODE_PACKAGE_DIR=${./nix-libraries/buildNodePackage}
  '';
  postInstall = ''
    source ${pkgs.makeWrapper}/nix-support/setup-hook
    wrapProgram $out/bin/nixfromnpm \
      --set BUILD_NODE_PACKAGE_DIR ${./nix-libraries/buildNodePackage}
  '';
  description = "Generate nix expressions from npm packages";
  license = stdenv.lib.licenses.mit;
}
