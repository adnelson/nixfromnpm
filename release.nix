{ nixpkgs ? import ./nix/17_09.nix }:
let
  config   = { allowUnfree = true; };

  overlays = [
    (newPkgs: oldPkgs: {

      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld:
            { semver-range =
                haskellPackagesNew.callPackage ./nix/semver-range.nix { };

              text-render =
                haskellPackagesNew.callPackage ./nix/text-render.nix { };

              nixfromnpm =
                let
                  inherit (newPkgs.lib) any flip elem hasSuffix hasPrefix elemAt splitString;
                  # We'll typically have a lot of files in this directory;
                  # we only want to take a few of them though. Make a filtering
                  # function which will choose them.
                  dirsToInclude = ["src" "tests" "nix-libs"];
                  filesToInclude = ["LICENSE" "nixfromnpm.cabal"];
                  _filter = path: type: let
                    # NOTE: using PWD here is hacky; means its
                    # behavior depends on the directory the command is
                    # being executed from. Fix me!
                    subpath = elemAt (splitString "${builtins.getEnv "PWD"}/" path) 1;
                    spdir = elemAt (splitString "/" subpath) 0;
                  in elem spdir dirsToInclude ||
                     (type == "regular" && elem subpath filesToInclude);
                in
                newPkgs.haskell.lib.overrideCabal
                  (haskellPackagesNew.callPackage ./default.nix { })
                  (oldDerivation: rec {
                    src = builtins.filterSource _filter oldDerivation.src;
                    shellHook = builtins.trace src ((oldDerivation.shellHook or "") + ''
                      export SRC=${src}
                      export CURL_CA_BUNDLE=${newPkgs.cacert}/etc/ssl/certs/ca-bundle.crt
                      export NIX_LIBS_DIR=$PWD/nix-libs
                    '');
                  });
            };
      };

    })
  ];

  pkgs = import nixpkgs { inherit config overlays; };

in

  { inherit (pkgs.haskellPackages) nixfromnpm; inherit pkgs; }
