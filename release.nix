let

  config   = { allowUnfree = true; };

  overlays = [
    (newPkgs: oldPkgs: {

      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: 
          let
            inherit (newPkgs.lib) any flip elem hasSuffix hasPrefix;
            # We'll typically have a lot of files in this directory; we only want
            # to take a few of them though.
            filesToExclude = ["node_modules" "shell.nix" "project.nix" "dist" "scripts" "README.md" ];
            suffixesToExclude = ["-test"];
            filter = baseName: (!elem baseName filesToExclude) &&
                               (!hasPrefix "." baseName) &&
                               (!any (flip hasSuffix baseName) suffixesToExclude);

            src = builtins.filterSource (path: _: filter (baseNameOf path)) ./.;
          in
            { hnix =
                haskellPackagesNew.callPackage ./nix/hnix.nix { };

              semver-range = 
                haskellPackagesNew.callPackage ./nix/semver-range.nix { };

              text-render =
                haskellPackagesNew.callPackage ./nix/text-render.nix { };

              nixfromnpm =
                newPkgs.haskell.lib.overrideCabal
                  (haskellPackagesNew.callPackage ./default.nix { })
                  (oldDerivation: rec {
                    src = builtins.filterSource (path: _: filter (baseNameOf path)) oldDerivation.src;
                    shellHook = (oldDerivation.shellHook or "") + ''
                      export SRC=${src}
                      export CURL_CA_BUNDLE=${newPkgs.cacert}/etc/ssl/certs/ca-bundle.crt
                    '';
                  });
            };
      };

    })
  ];

  nixpkgs = import ./nix/17_09.nix;

  pkgs = import nixpkgs { inherit config overlays; };

in

  { inherit (pkgs.haskellPackages) nixfromnpm; }
