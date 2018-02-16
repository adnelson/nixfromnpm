/*
A set of tools for generating node packages, such as to be imported by
default.nix files generated by nixfromnpm.
*/

# NOTE: this should be removed when backwards compatibility breaking
# changes are allowed to be made to the top-level generated
# default.nix

{
  # Self-reference so that we can pass through to downstream libraries
  self
}:

{
  # Base set of packages, i.e. nixpkgs.
  pkgs,
  # nodejs derivation.
  nodejs ? pkgs.nodejs-8_x,
} @ args:

let

  inherit (pkgs.lib) extends makeExtensible makeOverridable;

  # Function to replace dots with something
  replaceDots = c: replaceChars ["."] [c];
  inherit (builtins) readDir removeAttrs length getEnv elemAt hasAttr;
  inherit (pkgs.lib) attrNames attrValues filterAttrs flip foldl
                     hasSuffix hasPrefix removeSuffix replaceChars
                     optional optionals stringToCharacters
                     concatStrings tail splitString;
  inherit (pkgs.stdenv) isLinux;

  # Function to remove the first character of a string.
  dropFirstChar = str: concatStrings (tail (stringToCharacters str));

  # Concatenate a list of sets.
  joinSets = foldl (a: b: a // b) {};

  # Parse the `NPM_AUTH_TOKENS` environment variable to discover
  # namespace-token associations and turn them into an attribute set
  # which we can use as an input to the fetchPrivateNpm function.
  # Split the variable on ':', then turn each k=v element in
  # the list into an attribute set and join all of those sets.
  namespaceTokens = joinSets (
    flip map (splitString ":" (getEnv "NPM_AUTH_TOKENS")) (kvPair:
      let kv = splitString "=" kvPair; in
      if length kv != 2 then {}
      else {"${elemAt kv 0}" = elemAt kv 1;}));

  # A function similar to fetchUrl but allows setting of custom headers.
  fetchUrlNamespaced = pkgs.callPackage ./fetchUrlNamespaced.nix {
    inherit namespaceTokens;
  };

  fetchUrlWithHeaders = fetchUrlNamespaced;

  xcode-wrapper = let
    xcodeBaseDir = if getEnv "XCODE_BASE_DIR" != ""
                   then getEnv "XCODE_BASE_DIR"
                   else "/Applications/Xcode.app";
  in pkgs.stdenv.mkDerivation {
    name = "xcode-wrapper";
    buildCommand = ''
      # Wrap ln -s with logic checking that target exists.
      link() {
        if [[ -e $1 ]]; then
          ln -s "$1" $2
        elif [[ $2 == if_exists ]]; then
          echo "WARNING: $1 doesn't exist, something might not work." >&2
        else
          echo "Error: $1 does not exist. Check that XCode is installed." >&2
          exit 1
        fi
      }
      mkdir -p $out/bin
      cd $out/bin
      link /usr/bin/xcode-select
      link /usr/bin/security
      link /usr/bin/codesign
      link /usr/bin/xcodebuild
      link /usr/bin/xcrun
      link "${xcodeBaseDir}/Contents/Developer/Applications/Simulator.app/Contents/MacOS/Simulator" if_exists

      cd $out
      link "${xcodeBaseDir}/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs" if_exists

      # Check that xcodebuild can run, as a canary for a working result
      if ! $out/bin/xcodebuild -version; then
        echo "xcodebuild does not seem to be working. :(" >&2
        exit 1
      fi
    '';
  };
  # Directory containing build tools for buildNodePackage
  node-build-tools = pkgs.stdenv.mkDerivation {
    name = "node-build-tools";
    buildInputs = [pkgs.makeWrapper];
    buildCommand = ''
      mkdir -p $out
      cp -r ${./tools} $out/bin
      chmod -R +w $out/bin
      wrapProgram $out/bin/check-package-json \
        --set SEMVER_PATH ${nodejs}/lib/node_modules/npm/node_modules/semver
      patchShebangs $out/bin
    '';
  };

  # A script performing various sanity/correctness checks on the package.json
  checkPackageJson = pkgs.writeScript "checkPackageJson" ''
    #!${pkgs.stdenv.shell}
    export SEMVER_PATH=${nodejs}/lib/node_modules/npm/node_modules/semver
    exec ${nodejs}/bin/node ${./tools/check-package-json} "$@"
  '';

  # A script which will install all of the binaries a package.json
  # declares into the output folder.
  installPackageJsonBinaries = pkgs.writeScript "installPackageJsonBinaries" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.python2.interpreter} ${./tools/install-binaries} "$@"
  '';
in

rec {
  inherit nodejs;

  buildNodePackage = import ./buildNodePackage.nix {
    inherit pkgs nodejs buildNodePackage xcode-wrapper node-build-tools;
  };
  # A generic package that will fail to build. This is used to indicate
  # packages that are broken, without failing the entire generation of
  # a package expression.
  brokenPackage = {name, reason}:
    let
     deriv = pkgs.stdenv.mkDerivation {
        name = "BROKEN-${name}";
        buildCommand = ''
          echo "Package ${name} is broken: ${reason}"
          exit 1
        '';
        passthru.withoutTests = deriv;
        passthru.pkgName = name;
        passthru.basicName = "BROKEN";
        passthru.uniqueName = "BROKEN";
        passthru.overrideNodePackage = (_: (_: deriv));
        passthru.namespace = null;
        passthru.version = "BROKEN";
        passthru.override = _: deriv;
        passthru.recursiveDeps = [];
        passthru.peerDependencies = {};
      };
    in
    deriv;

  # The function that a default.nix can call into which will scan its
  # directory for all of the package files and generate a big attribute set
  # for all of them. Re-exports the `callPackage` function and all of the
  # attribute sets, as well as the nodeLib.
  #
  # We use `lib.makeOverridable` so that this function's result can be
  # overridden in the same way that we can override the result from a
  # `callPackage` invocation. This is intended to pave the way for a
  # non-backwards compatible refactor to provide a simpler interface
  # that resembles other overridable language package sets
  # (e.g. haskellPackages).
  #
  # In truth, this whole file should be refactored so that it only
  # provides "libs" and then have a make-packages.nix file that can be
  # `callPackage`'ed from the `default.nix` one level up from the
  # `nodeLibs`; the intake arguments would a union of the arguments to
  # this file and to this generatePackages function.
  generatePackages = makeOverridable ({

    # Path to find node packages in.
    nodePackagesPath,

    # Extensions are other node libraries which will be folded into the
    # generated one.
    # 
    # This is deprecated, the overrides argument should be used
    # instead and if there are multiple package sets to give they can
    # be composed first with `composeExtensions`.
    #
    # Note that any overrides provided via the `overrides` argument
    # will override any package of the same name from the union of all
    # package sets given in `extensions`.
    extensions ? [],

    overrides ? (self: super: {}),

    # If any additional arguments should be made available to callPackage
    # (for example for packages which require additional arguments), they
    # can be passed in here. Those packages can declare an `extras` argument
    # which will contain whatever is passed in here.
    extras ? {}
  }:
    let

      mkScope = scope: ({
        inherit fetchUrlNamespaced fetchUrlWithHeaders namespaceTokens;
        inherit pkgs buildNodePackage brokenPackage extras;
      } // scope);

      callPackage = pkgs.newScope (mkScope {
        inherit nodePackages;
        inherit (nodePackages) namespaces;
      });

      initialNodePackages = self: 
        let
          oldExtensions = joinSets (map (e: e.nodePackages) extensions);
          packageSet = pkgs.callPackage nodePackagesPath { 
            callPackage = pkgs.newScope (mkScope {
              nodePackages = self;
              inherit (self) namespaces;
            });
          };
        in
          oldExtensions // packageSet;

      nodePackages = makeExtensible (extends overrides initialNodePackages);

    in
      { inherit callPackage namespaceTokens pkgs node-build-tools;
        nodePackages = nodePackages // {inherit nodejs;};
        nodeLib = self args;
      });
}
