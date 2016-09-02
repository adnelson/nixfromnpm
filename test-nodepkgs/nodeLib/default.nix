/*
A set of tools for generating node packages, such as to be imported by
default.nix files generated by nixfromnpm.
*/

{
  # Self-reference so that we can pass through to downstream libraries
  self
}:

{
  # Base set of packages, i.e. nixpkgs.
  pkgs,
  # nodejs derivation.
  nodejs ? pkgs.nodejs-4_x,
  # Whether to use npm3 (requires a prebuilt tarball of npm3).
  npm3 ? true
} @ args:

let
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

  # Builds the extracted nix file. Since of course it can't use npm3,
  # being that it hasn't been built yet, we disable npm3 for this.
  _npm3 = ((self (args // {npm3 = false;}))
          .generatePackages {nodePackagesPath = ../nodePackages;})
          .nodePackages.npm;

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
  fetchUrlWithHeaders = pkgs.callPackage ./fetchUrlWithHeaders.nix {};

  # Uses the parsed namespace tokens to create a function that can
  # fetch a private package from an npm repo.
  fetchPrivateNpm = {namespace, headers ? {}, ...}@args:
    if !(hasAttr namespace namespaceTokens)
    then throw "NPM_AUTH_TOKENS does not contain namespace ${namespace}"
    else let
      Authorization = "Bearer ${namespaceTokens.${namespace}}";
      headers = {inherit Authorization;} // headers;
    in
    fetchUrlWithHeaders (removeAttrs args ["namespace"] // {inherit headers;});
in

rec {
  inherit nodejs;

  buildNodePackage = import ./buildNodePackage.nix ({
    inherit pkgs nodejs buildNodePackage;
  } // (if npm3 then {npm = _npm3;} else {}));
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
        passthru.overrideNodePackage = (_: deriv);
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
  generatePackages = {
    # Path to find node packages in.
    nodePackagesPath,
    # Extensions are other node libraries which will be folded into the
    # generated one.
    extensions ? [],
    # If any additional arguments should be made available to callPackage
    # (for example for packages which require additional arguments), they
    # can be passed in here. Those packages can declare an `extras` argument
    # which will contain whatever is passed in here.
    extras ? {}
  }:
    let
      callPackageWith = pkgSet: path: overridingArgs: let
        inherit (builtins) intersectAttrs functionArgs;
        inherit (pkgs.lib) filterAttrs;
        # The path must be a function; import it here.
        func = import path;
        # Get the arguments to the function; e.g. "{a=false; b=true;}", where
        # a false value is an argument that has no default.
        funcArgs = functionArgs func;
        # Take only the arguments that don't have a default.
        noDefaults = filterAttrs (_: v: v == false) funcArgs;
        # Intersect this set with the package set to create the arguments to
        # the function.
        satisfyingArgs = intersectAttrs noDefaults pkgSet;
        # Override these arguments with whatever's passed in.
        actualArgs = satisfyingArgs // overridingArgs;
        # Call the function with these args to get a derivation.
        deriv = func actualArgs;
        in deriv;

      nodePackages = joinSets (map (e: e.nodePackages) extensions) //
                     (import nodePackagesPath {inherit callPackage;});

      callPackage = callPackageWith {
        inherit fetchUrlWithHeaders namespaceTokens;
        inherit pkgs nodePackages buildNodePackage brokenPackage;
        inherit extras;
        inherit (nodePackages) namespaces;
      };
    in
    {
      inherit callPackage namespaceTokens pkgs;
      nodePackages = nodePackages // {inherit nodejs;};
      nodeLib = self args;
      npm3 = _npm3;
    };
}