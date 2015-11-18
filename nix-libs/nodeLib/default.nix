/*
A set of tools for generating node packages, such as to be imported by
default.nix files generated by nixfromnpm.
*/

{
  # Self-reference so that we can pass through to downstream libraries
  self,
  # Base set of packages, i.e. nixpkgs.
  pkgs,
  # Version of nodejs.
  nodejsVersion ? "4.1"
}:

let
  # Function to replace dots with something
  replaceDots = c: replaceChars ["."] [c];
  inherit (builtins) readDir removeAttrs;
  inherit (pkgs.lib) attrNames attrValues filterAttrs flip foldl
                     hasSuffix hasPrefix removeSuffix replaceChars
                     optional optionals stringToCharacters
                     concatStrings tail;
  inherit (pkgs.stdenv) isLinux;

  # Function to remove the first character of a string.
  dropFirstChar = str: concatStrings (tail (stringToCharacters str));

  fetchUrlWithHeaders = pkgs.callPackage ./pyFetchurl.nix {};

  fetchPrivateNpm = {sha1, namespace, name, version, bearer}:
    fetchUrlWithHeaders {
      inherit sha1;
      url = "http://registry.npmjs.org/@${namespace}/${name}/-/${name}-${version}.tgz";
      headers.Authentication = "Bearer ${bearer}";
    };
in

rec {
  nodejs = pkgs."nodejs-${replaceDots "_" nodejsVersion}" or (
    throw "The given nodejs version ${nodejsVersion} has not been defined."
  );
  buildNodePackage = import ./buildNodePackage.nix {
    inherit (pkgs) stdenv runCommand;
    inherit nodejs buildNodePackage;
    neededNatives = [pkgs.python] ++ optionals isLinux [pkgs.utillinux];
  };
  # A generic package that will fail to build. This is used to indicate
  # packages that are broken, without failing the entire generation of
  # a package expression.
  brokenPackage = {name, reason}:
    let
     deriv = pkgs.stdenv.mkDerivation {
        name = "broken-${name}";
        buildCommand = ''
          echo "Package ${name} is broken: ${reason}"
          exit 1
        '';
        passthru.withoutTests = deriv;
        passthru.pkgName = name;
        passthru.version = "BROKEN";
      };
    in
    deriv;

  # Concatenate a list of sets.
  joinSets = foldl (a: b: a // b) {};

  # List a directory after filtering the files.
  lsFilter = pred: dir: attrNames (filterAttrs pred (readDir dir));

  # Checks the name and type of a listing to grab non-dotfile dirs.
  isRegDir = name: type: type == "directory" && !(hasPrefix "." name);

  # Discover all of the node packages in a folder and turn them into a set
  # mapping `<name>_<version>` to the expression to build that package.
  discoverPackages = {callPackage, rootPath}:
    # if true then throw "huh? ${rootPath}" else
    let
    # Names of NPM packages defined in this directory. Don't take
    # files that start with '@'.
    nodeDirs = lsFilter (n: t: isRegDir n t && !(hasPrefix "@" n))
                        (/. + rootPath);
    # Generate the package expression from a package name and .nix path.
    toPackage = name: filepath: let
      versionRaw = removeSuffix ".nix" filepath; # Raw version, i.e. "1.2.4"
      # Join with package name to make the variable name.
      varName = "${replaceDots "-" name}_${replaceDots "-" versionRaw}";
      in
      # Return the singleton set which maps that name to the actual expression.
      {"${varName}" = callPackage (/. + rootPath + "/${name}/${filepath}") {};};
    in
    # For each directory, and each .nix file in it, create a package from that.
    joinSets (flip map nodeDirs (pkgName: let
      pkgDir = /. + rootPath + "/${pkgName}";
      # List of .nix files in the directory (excluding symlinks).
      versionFiles = lsFilter (name: type: type == "regular" &&
                               hasSuffix ".nix" name)
                               pkgDir;
      # Check if there is a `latest.nix` file
      hasLatest = lsFilter (n: _: n == "latest.nix") pkgDir != [];
      in
      joinSets (
        # Find all of the versions listed in the folder.
        map (toPackage pkgName) versionFiles ++
        # If the folder has a `latest.nix` file, link the bare name of
        # the package to that file.
        optional hasLatest {
          "${replaceDots "-" pkgName}" = callPackage
                           (/. + rootPath + "/${pkgName}/latest.nix") {};
        })));

  # Same as above, except that we take all of the namespaced packages;
  # these packages are in folders prefaced with `@`, and contain
  # packages in that folder. So, for example the path `@foo/bar` is
  # the path to all of the versions of the `bar` package under the
  # namespace `foo`.
  discoverNamespacePackages = {callPackage, rootPath}: let
    isNsDir = name: type: type == "directory" && hasPrefix "@" name;
    # Names of NPM packages defined in this directory.
    namespaceDirs = lsFilter isNsDir (/. + rootPath);
    in
    # For each namespace directory, each package folder in it, and
    # each .nix file in that, create a package from that and then
    # create a namespace out of that.
    joinSets (flip map namespaceDirs (nsDirName: {
      "${dropFirstChar nsDirName}" = discoverPackages {
          inherit callPackage;
          rootPath = /. + rootPath + "/${nsDirName}";
        };
      }));

  # The function that a default.nix can call into which will scan its
  # directory for all of the package files and generate a big attribute set
  # for all of them. Re-exports the `callPackage` function and all of the
  # attribute sets, as well as the nodeLib.
  generatePackages = {rootPath, extensions ? []}:
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

      callPackage = callPackageWith {
        inherit fetchPrivateNpm fetchUrlWithHeaders;
        inherit pkgs nodePackages buildNodePackage brokenPackage;
      };
      nodePackages = joinSets (map (e: e.nodePackages) extensions) //
                     discoverPackages {inherit callPackage rootPath;};
      namespaces = discoverNamespacePackages {inherit callPackage rootPath;};
    in {
      inherit nodePackages callPackage namespaces pkgs;
      nodeLib = self;
    };
}
