{
  # System packages.
  pkgs,
  # Derivation for nodejs and npm.
  nodejs,
  # Which version of npm to use.
  npm ? nodejs,
  # Self-reference for overriding purposes.
  buildNodePackage,
}:

let
  inherit (pkgs) stdenv python;
  inherit (pkgs.lib) showVal optional foldl;
  inherit (stdenv.lib) fold removePrefix hasPrefix subtractLists flip
                       intersectLists isAttrs listToAttrs nameValuePair hasAttr
                       mapAttrs filterAttrs attrNames elem concatMapStrings
                       attrValues concatStringsSep optionalString;

  # Map a function and concatenate with newlines.
  concatMapLines = list: func: concatStringsSep "\n" (map func list);

  # This expression builds the raw C headers and source files for the base
  # node.js installation. Node packages which use the C API for node need to
  # link against these files and use the headers.
  nodejsSources = pkgs.runCommand "node-sources" {} ''
    tar --no-same-owner --no-same-permissions -xf ${nodejs.src}
    mv $(find . -type d -mindepth 1 -maxdepth 1) $out
  '';

  # Checks a derivation's structure; if it doesn't have certain attributes then
  # it isn't a node package and we error. Otherwise return the package.
  verifyNodePackage = pkg:
    if !builtins.hasAttr "namespace" pkg
    then throw ''
      Package dependency does not appear to be a node package: ${showVal pkg}.
      Use "buildInputs" or "propagatedBuildInputs" for non-node dependencies,
      instead of "deps".
    ''
    else pkg;

  # By default, when checking we'll run npm test.
  defaultCheckPhase = ''
    runHook preCheck
    npm test
    runHook postCheck
  '';

  # Convert a list of derivations into an attribute set. Ensure
  # everything in the set is a node package. Keys of the set will be
  # package names, and values will be the packages.
  toAttrSet = obj:
    if isAttrs obj then obj
    else if obj == null then {}
    else listToAttrs (map (x: nameValuePair x.name x)
                     (map verifyNodePackage obj));

  # Npm supports circular dependencies, while nix does not. A package
  # P has a circular dependency D if and only if P appears in the
  # dependency closure of D. Imagine the following situation ("->"
  # means "depends on"):
  #
  #  A -> {B}, B -> {A, C}, C -> {D, E, F}
  #
  # In this case we have circularity between A and B. When building A,
  # we can't build B the normal way, because we'd have to first build
  # A, and we'd get infinite recursion. However, we can put the
  # package B, and all of its dependencies, in the `node_modules`
  # folder for A, and then A will be able to use B and vice versa when
  # A is a dependency of some library. This means to build A we need to:
  #
  # 1. Build C, D, E, and F the normal way.
  # 2. Symlink C, D, E and F into A's node_modules folder.
  # 3. Extract the source of B into the node_modules folder.
  # 4. Make a self-referential symlink of A into its own node_modules folder.
  #
  # The last step is a bit strange but since B needs to be able to import
  # A, A needs to be in the same node_modules folder as B. We can
  # accomplish this with a symlink.
  #
  # Now, how about building B? A is a circular dependency of B, but A
  # doesn't have any other dependencies. This means that the process
  # is simply
  #
  # 1. Extract the source of A into the node_modules folder.
  # 2. Symlink B into the node_modules folder.
  #
  # Now let's imagine a slightly more complicated situation involving
  # a three-way circularity.
  #
  # A -> {B, C}, B -> {C, D, E}, C -> {A, D}
  #
  # In this case the only packages we can build normally are D and
  # E. Then to build A, we build E and D as normal and symlink them
  # into node_modules, extract B and C's source into node_modules, and
  # make a reflexive symink.
  #
  # Finally, a double-circular dependency:
  #
  # A -> {B}, B -> {C, A}, C -> {B}
  #
  # So the more general algorithm to build a package A which has one
  # or more circular dependencies is:
  #
  # 1. Compute the full set of packages that those dependencies depend
  #    on at runtime (including transitive dependencies), *except* A and
  #    the packages themselves.
  # 2. Symlink all of these into node_modules.
  # 3. Extract the source of each circular dependency into node_modules.
  # 4. Make a self-referential symlink of A.

  # So the first step is to compute the closure of a package's
  # circular dependencies.  We need to keep track of the names of
  # packages we've already encountered to avoid loops.
  cycles =
    # Packages we've already seen.
    seenPackages:
    # Name of package we're inspecting.
    package:
      if hasAttr package.name seenPackages
      # We've completed the cycle; stop here.
      then seenPackages
      else let
        # Add package to seen.
        seen = seenPackages // {"${package.name}" = package;};
        # Recur on circular dependencies.
        cycles' = map (cycles seen) (attrValues package.circularDependencies);
      in
      # Combine the results into a single set.
      foldl (a: b: a // b) {} cycles';

  # We'll put all of these together below, for packages that have
  # circular dependencies.
in

{
  # The name of the package. If it's a private package with a namespace,
  # this should not contain the namespace.
  name,

  # Used for private packages. Indicated in the name field of the
  # package.json, e.g. if the name in that file is "@mynamespace/mypackage",
  # then the namespace is 'mynamespace' and the name is 'mypackage'. Public
  # packages will not need this.
  namespace ? null,

  # Version of the package. This should follow the semver standard, although
  # we don't explicitly enforce that in this function.
  version,

  # Source of the package; can be a tarball or a folder on the filesystem.
  src,

  # The suffix to the name as it appears in `nix-env -q` and the nix store. By
  # default, the name of nodejs interpreter e.g:
  # "<package name>-<package version>-nodejs-<nodejs version>"
  nameSuffix ? "-${nodejs.name}",

  # If there's a namespace, by default it will be prepended to the package
  # name. Otherwise, a prefix can be given explicitly.
  namePrefix ? (if namespace == null then "" else "${namespace}-"),

  # List of (runtime) dependencies.
  deps ? [],

  # List of runtime dependencies which are circular, meaning that the
  # package being defined here occurs somewhere in its own dependency
  # tree.
  circularDependencies ? [],

  # List of peer dependencies. See:
  # https://nodejs.org/en/blog/npm/peer-dependencies/
  peerDependencies ? [],

  # List of optional dependencies.
  optionalDependencies ? [],

  # List of optional dependencies to skip. List of strings, where a string
  # should contain the `name` of the derivation to skip (not a version or
  # namespace).
  skipOptionalDependencies ? [],

  # List or set of development dependencies (or null). These will only be
  # installed when `includeDevDependencies` is true, which is provided by
  # the `.env` attribute.
  devDependencies ? null,

  # If true and devDependencies are defined, the package will only be
  # installed contingent on successfully running tests.
  doCheck ? false,

  # If true, devDependencies will be added to the packages to the
  # build environment. By default, this is true whenever doCheck is true.
  includeDevDependencies ? doCheck,

  # Bash command to run package tests.
  checkPhase ? defaultCheckPhase,

  # Additional flags passed to npm install. A list of strings.
  extraNpmFlags ? [],

  # Build inputs to propagate in addition to nodejs and non-dev dependencies.
  propagatedBuildInputs ? [],

  # Build inputs in addition to npm and dev dependencies.
  buildInputs ? [],

  # Whether to strip debugging symbols from binaries and patch ELF executables.
  # These should both probably be true but can be overridden.
  # Doc for details: https://nixos.org/wiki/NixPkgs_Standard_Environment.
  dontStrip ? true, dontPatchELF ? true,

  # Optional attributes to pass through to downstream derivations.
  passthru ? {},

  # A set of dependencies to patch.
  patchDependencies ? {},

  # We attempt to automatically remove dev dependencies from the node_modules
  # folder prior to copying to the nix store. If this isn't desired (for
  # example, custom behavior is needed), then set this to true.
  skipDevDependencyCleanup ? false,

  # Indicates the package is broken. Not super user-friendly but
  # better than nothing.
  isBroken ? false,

  # Metadata about the package.
  meta ? {},

  # Any remaining flags are passed through to mkDerivation.
  ...
} @ args:

let
  # The package name as it appears in the package.json. This contains a
  # namespace if there is one, so it will be a distinct identifier for
  # different packages.
  fullName = if namespace == null then name
                    else "@${namespace}/${name}";

  # The package name with a version appended. This should be unique amongst
  # all packages.
  uniqueName = "${fullName}@${version}";

in

# Dev dependencies are required to be installed to run unit tests for
# nearly all packages. Therefore we require that they be installed in
# order to enable tests.
if doCheck && (devDependencies == null)
then throw ("${uniqueName}: Can't run tests because devDependencies have " +
            "not been defined. You can pass in `devDependencies = [];` if " +
            "there are no dev dependencies.")
else if includeDevDependencies && (devDependencies == null)
then throw ("${uniqueName}: Can't include dev dependencies since they have " +
            "not been defined. You can pass in `devDependencies = [];` if " +
            "there are no dev dependencies.")
else if isBroken
then throw "${uniqueName}: listed as broken, see definition for details"
else

let
  # Types of npm dependencies as they appear as keys in a package.json file.
  dependencyTypes = ["dependencies" "devDependencies" "peerDependencies"
                     "optionalDependencies"];

  # These arguments are intended as directives to this function and not
  # to be passed through to mkDerivation. They are removed below.
  attrsToRemove = ["deps" "flags" "skipOptionalDependencies" "isBroken"
                   "passthru" "doCheck" "includeDevDependencies" "version"
                   "namespace" "patchDependencies" "skipDevDependencyCleanup"
                   "circularDependencies"] ++ dependencyTypes;

  # We create a `self` object for self-referential expressions. It
  # bottoms out in a call to `mkDerivation` at the end.
  self = let
    # Set of normal dependencies.
    _dependencies = toAttrSet deps;
    # Set of circular dependencies.
    _circularDependencies = toAttrSet circularDependencies;
    # Set of optional dependencies.
    _optionalDependencies = toAttrSet optionalDependencies;
    # Set of peer dependencies.
    _peerDependencies = toAttrSet peerDependencies;

    # Dev dependencies will only be included if requested.
    _devDependencies = if !includeDevDependencies then {}
                       else toAttrSet devDependencies;

    # Dependencies we need to propagate, meaning they need to be
    # available to the package at runtime. We don't include the
    # circular dependencies here, even though they might be needed at
    # runtime, because we have a "special way" of building them.
    runtimeDependencies = _dependencies //
                             _optionalDependencies //
                             _peerDependencies;

    # Names of packages to keep when cleaning up dev dependencies. We
    # put them in a dictionary for fast lookup, but the values are
    # just null.
    packagesToRetain = mapAttrs (_: _: null) (
      runtimeDependencies // _circularDependencies);

    # Required dependencies are those that we haven't filtered yet.
    requiredDependencies = _devDependencies // runtimeDependencies;

    # Flags that we will pass to `npm install`.
    npmFlags = concatStringsSep " " ([
      # We point the registry at something that doesn't exist. This will
      # mean that NPM will fail if any of the dependencies aren't met, as it
      # will attempt to hit this registry for the missing dependency.
      "--registry=http://notaregistry.$UNIQNAME.com"
      # These flags make failure fast, as otherwise NPM will spin for a while.
      "--fetch-retry-mintimeout=0" "--fetch-retry-maxtimeout=10" "--fetch-retries=0"
      # This will disable any user-level npm configuration.
      "--userconfig=/dev/null"
      # This flag is used for packages which link against the node headers.
      "--nodedir=${nodejsSources}"
      # This will tell npm not to run pre/post publish hooks
      # "--ignore-scripts"
      ] ++
      # Use the --production flag if we're not running tests; this will make
      # npm skip the dev dependencies.
      (if !doCheck then ["--production"] else []) ++
      # Add any extra headers that the user has passed in.
      extraNpmFlags);

    patchPhase = ''
      runHook prePatch
      patchShebangs $PWD >/dev/null

      # Ensure that the package name matches what is in the package.json.
      node ${./checkPackageJson.js} checkPackageName ${fullName}

      # Remove any impure dependencies from the package.json (see script
      # for details)
      node ${./removeImpureDependencies.js}

      # We do not handle shrinkwraps yet
      rm npm-shrinkwrap.json 2>/dev/null || true

      ${if patchDependencies == {} then "" else ''
        cat <<EOF | python
        import json
        with open("package.json") as f:
            package_json = json.load(f)
        ${concatMapLines (attrNames patchDependencies) (name: let
            version = patchDependencies."${name}";
          in
          # Iterate through all of the dependencies we're patching, and for
          # each one either remove it or set it to something else.
          concatMapLines dependencyTypes (depType: ''
            if "${name}" in package_json.setdefault("${depType}", {}):
                ${if version == null then ''
                    print("removing ${name} from ${depType}")
                    package_json["${depType}"].pop("${name}", None)
                  '' else ''
                    print("Patching ${depType} ${name} to version ${version}")
                    package_json["dependencies"]["${name}"] = "${version}"
                  ''}
            ''))}
        with open("package.json", "w") as f:
            f.write(json.dumps(package_json))
        EOF
      ''}

      runHook postPatch
    '';

    # Compute any cycles. Remove 'self' from the dependency closure.
    circularDepClosure = removeAttrs (cycles {} self) [self.name];

    # Turn the closure into a list of all circular dependencies.
    circulars = attrValues circularDepClosure;

    # All of the transitive dependencies (non-circular) of the
    # circular packages.
    transCircularDeps =
      foldl (a: b: a // b) {} (map (p: p.runtimeDependencies) circulars);

    configurePhase =
    let
      # Symlink dependencies for node modules.
      link = dep: ''
        if ! [[ -e node_modules/${dep.fullName} ]]; then
          ln -sv ${dep.fullPath} ${dep.modulePath}
          if [[ -d ${dep}/bin ]]; then
            find -L ${dep}/bin -maxdepth 1 -type f -executable \
              | while read exec_file; do
                echo "Symlinking $exec_file binary to node_modules/.bin"
                mkdir -p node_modules/.bin
                ln -s $exec_file node_modules/.bin/$(basename $exec_file)
            done
          fi
        fi
      '';
    in concatStringsSep "\n" (
      ["runHook preConfigure"] ++
      (flip map (attrValues requiredDependencies) (dep:
        # Create symlinks (or copies) of all of the required dependencies.
        ''
          mkdir -p ${dep.modulePath}
          ${link dep}
          ${concatMapStrings link (attrValues dep.peerDependencies)}
        '')) ++
      ["runHook postConfigure"] ++
      (optional (circulars != []) (let
       in concatStringsSep "\n" [
        # Extract all of the circular dependencies' tarballs.
        (concatMapLines circulars (dep: ''
          echo Satisfying ${dep.fullName}, circular dependency \
            of ${self.fullName}
          mkdir -p node_modules
          if [[ ! -d node_modules/${dep.fullName} ]]; then
            tar xf ${dep.src}
            if [[ ! -d package ]]; then
              echo "Expected ${dep.src} to be a tarball containing a" \
                   "'package' directory. Don't know how to handle this :("
              exit 1
            fi
            mv package node_modules/${dep.fullName}
          fi
        ''))
        # Symlink all of the transitive dependencies of the circular packages.
        (concatMapLines (attrValues transCircularDeps) link)
        # Create a temporary symlink to the current package directory,
        # so that node knows that the dependency is satisfied when
        # checking the recursive dependencies (grumble grumble).
        "ln -s $PWD node_modules/${self.fullName}"
      ]
    )));

    buildPhase = concatStringsSep "\n" [
      "runHook preBuild"
      ''
      (
        # NPM reads the `HOME` environment variable and fails if it doesn't
        # exist, so set it here.
        export HOME=$PWD
        echo npm install ${npmFlags}

        # Try doing the install first. If it fails, first check the
        # dependencies, and if we don't uncover anything there just rerun it
        # with verbose output.
        npm install ${npmFlags} >/dev/null 2>&1 || {
          echo "Installation of ${name}@${version} failed!"
          echo "Checking dependencies to see if any aren't satisfied..."
          node ${./checkPackageJson.js} checkDependencies
          echo "Dependencies seem ok. Rerunning with verbose logging:"
          npm install . ${npmFlags} --loglevel=verbose
          if [[ -d node_modules ]]; then
            echo "node_modules contains these files:"
            ls -l node_modules
          fi
          exit 1
        }
      )
      ''
      # If we have any circular dependencies, they will need to reference
      # the current package at runtime. Make a symlink into the node modules
      # folder which points at where the package will live in $out.
      (optionalString (circulars != []) ''
        rm node_modules/${self.fullName}
        ln -s $out/lib/node_modules/${self.fullName} \
          node_modules/${self.fullName}
      '')
      "runHook postBuild"
    ];

    installPhase = ''
      runHook preInstall

      # Ensure that the main entry point appears post-build.
      node ${./checkPackageJson.js} checkMainEntryPoint

      # Install the package that we just built.
      mkdir -p $out/lib/${self.modulePath}

      # Remove all of the dev dependencies which do not appear in other
      # dependency sets.
      ${if skipDevDependencyCleanup then "" else
        flip concatMapStrings (attrValues _devDependencies) (dep:
         let
           rm = dep:
             if !hasAttr dep.name packagesToRetain
             then ''
               # Remove the dependency from node modules
               rm -rfv node_modules/${dep.fullName}
               # Remove any binaries it generated from node_modules/.bin
               if [[ -d ${dep}/bin ]]; then
                 find -L ${dep}/bin -maxdepth 1 -type f -executable \
                   | while read exec_file; do
                     rm -fv node_modules/.bin/$(basename $exec_file)
                 done
               fi

               # Remove any peer dependencies that package might have brought
               # with it.
               ${concatMapStrings rm (attrValues dep.peerDependencies)}
             ''
             else ''
               echo "Retaining ${dep.basicName} since it " \
                    "appears in the set of dependencies to propagate"
             '';
         in
         rm dep)}

      # Copy the folder that was created for this path to $out/lib.
      cp -r $PWD $out/lib/node_modules/${self.fullName}

      # Remove the node_modules subfolder from there, and instead put things
      # in $PWD/node_modules into that folder.
      if [ -e "$out/lib/node_modules/${self.fullName}/man" ]; then
        echo -n "Linking manpages... "
        NUM_MAN_PAGES=0
        mkdir -p $out/share
        for dir in $out/lib/node_modules/${self.fullName}/man/*; do
          mkdir -p $out/share/man/$(basename "$dir")
          for page in $dir/*; do
            ln -s $page $out/share/man/$(basename "$dir")
            NUM_MAN_PAGES=$(($NUM_MAN_PAGES + 1))
          done
        done
        echo "linked $NUM_MAN_PAGES man pages."
      fi

      # Move peer dependencies to node_modules
      ${concatMapStrings (dep: ''
        mkdir -p ${dep.modulePath}
        mv node_modules/${dep.fullName} $out/lib/${dep.modulePath}
      '') (attrValues _peerDependencies)}

      # Install binaries using the `bin` object in the package.json
      python ${./installBinaries.py}

      runHook postInstall
    '';

    # These are the arguments that we will pass to `stdenv.mkDerivation`.
    mkDerivationArgs = {
      inherit
        buildPhase
        checkPhase
        configurePhase
        doCheck
        dontPatchELF
        dontStrip
        fullName
        installPhase
        meta
        npmFlags
        patchPhase
        src;

      # Informs lower scripts not to check dev dependencies
      NO_DEV_DEPENDENCIES = devDependencies == null;

      # Tell mkDerivation to run `setVariables` prior to other phases.
      prePhases = ["setVariables"];

      # Define some environment variables that we will use in the build.
      setVariables = ''
        # This creates a string for this package which is unique but
        # deterministic. We can use it to create temporary directories
        # and URLs and be confident there will be no collisions.
        export HASHEDNAME=$(echo "$propagatedNativeBuildInputs $name" \
                          | md5sum | awk '{print $1}')

        # This appends the package name and version to the hash string
        # we defined above, so that it is more human-readable.
        export UNIQNAME="''${HASHEDNAME:0:10}-${name}-${version}"

        # This is used by the checkPackageJson script so that it can
        # confirm that version ranges are satisfied by installed
        # versions.
        export SEMVER_PATH=${npm}/lib/node_modules/npm/node_modules/semver
      '';

      shellHook = ''
        runHook preShellHook
        runHook setVariables
        export PATH=${npm}/bin:${nodejs}/bin:$(pwd)/node_modules/.bin:$PATH
        rm -rf $TMPDIR/$UNIQNAME
        mkdir -p $TMPDIR/$UNIQNAME
        (
          cd $TMPDIR/$UNIQNAME
          eval "$configurePhase"
        )
        echo "Installed $fullName dependencies in temporary directory" \
             "$TMPDIR/$UNIQNAME"
        export PATH=$TMPDIR/$UNIQNAME/node_modules/.bin:$PATH
        NODE_MODULES=$TMPDIR/$UNIQNAME/node_modules
        export NODE_PATH=$NODE_MODULES:$NODE_PATH
        # Check if the current directory contains the package.json for
        # this package.
        if python -c "import json; assert json.load(open('package.json'))['name'] == '$fullName'" 2>/dev/null; then
          # If we're in the package directory, symlink it into the
          # temporary node modules folder we're building and then
          # attempt to import it. Issue a warning if we're not
          # successful.
          echo "Symlinking current directory into node modules folder..."
          mkdir -p $(dirname $NODE_MODULES/$fullName)
          ln -s $(pwd) $NODE_MODULES/$fullName
          if echo "require('$fullName')" | node; then
            echo "Successfully set up $fullName in local environment."
          else
            echo "WARNING: could not set up $fullName in local environment."
          fi
        else
          echo "WARNING: you are not in the directory for package $fullName," \
               "so the shell hook can't symlink the local source code into" \
               "the temporary node_modules directory. This might, for" \
               "example, prevent you from being able to" \
               "\`require('$fullName')\` in a node REPL. You might need to" \
               "do something manually to set this up; for example if this" \
               "package's source is a tarball, the command" '`tar -xf $src;' \
               'ln -s $PWD/package $NODE_MODULES/$fullName` might work.'
        fi
        runHook postShellHook
      '';

      # Propagate pieces of information about the package so that downstream
      # packages can reflect on them.
      passthru = (passthru // {
        inherit uniqueName fullName namespace version runtimeDependencies;
        circularDependencies = _circularDependencies;
        # The basic name is the name without namespace or version, in contrast
        # to the fullName which might have a namespace attached, or the
        # uniqueName which has a version attached.
        basicName = name;

        # The path within $out/lib to find the package. If the package does not
        # have a namespace, it will simply be in `node_modules`, and otherwise
        # it will appear in `node_modules/@namespace`.
        modulePath = if namespace == null then "node_modules"
                     else "node_modules/@${namespace}";

        # The full path to the package's code (i.e. folder containing
        # package.json) within the nix store.
        fullPath = "${self}/lib/node_modules/${self.fullName}";

        # Downstream packages need to have access to peer dependencies.
        peerDependencies = _peerDependencies;

        # The `env` attribute is meant to be used with `nix-shell` (although
        # that's not required). It will build the package with its dev
        # dependencies. This means that the package must have dev dependencies
        # defined, or it will error.
        env = buildNodePackage (args // {includeDevDependencies = true;});

        # An 'overrideNodePackage' attribute, which will call
        # `buildNodePackage` with the given arguments overridden.
        # We don't use the name `override` because this will get stomped on
        # if the derivation is the result of a `callPackage` application.
        overrideNodePackage = newArgs: buildNodePackage (args // newArgs);
      });
    } // (removeAttrs args attrsToRemove) // {
      name = if namePrefix == null then throw "Name prefix is null"
             else if name == null then throw "Name is null"
             else if version == null then throw "Version of ${name} is null"
             else if nameSuffix == null then throw "Name suffix is null"
             else "${namePrefix}${name}-${version}${nameSuffix}";

      # Propagate the runtime dependencies, any non-nodejs dependencies,
      # and nodejs itself.
      propagatedBuildInputs = propagatedBuildInputs ++
                              attrValues runtimeDependencies ++
                              [nodejs];


      # Give as buildInputs npm, python, dev dependencies (if any) and
      # additional specified build inputs. In addition, on darwin we
      # provide XCode, since node-gyp will use it, and on linux we add
      # utillinux.
      buildInputs = [npm python] ++
                    attrValues _devDependencies ++
                    buildInputs ++
                    (optional stdenv.isLinux pkgs.utillinux) ++
                    (optional stdenv.isDarwin pkgs.xcodeenv.xcodewrapper);
    };

    in stdenv.mkDerivation mkDerivationArgs;

in self
