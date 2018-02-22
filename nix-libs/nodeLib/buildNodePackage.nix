{
  # System packages.
  pkgs,
  # Derivation for nodejs and npm.
  nodejs,
  # Which version of npm to use.
  npm ? nodejs,
  # Self-reference for overriding purposes.
  buildNodePackage,
  # Provides xcode binaries to OSX builds (for native packages).
  xcode-wrapper,
  # Scripts that we use during the npm builds.
  node-build-tools,
  # C header files for node libraries
  nodejsSources,
}:

let
  inherit (pkgs) stdenv python2 file;
  inherit (pkgs.lib) showVal optional foldl;
  inherit (stdenv.lib) fold removePrefix hasPrefix subtractLists flip
                       intersectLists isAttrs listToAttrs nameValuePair hasAttr
                       mapAttrs filterAttrs attrNames elem concatMapStrings
                       attrValues concatStringsSep optionalString filter
                       optionalAttrs;

  # Join a list of strings with newlines, filtering out empty lines.
  joinLines = strings: concatStringsSep "\n" (filter (s: s != "") strings);

  # Map a function and concatenate with newlines.
  concatMapLines = list: func: joinLines (map func list);

  # Create a tar wrapper that filters all the 'Ignoring unknown
  # extended header keyword' noise
  #
  # Cribbed from nixpkgs/pkgs/development/node-packages/node-env.nix
  tarWrapper = pkgs.runCommand "tarWrapper" {} ''
    mkdir -p $out/bin
    cat > $out/bin/tar <<EOF
    #! ${pkgs.stdenv.shell} -e
    $(type -p tar) "\$@" --warning=no-unknown-keyword
    EOF
    chmod +x $out/bin/tar
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
  namePrefix ? (if namespace == null then "" else "=${namespace}=-"),

  # List of (runtime) dependencies.
  deps ? [],

  # List of runtime dependencies which are circular, meaning that the
  # package being defined here occurs somewhere in its own dependency
  # tree.
  circularDependencies ? [],

  # List of optional dependencies.
  optionalDependencies ? [],

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

  # Build inputs to propagate in addition to nodejs and non-dev dependencies.
  propagatedBuildInputs ? [],

  # Build inputs in addition to npm and dev dependencies.
  buildInputs ? [],

  # Whether to strip debugging symbols from binaries.
  # This normally shouldn't be necessary but it can be enabled if desired.
  # Doc for details: https://nixos.org/wiki/NixPkgs_Standard_Environment.
  dontStrip ? true,

  # Optional attributes to pass through to downstream derivations.
  passthru ? {},

  # A set of dependencies to patch, changing the version given in the
  # package.json. Keys are dependency names, values are new
  # versions. Alternatively, a value can be `null`, which will have
  # the effect of removing the dependency from the package.json.
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

  # Build step
  buildStep ? "execute-install-scripts",

  # Overrides to the arguments to mkDerivation. This can be used to
  # set custom values for the arguments that buildNodePackage would
  # set, so it's only necessary for a certain set of keys (everything
  # else can just be passed in directly).
  derivationOverrides ? {},

  # Any remaining flags are passed through to mkDerivation.
  ...
} @ args:

let
  # The package name as it appears in the package.json. This contains a
  # namespace if there is one, so it will be a distinct identifier for
  # different packages.
  fullName = if namespace == null then name else "@${namespace}/${name}";

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
  dependencyTypes = ["dependencies" "devDependencies" "optionalDependencies"];

  # These arguments are intended as directives to this function and not
  # to be passed through to mkDerivation. They are removed below.
  attrsToRemove = ["deps" "flags" "isBroken"
                   "passthru" "doCheck" "includeDevDependencies" "version"
                   "namespace" "skipDevDependencyCleanup" "patchDependencies"
                   "circularDependencies" "derivationOverrides"] ++ dependencyTypes;

  # We create a `self` object for self-referential expressions. It
  # bottoms out in a call to `mkDerivation` at the end.
  self = let
    # Set of normal dependencies.
    _dependencies = toAttrSet deps;
    # Set of circular dependencies.
    _circularDependencies = toAttrSet circularDependencies;
    # Set of optional dependencies.
    _optionalDependencies = toAttrSet optionalDependencies;

    # Dev dependencies will only be included if requested.
    _devDependencies = if !includeDevDependencies then {}
                       else toAttrSet devDependencies;

    # Dependencies we need to propagate, meaning they need to be
    # available to the package at runtime. We don't include the
    # circular dependencies here, even though they might be needed at
    # runtime, because we have a "special way" of building them.
    runtimeDependencies = _dependencies // _optionalDependencies;

    # Names of packages to keep when cleaning up dev dependencies. We
    # put them in a dictionary for fast lookup, but the values are
    # just null.
    packagesToRetain = mapAttrs (_: _: null) (
      runtimeDependencies // _circularDependencies);

    # Required dependencies are those that we haven't filtered yet.
    requiredDependencies = _devDependencies // runtimeDependencies;

    patchPhase = joinLines [
      "runHook prePatch"
      "patchShebangs $PWD >/dev/null"
      # Ensure that the package name matches what is in the package.json.
      "check-package-json checkPackageName ${fullName}"
      # Remove any impure dependencies from the package.json (see script
      # for details). Apply patches in patchDependencies arguments.
      "patch-dependencies"
      # We do not handle shrinkwraps yet
      "rm -fv npm-shrinkwrap.json"
      (args.patchPhase or "")
      "runHook postPatch"
    ];

    # Computes the "circular closure" of a package.
    # See ./circular_dependencies.md for details.
    circularClosure =
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
          closure = map (circularClosure seen)
                        (attrValues package.circularDependencies);
        in
        # Combine the results into a single set.
        foldl (a: b: a // b) self.circularDependencies closure;

    # Compute any cycles. Remove 'self' from the dependency closure.
    circularDepClosure = removeAttrs (circularClosure {} self) [self.name];

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
      # Previous NODE_PATH should be empty, but it might have been set
      # in the custom derivation steps.
      "export NODE_PATH=$PWD/node_modules:$NODE_PATH"
      "check-package-json checkDependencies"
      buildStep
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
      check-package-json checkMainEntryPoint

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
        for dir in $out/lib/node_modules/${self.fullName}/man/*; do         #*/
          mkdir -p $out/share/man/$(basename "$dir")
          for page in $dir/*; do                                            #*/
            ln -s $page $out/share/man/$(basename "$dir")
            NUM_MAN_PAGES=$(($NUM_MAN_PAGES + 1))
          done
        done
        echo "linked $NUM_MAN_PAGES man pages."
      fi

      # Install binaries using the `bin` object in the package.json
      install-binaries

      runHook postInstall
    '';

    # These are the arguments that we will pass to `stdenv.mkDerivation`.
    mkDerivationArgs = removeAttrs args attrsToRemove // {
      inherit
        buildPhase
        checkPhase
        configurePhase
        doCheck
        dontStrip
        fullName
        installPhase
        meta
        patchPhase
        nodejsSources
        src;

      patchDependencies = builtins.toJSON patchDependencies;

      NO_DEV_DEPENDENCIES = !includeDevDependencies;

      # Tell mkDerivation to run `setVariables` prior to other phases.
      prePhases = ["setVariables"];

      # Define some environment variables that we will use in the build.
      setVariables = ''
        # In case this was set by an upstream derivation.
        unset NODE_PATH

        # This creates a string for this package which is unique but
        # deterministic. We can use it to create temporary directories
        # and URLs and be confident there will be no collisions.
        HASHEDNAME=$(echo "$propagatedNativeBuildInputs $name" \
                     | md5sum | awk '{print $1}')
        export HASHEDNAME

        # This appends the package name and version to the hash string
        # we defined above, so that it is more human-readable.
        export UNIQNAME="''${HASHEDNAME:0:10}-${name}-${version}"

        # Add gyp to the path in case it's needed
        export PATH=${nodejs}/lib/node_modules/npm/bin/node-gyp-bin:$PATH
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
        py_cmd='import json; print(json.load(open("package.json"))["name"])'
        if [[ -e package.json ]] && \
            [[ $(python -c "$py_cmd" 2>/dev/null) == "$fullName" ]]; then
          IN_PACKAGE_DIR=true
          # If we're in the package directory, symlink it into the
          # temporary node modules folder we're building and then
          # attempt to import it. Issue a warning if we're not
          # successful.
          echo "Symlinking current directory into node modules folder..."
          mkdir -pv $(dirname $NODE_MODULES/$fullName)
          ln -sv $(pwd) $NODE_MODULES/$fullName
          # Symlink the node modules folder to whatever has been built.
          # Don't do this if there is a node_modules directory because this
          # could break current directory state. However, issue a warning in
          # this case.
          if [[ -e node_modules ]] && [[ ! -L node_modules ]]; then
            echo "Warning: node_modules exists but is not a symlink." >&2
            echo "You can remove it (rm -r node_modules) and re-enter the" >&2
            echo 'shell, or run `ln -sf $NODE_MODULES node_modules`' >&2
          else
            rm -fv node_modules
            ln -sfv $NODE_MODULES node_modules
          fi
        else
          echo >&2
          echo "WARNING:" >&2
          echo "You are not in the directory for $fullName, so the shell"\
               "hook can't symlink the local source code into the temporary"\
               "node_modules directory. This will probably prevent you from"\
               "using $fullName in a node REPL or running its code." >&2
          echo "You might be able to do something manually to"\
               "set this up. For example if this package's source is a "\
               "tarball, running these commands might work:" >&2
          echo >&2
          echo '  $ tar -xf $src' >&2
          echo '  $ ln -s $PWD/package $NODE_MODULES/$fullName' >&2
          echo >&2
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

        # The `env` attribute is meant to be used with `nix-shell` (although
        # that's not required). It will build the package with its dev
        # dependencies. This means that the package must have dev dependencies
        # defined, or it will error.
        env = buildNodePackage (args // {includeDevDependencies = true;});

        # An 'overrideNodePackage' attribute, which will call
        # `buildNodePackage` with new arguments produced by the given
        # arg-override function. The function consumes the original
        # argument set.
        #
        # N.B: the legacy behavior of accepting a set is preserved but
        # the preferred usage-pattern is to supply a function that
        # discards its argument; e.g:
        #
        # overrideNodePackage (_: { ... })
        #
        # We don't use the name `override` because this will get stomped on
        # if the derivation is the result of a `callPackage` application.
        overrideNodePackage = newArgs:
          if builtins.isFunction newArgs
          then buildNodePackage (args // (newArgs args))
          else buildNodePackage (args // newArgs);

      });
    } // {
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
      buildInputs = [ tarWrapper npm python2 file node-build-tools ] ++
                    attrValues _devDependencies ++
                    buildInputs ++
                    (optional stdenv.isLinux pkgs.utillinux) ++
                    (optional stdenv.isDarwin xcode-wrapper);
    } // optionalAttrs stdenv.isLinux {
      LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    } // derivationOverrides;

    in stdenv.mkDerivation mkDerivationArgs;
in self
