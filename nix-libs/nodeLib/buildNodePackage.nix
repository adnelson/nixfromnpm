{
  # System packages.
  pkgs,
  # Derivation for nodejs and npm.
  nodejs,
  # Which version of npm to use.
  npm ? nodejs,
  # List of required native build inputs.
  neededNatives,
  # Self-reference for overriding purposes.
  buildNodePackage,
}:

let
  inherit (pkgs) stdenv;
  inherit (pkgs.lib) showVal;
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
    if !(builtins.hasAttr "namespace" pkg)
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

  # List of names of circular dependencies (dependencies which
  # reflexively depend on this package).
  circularDependencies ? [],

  # List of (runtime) dependencies.
  deps ? [],

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
  # installed when `installDevDependencies` is true, which is provided by
  # the `.env` attribute.
  devDependencies ? null,

  # If true and devDependencies are defined, the package will only be
  # installed contingent on successfully running tests.
  doCheck ? false,

  # Test command.
  checkPhase ? defaultCheckPhase,

  # Additional flags passed to npm install. A list of strings.
  extraNpmFlags ? [],

  # Same as https://docs.npmjs.com/files/package.json#os
  os ? [],

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

if doCheck && (devDependencies == null)
then throw ("${uniqueName}: Can't run tests because devDependencies have " +
            "not been defined.")
else

let
  inherit (stdenv.lib) fold removePrefix hasPrefix subtractLists flip
                       intersectLists isAttrs listToAttrs nameValuePair hasAttr
                       mapAttrs filterAttrs attrNames elem concatMapStrings
                       attrValues concatStringsSep;

  dependencyTypes = ["dependencies" "devDependencies" "peerDependencies"
                     "optionalDependencies"];


  # These arguments are intended as directives to this function and not
  # to be passed through to mkDerivation. They are removed below.
  attrsToRemove = ["deps" "flags" "os" "skipOptionalDependencies"
                   "passthru" "doCheck" "installDevDependencies" "version"
                   "namespace" "patchDependencies" "skipDevDependencyCleanup"]
                   ++ dependencyTypes;

  # We create a `self` object for self-referential expressions. It
  # bottoms out in a call to `mkDerivation` at the end.
  self = let
    toAttrSet = obj: if isAttrs obj then obj else if obj == null then {} else
      (listToAttrs (map (x: nameValuePair x.name x) obj));

    _dependencies = toAttrSet (map verifyNodePackage deps);
    _optionalDependencies = toAttrSet (map verifyNodePackage optionalDependencies);
    _peerDependencies = toAttrSet (map verifyNodePackage peerDependencies);
    _devDependencies = if !doCheck then {}
                       else toAttrSet (map verifyNodePackage devDependencies);

    # Depencencies we need to propagate (all except devDependencies)
    propagatedDependencies = _dependencies // _optionalDependencies // _peerDependencies;

    # Required dependencies are those that we haven't filtered yet.
    requiredDependencies = _devDependencies // propagatedDependencies;

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
      patchShebangs $PWD

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
        ${flip concatMapStrings (attrNames patchDependencies) (name: let
            version = patchDependencies.${name};
          in
          # Iterate through all of the dependencies we're patching, and for
          # each one either remove it or set it to something else.
          flip concatMapStrings dependencyTypes (depType: ''
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

    configurePhase = ''
      runHook preConfigure
      ${
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
        in
        flip concatMapStrings (attrValues requiredDependencies) (dep:
        # Create symlinks (or copies) of all of the required dependencies.
        ''
          mkdir -p ${dep.modulePath}
          ${link dep}
          ${concatMapStrings link (attrValues dep.peerDependencies)}
        '')}

      runHook postConfigure
    '';

    buildPhase = ''
      runHook preBuild
      (
        # NPM reads the `HOME` environment variable and fails if it doesn't
        # exist, so set it here.
        export HOME=$PWD
        echo npm install $npmFlags

        # Try doing the install first. If it fails, first check the
        # dependencies, and if we don't uncover anything there just rerun it
        # with verbose output.
        npm install $npmFlags >/dev/null 2>&1 || {
          echo "Installation of ${name}@${version} failed!"
          echo "Checking dependencies to see if any aren't satisfied..."
          node ${./checkPackageJson.js} checkDependencies
          echo "Dependencies seem ok. Rerunning with verbose logging:"
          npm install . $npmFlags --loglevel=verbose
          if [[ -d node_modules ]]; then
            echo "node_modules contains these files:"
            ls -l node_modules
          fi
          exit 1
        }
      )
      runHook postBuild
    '';

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
             if !hasAttr dep.name propagatedDependencies
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
        echo "Linking manpages..."
        mkdir -p $out/share
        for dir in $out/lib/node_modules/${self.fullName}/man/*; do          #*/
          mkdir -p $out/share/man/$(basename "$dir")
          for page in $dir/*; do                                        #*/
            ln -sv $page $out/share/man/$(basename "$dir")
          done
        done
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
        src
        patchPhase
        configurePhase
        buildPhase
        checkPhase
        installPhase
        doCheck
        circularDependencies;

      # Informs lower scripts not to check dev dependencies
      NO_DEV_DEPENDENCIES = devDependencies == null;

      # Tell mkDerivation to run `setVariables` prior to other phases.
      prePhases = ["setVariables"];

      # Define some environment variables that we will use in the build.
      setVariables = ''
        export HASHEDNAME=$(echo "$propagatedNativeBuildInputs $name" \
                          | md5sum | awk '{print $1}')
        export UNIQNAME="''${HASHEDNAME:0:10}-${name}-${version}"
        export BUILD_DIR=$TMPDIR/$UNIQNAME-build
        export npmFlags="${npmFlags}"
        export SEMVER_PATH=${npm}/lib/node_modules/npm/node_modules/semver
      '';

      shellHook = ''
        runHook preShellHook
        runHook setVariables
        export PATH=${npm}/bin:${nodejs}/bin:$(pwd)/node_modules/.bin:$PATH
        mkdir -p $TMPDIR/$UNIQNAME
        (
          cd $TMPDIR/$UNIQNAME
          eval "$configurePhase"
        )
        echo "Installed dependencies in $TMPDIR/$UNIQNAME."
        export PATH=$TMPDIR/$UNIQNAME/node_modules/.bin:$PATH
        export NODE_PATH=$TMPDIR/$UNIQNAME/node_modules
        runHook postShellHook
      '';

      inherit dontStrip dontPatchELF;

      meta = {
        maintainers = [ stdenv.lib.maintainers.offline ];
      } // (args.meta or {});

      # Propagate pieces of information about the package so that downstream
      # packages can reflect on them.
      passthru = (passthru // {
        inherit uniqueName fullName namespace version requiredDependencies;
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
        env = buildNodePackage (args // {installDevDependencies = true;});

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

      # Pass the required dependencies and
      propagatedBuildInputs = propagatedBuildInputs ++
                              attrValues propagatedDependencies ++
                              [nodejs pkgs.tree];


      buildInputs = [npm] ++ buildInputs ++
                    attrValues _devDependencies ++
                    neededNatives;
    };

    in stdenv.mkDerivation mkDerivationArgs;

in self
