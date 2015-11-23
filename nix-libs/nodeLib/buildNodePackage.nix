{
  # Provides the mkDerivation function.
  stdenv,
  # Lets us run a command.
  runCommand,
  # Derivation for nodejs and npm.
  nodejs,
  # Which version of npm to use.
  npm ? nodejs,
  # List of required native build inputs.
  neededNatives,
  # Self-reference for overriding purposes.
  buildNodePackage
}:

let
  # The path within $out/lib to find a package. If the package does not
  # have a namespace, it will simply be in `node_modules`, and otherwise it
  # will appear in `node_modules/@namespace`.
  modulePath = pkg: if pkg.namespace == null then "node_modules"
                    else "node_modules/@${pkg.namespace}";

  # The path to the package within its modulePath. Just appending the name
  # of the package.
  pathInModulePath = pkg: "${modulePath pkg}/${pkg.basicName}";
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

  # The prefix to the name as it appears in `nix-env -q` and the nix store. By
  # default, the name of nodejs interpreter e.g. "nodejs-<version>-${name}".
  namePrefix ? "${nodejs.name}-" +
               (if namespace == null then "" else "${namespace}-"),

  # List or attribute set of (runtime) dependencies.
  deps ? {},

  # List or attribute set of peer dependencies. See:
  # https://nodejs.org/en/blog/npm/peer-dependencies/
  peerDependencies ? {},

  # List or attribute set of optional dependencies.
  optionalDependencies ? {},

  # List of optional dependencies to skip. List of strings, where a string
  # should contain the `name` of the derivation to skip (not a version or
  # namespace).
  skipOptionalDependencies ? [],

  # List or set of development dependencies (or null). These will only be
  # installed when `installDevDependencies` is true, which is provided by
  # the `.env` attribute.
  devDependencies ? null,

  # Install the dev dependencies. It's an error to set this to be true if
  # `devDependencies` is null.
  installDevDependencies ? false,

  # If true and devDependencies are defined, the package will only be
  # installed contingent on successfully running tests.
  doCheck ? false,

  # Additional flags passed to npm install. A list of strings.
  extraNpmFlags ? [],

  # Same as https://docs.npmjs.com/files/package.json#os
  os ? [],

  # Attribute set of already resolved dependencies, for avoiding infinite
  # recursion. Used internally (don't use this argument explicitly).
  resolvedDeps ? {},

  # Any remaining flags are passed through to mkDerivation.
  ...
} @ args:

let
  # The package name as it appears in the package.json. This contains a
  # namespace if there is one, so it will be a distinct identifier for
  # different packages.
  pkgName = if namespace == null then "${name}@${version}"
            else "@${namespace}/${name}@${version}";
in

if installDevDependencies && (devDependencies == null)
then throw ("${pkgName}: Can't install dev dependencies because " +
            "they're not defined. Please provide a devDependencies argument.")
else

let
  inherit (stdenv.lib) fold removePrefix hasPrefix subtractLists isList flip
                       intersectLists isAttrs listToAttrs nameValuePair
                       mapAttrs filterAttrs attrNames elem concatMapStrings
                       attrValues getVersion flatten remove concatStringsSep;

  # These arguments are intended as directives to this function and not
  # to be passed through to mkDerivation. They are removed below.
  attrsToRemove = ["deps" "resolvedDeps" "optionalDependencies" "flags"
                   "devDependencies" "os" "skipOptionalDependencies"
                   "installDevDependencies" "version" "namespace"];

  # Whether we should run tests.
  shouldTest = doCheck && installDevDependencies;

  # We create a `self` object for self-referential expressions. It
  # bottoms out in a call to `mkDerivation` at the end.
  self = let
    sources = runCommand "node-sources" {} ''
      tar --no-same-owner --no-same-permissions -xf ${nodejs.src}
      mv $(find . -type d -mindepth 1 -maxdepth 1) $out
    '';

    platforms = if os == [] then nodejs.meta.platforms else
      fold (entry: platforms:
        let
          filterPlatforms =
            stdenv.lib.platforms.${removePrefix "!" entry} or [];
        in
          # Ignore unknown platforms
          if filterPlatforms == [] then (if platforms == [] then nodejs.meta.platforms else platforms)
          else
            if hasPrefix "!" entry then
              subtractLists (intersectLists filterPlatforms nodejs.meta.platforms) platforms
            else
              platforms ++ (intersectLists filterPlatforms nodejs.meta.platforms)
      ) [] os;

    toAttrSet = obj: if isAttrs obj then obj else
      (listToAttrs (map (x: nameValuePair x.name x) obj));

    mapDependencies = deps: filterFunc: let
        attrDeps = toAttrSet deps;
      in rec {
        # All required node modules, without already resolved dependencies
        # Also override with already resolved dependencies
        requiredDeps = mapAttrs (name: dep:
          dep.override {resolvedDeps = resolvedDeps // { "${name}" = self; };}
        ) (filterAttrs filterFunc
            (removeAttrs attrDeps (attrNames resolvedDeps)));

        # Recursive dependencies that we want to avoid with shim creation
        recursiveDeps = filterAttrs filterFunc
                          (removeAttrs attrDeps (attrNames requiredDeps));
      };

    # Filter out self-referential dependencies.
    _dependencies = mapDependencies deps (name: dep: dep.pkgName != pkgName);

    # Filter out self-referential peer dependencies.
    _peerDependencies = mapDependencies peerDependencies (name: dep:
      dep.pkgName != pkgName);

    # Filter out any optional dependencies which don't build correctly.
    _optionalDependencies = mapDependencies optionalDependencies (name: dep:
      (builtins.tryEval dep).success &&
      !(elem dep.basicName skipOptionalDependencies)
    );

    # Grab development dependencies if we are told to.
    _devDependencies = let
        filterFunc = name: dep: dep.pkgName != pkgName;
        depSet = if installDevDependencies then devDependencies else [];
      in
      mapDependencies depSet filterFunc;

    # Required dependencies are those that we haven't filtered yet.
    requiredDependencies =
      _devDependencies.requiredDeps //
      _dependencies.requiredDeps //
      _optionalDependencies.requiredDeps //
      _peerDependencies.requiredDeps;

    # Recursive dependencies. These are turned into "shims" or fake packages,
    # which allows us to have dependency cycles, something npm allows.
    recursiveDependencies =
      _devDependencies.recursiveDeps //
      _dependencies.recursiveDeps //
      _optionalDependencies.recursiveDeps //
      _peerDependencies.recursiveDeps;

    # Flags that we will pass to `npm install`.
    npmFlags = concatStringsSep " " ([
      # We point the registry at something that doesn't exist. This will
      # mean that NPM will fail if any of the dependencies aren't met, as it
      # will attempt to hit this registry for the missing dependency.
      "--registry=fakeprotocol://notaregistry.$UNIQNAME.derp"
      # These flags make failure fast, as otherwise NPM will spin for a while.
      "--fetch-retry-mintimeout=0"
      "--fetch-retry-maxtimeout=10"
      # This will disable any user-level npm configuration.
      "--userconfig=/dev/null"
      # This flag is used for packages which link against the node headers.
      "--nodedir=${sources}"] ++
      # This flag will tell npm to run `npm test`.
      (if shouldTest then ["--npat"] else []) ++
      # Add any extra headers that the user has passed in.
      extraNpmFlags);

    # A bit of bash to check that variables are set.
    checkSet = vars: concatStringsSep "\n" (flip map vars (var: ''
      [[ -z $${var} ]] && { echo "${var} is not set."; exit 1; }
    ''));

    # These are the arguments that we will pass to `stdenv.mkDerivation`.
    mkDerivationArgs = {
      inherit src;

      # Tell mkDerivation to run `setVariables` prior to other phases.
      prePhases = ["setVariables"];

      # Define some environment variables that we will use in the build.
      setVariables = ''
        export HASHEDNAME=$(echo "$propagatedNativeBuildInputs $name" \
                          | md5sum | awk '{print $1}')
        export UNIQNAME="''${HASHEDNAME:0:10}-${name}-${version}"
        export BUILD_DIR=$TMPDIR/$UNIQNAME-build
      '';

      # Patch the source before building the package.
      patchPhase = ''
        runHook prePatch
        patchShebangs $PWD

        # Remove any impure dependencies from the package.json (see script
        # for details)
        node ${./removeImpureDependencies.js}

        # We do not handle shrinkwraps yet
        rm npm-shrinkwrap.json 2>/dev/null || true

        # Perform postPatch steps prior to building the tarball.
        runHook postPatch

        # Repackage source into a tarball, so npm pre/post publish hooks are
        # not triggered,
        mkdir -p $BUILD_DIR
        GZIP=-1 tar -czf $BUILD_DIR/package.tgz ./
        export PATCHED_SRC=$BUILD_DIR/package.tgz
      '';

      # Set up the environment for building by creating links to or copies of
      # all of the dependencies.
      configurePhase = ''
        runHook preConfigure
        (
          ${checkSet ["BUILD_DIR"]}
          mkdir -p $BUILD_DIR
          cd $BUILD_DIR
          # Symlink or copy dependencies for node modules
          # copy is needed if dependency has recursive dependencies,
          # because node can't follow symlinks while resolving recursive deps.
          ${
            let
              link = dep: ''
                ${if dep.recursiveDeps == [] then "ln -sfv" else "cp -rf"} \
                  ${dep}/lib/${pathInModulePath dep} ${modulePath dep}
              '';
            in
            flip concatMapStrings (attrValues requiredDependencies) (dep: ''
              mkdir -p ${modulePath dep}
              ${link dep}
              ${concatMapStrings link (attrValues dep.peerDependencies)}
            '')}

          # Create shims for recursive dependenceies
          ${concatMapStrings (dep: ''
            echo "Creating shim for recursive dependency ${dep.pkgName}"
            mkdir -pv ${pathInModulePath dep}
            cat > ${pathInModulePath dep}/package.json <<EOF
            {
                "name": "${dep.pkgName}",
                "version": "${dep.version}"
            }
            EOF
          '') (attrValues recursiveDependencies)}

          # Create dummy package.json file
          cat <<EOF > package.json
        {"name":"dummy-for-$UNIQNAME","version":"0.0.0", "license":"MIT",
         "description":"Dummy package file for building $name",
         "repository":{"type":"git","url":"http://$UNIQNAME.com"}}
        EOF

          # Create dummy readme
          echo "Dummy package" > README.md
        )

        export HOME=$BUILD_DIR
        runHook postConfigure
      '';

      # Run npm to install. Cross your fingers here, because if something's
      # going to fail, this is probably where.
      buildPhase = ''
        runHook preBuild

        # Install package
        (
          ${checkSet ["BUILD_DIR" "PATCHED_SRC"]}

          echo "Building $name in $BUILD_DIR"
          cd $BUILD_DIR
          # NPM reads the `HOME` environment variable and fails if it doesn't
          # exist, so set it here.
          HOME=$PWD npm install $PATCHED_SRC ${npmFlags} || {
            npm list
            exit 1
          }
        )

        runHook postBuild
      '';

      # After building, we will have constructed a node_modules folder in the
      # BUILD_DIR. We can take what was built and drop it into $out.
      installPhase = ''
        runHook preInstall

        (
          cd $BUILD_DIR

          # Remove shims
          ${concatMapStrings (dep: ''
            echo "Removing shim for recursive dependency ${dep.pkgName}"
            rm -rvf ${pathInModulePath dep}/package.json
          '') (attrValues recursiveDependencies)}

          # Install the package that we just built.
          mkdir -p $out/lib/${modulePath self}

          # Move the folder that was created for this path to $out/lib.
          mv ${pathInModulePath self} $out/lib/${pathInModulePath self}

          # Remove the node_modules subfolder from there, and instead put things
          # in $PWD/node_modules into that folder.
          rm -rf $out/lib/${pathInModulePath self}/node_modules
          cp -r node_modules $out/lib/${pathInModulePath self}/node_modules

          if [ -e "$out/lib/${pathInModulePath self}/man" ]; then
            mkdir -p $out/share
            for dir in $out/lib/${pathInModulePath self}/man/*; do          #*/
              mkdir -p $out/share/man/$(basename "$dir")
              for page in $dir/*; do                                        #*/
                ln -sv $page $out/share/man/$(basename "$dir")
              done
            done
          fi

          # Move peer dependencies to node_modules
          ${concatMapStrings (dep: ''
            mkdir -p ${modulePath dep}
            mv ${pathInModulePath dep} $out/lib/${modulePath dep}
          '') (attrValues _peerDependencies.requiredDeps)}

          # Install binaries and patch shebangs. These are always found in
          # node_modules/.bin, regardless of a package namespace.
          mv node_modules/.bin $out/lib/node_modules 2>/dev/null || true
          if [ -d "$out/lib/node_modules/.bin" ]; then
            ln -sv $out/lib/node_modules/.bin $out/bin
            patchShebangs $out/lib/node_modules/.bin
          fi
        )

        runHook postInstall
      '';

      shellHook = ''
        runHook preShellHook
        export PATH=${npm}/bin:${nodejs}/bin:$(pwd)/node_modules/.bin:$PATH
        mkdir -p node_modules
        ${concatMapStrings (dep: ''
          mkdir -p ${modulePath dep}
          ln -sfv ${dep}/lib/${pathInModulePath dep} ${pathInModulePath dep}
        '') (attrValues requiredDependencies)}
        runHook postShellHook
      '';

      # No need to strip debug information from executables (search in this
      # doc for details: https://nixos.org/wiki/NixPkgs_Standard_Environment).
      dontStrip = true;

      meta = {
        inherit platforms;
        maintainers = [ stdenv.lib.maintainers.offline ];
      };

      # Propagate pieces of information about the package so that downstream
      # packages can reflect on them.
      passthru.pkgName = pkgName;
      # The basic name is the name without namespace or version.
      passthru.basicName = name;
      passthru.namespace = namespace;
      passthru.version = version;
      passthru.peerDependencies = _peerDependencies.requiredDeps;
      passthru.recursiveDeps = let
          required = attrValues requiredDependencies;
          recursive = attrNames recursiveDependencies;
        in
        flatten (map (dep: remove name dep.recursiveDeps) required) ++
        recursive;

      # The `env` attribute is meant to be used with `nix-shell` (although
      # that's not required). It will build the package with its dev
      # dependencies. This means that the package must have dev dependencies
      # defined, or it will error.
      passthru.env =
        buildNodePackage (args // {installDevDependencies = true;});

      # An 'override' attribute, which will call `buildNodePackage` with the
      # given arguments overridden.
      passthru.override = newArgs: buildNodePackage (args // newArgs);
    } // (removeAttrs args attrsToRemove) // {
      name = "${namePrefix}${name}-${version}";

      # Run the node setup hook when this package is a build input
      propagatedNativeBuildInputs = (args.propagatedNativeBuildInputs or []) ++
                                    [ npm nodejs ];

      nativeBuildInputs =
        (args.nativeBuildInputs or []) ++ neededNatives ++
        (attrValues requiredDependencies);

      # Expose list of recursive dependencies upstream, up to the package that
      # caused recursive dependency
      recursiveDeps =
        (flatten (
          map (dep: remove name dep.recursiveDeps) (attrValues requiredDependencies)
        )) ++
        (attrNames recursiveDependencies);
    };

    in stdenv.mkDerivation mkDerivationArgs;

in self
