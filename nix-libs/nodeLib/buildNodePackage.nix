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

  # The path within $out/lib to find a package. If the package does not
  # have a namespace, it will simply be in `node_modules`, and otherwise it
  # will appear in `node_modules/@namespace`.
  modulePath = pkg:
    if !(builtins.hasAttr "namespace" pkg)
    then throw ''
      Package dependency does not appear to be a node package: ${showVal pkg}.
      Use "buildInputs" or "propagatedBuildInputs" for non-node dependencies,
      instead of "deps".
    ''
    else if pkg.namespace == null then "node_modules"
    else "node_modules/@${pkg.namespace}";

  # The path to the package within its modulePath. Just appending the name
  # of the package.
  pathInModulePath = pkg: "${modulePath pkg}/${pkg.basicName}";

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

  # If true and devDependencies are defined, the package will only be
  # installed contingent on successfully running tests.
  doCheck ? false,

  # Test command.
  checkPhase ? defaultCheckPhase,

  # Additional flags passed to npm install. A list of strings.
  extraNpmFlags ? [],

  # Same as https://docs.npmjs.com/files/package.json#os
  os ? [],

  # Attribute set of already resolved dependencies, for avoiding infinite
  # recursion. Used internally (don't use this argument explicitly).
  resolvedDeps ? {},

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

  # Any remaining flags are passed through to mkDerivation.
  ...
} @ args:

let
  # The package name as it appears in the package.json. This contains a
  # namespace if there is one, so it will be a distinct identifier for
  # different packages.
  packageJsonName = if namespace == null then name
                    else "@${namespace}/${name}";

  # The package name with a version appended. This should be unique amongst
  # all packages.
  uniqueName = "${packageJsonName}@${version}";

in

if doCheck && (devDependencies == null)
then throw ("${uniqueName}: Can't run tests because devDependencies have " +
            "not been defined.")
else

let
  inherit (stdenv.lib) fold removePrefix hasPrefix subtractLists isList flip
                       intersectLists isAttrs listToAttrs nameValuePair
                       mapAttrs filterAttrs attrNames elem concatMapStrings
                       attrValues getVersion flatten remove concatStringsSep;

  dependencyTypes = ["dependencies" "devDependencies" "peerDependencies"
                     "optionalDependencies"];


  # These arguments are intended as directives to this function and not
  # to be passed through to mkDerivation. They are removed below.
  attrsToRemove = ["deps" "resolvedDeps" "flags" "os" "skipOptionalDependencies"
                   "passthru" "doCheck" "installDevDependencies" "version"
                   "namespace" "patchDependencies"] ++ dependencyTypes;

  # We create a `self` object for self-referential expressions. It
  # bottoms out in a call to `mkDerivation` at the end.
  self = let
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


    strict = x: builtins.seq x x;

    toAttrSet = obj: if isAttrs obj then obj else
      (listToAttrs (map (x: nameValuePair x.name x) obj));

    mapDependencies = deps: filterFunc: let
        attrDeps = toAttrSet deps;
      in rec {
        # All required node modules, without already resolved dependencies
        # Also override with already resolved dependencies
        requiredDeps = strict (mapAttrs (name: dep:
          dep.overrideNodePackage {
            resolvedDeps = resolvedDeps // {"${name}" = self;};
          }
        ) (filterAttrs filterFunc
            (removeAttrs attrDeps (attrNames resolvedDeps))));

        # Recursive dependencies that we want to avoid with shim creation
        recursiveDeps = strict (filterAttrs filterFunc
                          (removeAttrs attrDeps (attrNames requiredDeps)));
      };

    # Filter out self-referential dependencies.
    _dependencies = strict (mapDependencies deps (name: dep: dep.uniqueName != uniqueName));

    # Filter out self-referential peer dependencies.
    _peerDependencies = strict (mapDependencies peerDependencies (name: dep:
      dep.uniqueName != uniqueName));

    # Filter out any optional dependencies which don't build correctly.
    _optionalDependencies = strict (mapDependencies optionalDependencies (name: dep:
      (builtins.tryEval dep).success &&
      !(elem dep.basicName skipOptionalDependencies)
    ));

    # Grab development dependencies if doCheck is true.
    _devDependencies = let
        filterFunc = name: dep: dep.uniqueName != uniqueName;
        depSet = if doCheck then devDependencies else [];
      in
      strict (mapDependencies depSet filterFunc);

    # Depencencies we need to propagate (all except devDependencies)
    propagatedDependencies = strict (
      _dependencies.requiredDeps //
      _optionalDependencies.requiredDeps //
      _peerDependencies.requiredDeps);

    # Required dependencies are those that we haven't filtered yet.
    requiredDependencies =
      strict (_devDependencies.requiredDeps // propagatedDependencies);

    # Recursive dependencies. These are turned into "shims" or fake packages,
    # which allows us to have dependency cycles, something npm allows.
    recursiveDependencies = strict (
      _devDependencies.recursiveDeps //
      _dependencies.recursiveDeps //
      _optionalDependencies.recursiveDeps //
      _peerDependencies.recursiveDeps);

    # Flags that we will pass to `npm install`.
    npmFlags = concatStringsSep " " ([
      # We point the registry at something that doesn't exist. This will
      # mean that NPM will fail if any of the dependencies aren't met, as it
      # will attempt to hit this registry for the missing dependency.
      "--registry=http://notaregistry.$UNIQNAME.derp"
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

    # A bit of bash to check that variables are set.
    checkSet = vars: concatStringsSep "\n" (flip map vars (var: ''
      [[ -z $${var} ]] && { echo "${var} is not set."; exit 1; }
    ''));

    patchPhase = ''
      runHook prePatch
      patchShebangs $PWD

      # Ensure that the package name matches what is in the package.json.
      node ${./checkPackageJson.js} ${packageJsonName}

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
          in flip concatMapStrings dependencyTypes (depType: ''
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
      # Symlink or copy dependencies for node modules
      # copy is needed if dependency has recursive dependencies,
      # because node can't follow symlinks while resolving recursive deps.
      ${
        let
          linkCmd = p: if p.recursiveDeps == [] then "ln -sv" else "cp -r";
          link = dep: ''
            if ! [[ -e ${pathInModulePath dep} ]]; then
              ${linkCmd dep} ${dep}/lib/${pathInModulePath dep} ${modulePath dep}
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
          mkdir -p ${modulePath dep}
          ${link dep}
          ${concatMapStrings link (attrValues dep.peerDependencies)}
        '')}

      # Remove recursive dependencies from package.json
      ${if recursiveDependencies == {} then "" else ''
        python <<EOF
        import json
        with open("package.json") as f:
            package_json = json.load(f)
        dep_names = [${concatStringsSep ", "
                       (map (d: "'${d.packageJsonName}'")
                         (attrValues recursiveDependencies))}]
        for name in dep_names:
            print("Removing recursive dependency on {} from package.json"
                  .format(name))
            for k in ("dependencies", "devDependencies", "peerDependencies",
                      "optionalDependencies"):
                package_json[k] = package_json.setdefault(k, {})
                package_json[k].pop(name, None)
        with open("package.json", "w") as f:
            f.write(json.dumps(package_json))
        EOF
        ''}

      runHook postConfigure
    '';

    buildPhase = ''
      runHook preBuild
      (
        # NPM reads the `HOME` environment variable and fails if it doesn't
        # exist, so set it here.
        export HOME=$PWD
        echo npm install $npmFlags

        npm install $npmFlags || {
          echo "NPM installation of ${name}@${version} failed!"
          echo "Rerunning with verbose logging:"
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

      # Install the package that we just built.
      mkdir -p $out/lib/${modulePath self}

      # Copy the folder that was created for this path to $out/lib.
      cp -r $PWD $out/lib/${pathInModulePath self}

      # Remove the node_modules subfolder from there, and instead put things
      # in $PWD/node_modules into that folder.
      if [ -e "$out/lib/${pathInModulePath self}/man" ]; then
        echo "Linking manpages..."
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
        doCheck;

      # Tell mkDerivation to run `setVariables` prior to other phases.
      prePhases = ["setVariables"];

      # Define some environment variables that we will use in the build.
      setVariables = ''
        export HASHEDNAME=$(echo "$propagatedNativeBuildInputs $name" \
                          | md5sum | awk '{print $1}')
        export UNIQNAME="''${HASHEDNAME:0:10}-${name}-${version}"
        export BUILD_DIR=$TMPDIR/$UNIQNAME-build
        export npmFlags="${npmFlags}"
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
        inherit platforms;
        maintainers = [ stdenv.lib.maintainers.offline ];
      };

      # Propagate pieces of information about the package so that downstream
      # packages can reflect on them.
      passthru = (passthru // {
        inherit uniqueName packageJsonName namespace version;
        # The basic name is the name without namespace or version.
        basicName = name;
        peerDependencies = _peerDependencies.requiredDeps;

        # Expose a list of recursive dependencies to upstream packages, so that
        # they can be shimmed out.
        recursiveDeps = let
          required = attrValues requiredDependencies;
          recursive = attrNames recursiveDependencies;
        in
          flatten (map (dep: remove name dep.recursiveDeps) required) ++
          recursive;

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
      name = "${namePrefix}${name}-${version}${nameSuffix}";

      # Pass the required dependencies and
      propagatedBuildInputs = propagatedBuildInputs ++
                              attrValues propagatedDependencies ++
                              [nodejs pkgs.tree];


      buildInputs = [npm] ++ buildInputs ++
                    attrValues (_devDependencies.requiredDeps) ++
                    neededNatives;

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
