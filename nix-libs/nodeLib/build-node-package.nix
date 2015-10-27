{
  # Provides the mkDerivation function.
  stdenv,
  # Lets us run a command.
  runCommand,
  # Derivation for nodejs and npm.
  nodejs,
  # List of required native build inputs.
  neededNatives,
  # Self-reference for overriding purposes.
  buildNodePackage
}:

{
  name, version ? "", src,

  # by default name of nodejs interpreter e.g. "nodejs-${name}"
  namePrefix ? nodejs.interpreterName + "-",

  # Node package name
  pkgName ?
    if version != "" then stdenv.lib.removeSuffix "-${version}" name else
    (builtins.parseDrvName name).name,

  # List or attribute set of dependencies
  deps ? {},

  # List or attribute set of peer depencies
  peerDependencies ? {},

  # List or attribute set of optional dependencies
  optionalDependencies ? {},

  # List of optional dependencies to skip
  skipOptionalDependencies ? [],

  # List or set of development dependencies (currently ignored)
  devDependencies ? null,

  # Whether package is binary or library
  bin ? false,

  # Additional flags passed to npm install
  flags ? "",

  # Command to be run before shell hook
  preShellHook ? "",

  # Command to be run after shell hook
  postShellHook ? "",

  # Same as https://docs.npmjs.com/files/package.json#os
  os ? [],

  # Same as https://docs.npmjs.com/files/package.json#cpu
  cpu ? [],

  # Attribute set of already resolved deps (internal),
  # for avoiding infinite recursion
  resolvedDeps ? {},

  ...
} @ args:

let
  inherit (stdenv.lib) fold removePrefix hasPrefix subtractLists
                       intersectLists isAttrs listToAttrs nameValuePair
                       mapAttrs filterAttrs attrNames elem concatMapStrings
                       attrValues getVersion flatten remove concatStringsSep;
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

    mapDependencies = deps: f: rec {
      # Convert deps to attribute set
      attrDeps = if isAttrs deps then deps else
        (listToAttrs (map (dep: nameValuePair dep.name dep) deps));

      # All required node modules, without already resolved dependencies
      # Also override with already resolved dependencies
      requiredDeps = mapAttrs (name: dep:
        dep.override {
          resolvedDeps = resolvedDeps // { "${name}" = self; };
        }
      ) (filterAttrs f (removeAttrs attrDeps (attrNames resolvedDeps)));

      # Recursive dependencies that we want to avoid with shim creation
      recursiveDeps = filterAttrs f (removeAttrs attrDeps (attrNames requiredDeps));
    };

    _dependencies = mapDependencies deps (name: dep:
      dep.pkgName != pkgName);
    _optionalDependencies = mapDependencies optionalDependencies (name: dep:
      (builtins.tryEval dep).success &&
      !(elem dep.pkgName skipOptionalDependencies)
    );
    _peerDependencies = mapDependencies peerDependencies (name: dep:
      dep.pkgName != pkgName);

    requiredDependencies =
      _dependencies.requiredDeps //
      _optionalDependencies.requiredDeps //
      _peerDependencies.requiredDeps;

    recursiveDependencies =
      _dependencies.recursiveDeps //
      _optionalDependencies.recursiveDeps //
      _peerDependencies.recursiveDeps;

    patchShebangs = dir: ''
      node=`type -p node`
      coffee=`type -p coffee || true`
      find -L ${dir} -type f -print0 | xargs -0 grep -Il . | \
      xargs sed --follow-symlinks -i \
          -e 's@#!/usr/bin/env node@#!'"$node"'@' \
          -e 's@#!/usr/bin/env coffee@#!'"$coffee"'@' \
          -e 's@#!/.*/node@#!'"$node"'@' \
          -e 's@#!/.*/coffee@#!'"$coffee"'@' || true
    '';

  npmFlags = concatStringsSep " " [
    # We point the registry at something that doesn't exist. This will
    # mean that NPM will fail if any of the dependencies aren't met, as it
    # will attempt to hit this registry for the missing dependency.
    "--registry=fakeprotocol://not.a.registry"
    # These flags make failure fast, as otherwise NPM will spin for a while.
    "--fetch-retry-mintimeout=0"
    "--fetch-retry-maxtimeout=10"
    # This will disable any user-level npm configuration.
    "--userconfig=/dev/null"
    # This flag is used for packages which link against the node headers.
    "--nodedir=${sources}"
    flags
  ];

  mkDerivationArgs = {
    inherit src;


    preConfigure = ''
      export UNIQNAME=$(echo $propagatedNativeBuildInputs \
                        | md5sum | awk '{print $1}')
      export BUILD_DIR=$TMPDIR/$UNIQNAME-build
    '';

    configurePhase = ''
      runHook preConfigure

      ${patchShebangs "./"}

      node ${./removeImpureDependencies.js}

      # We do not handle shrinkwraps yet
      rm npm-shrinkwrap.json 2>/dev/null || true

      mkdir $BUILD_DIR
      (
        cd $BUILD_DIR
        mkdir node_modules

        # Symlink or copy dependencies for node modules
        # copy is needed if dependency has recursive dependencies,
        # because node can't follow symlinks while resolving recursive deps.
        ${concatMapStrings (dep:
          if dep.recursiveDeps == [] then ''
            ln -sv ${dep}/lib/node_modules/${dep.pkgName} node_modules/
          '' else ''
            cp -R ${dep}/lib/node_modules/${dep.pkgName} node_modules/
          ''
        ) (attrValues requiredDependencies)}

        # Create shims for recursive dependenceies
        ${concatMapStrings (dep: ''
          mkdir -p node_modules/${dep.pkgName}
          cat > node_modules/${dep.pkgName}/package.json <<EOF
          {
              "name": "${dep.pkgName}",
              "version": "${getVersion dep}"
          }
          EOF
        '') (attrValues recursiveDependencies)}
      )

      export HOME=$BUILD_DIR
      runHook postConfigure
    '';

    buildPhase = ''
      runHook preBuild

      # If source was a file, repackage it, so npm pre/post publish hooks are not triggered,
      if [[ -f $src ]]; then
        GZIP=-1 tar -czf $BUILD_DIR/package.tgz ./
        export src=$BUILD_DIR/package.tgz
      else
        export src=$PWD
      fi

      # Install package
      (
      cd $HOME
      npm install $src ${npmFlags} || {
        cat package.json | python -m json.tool
        npm list
        exit 1
      }
      )

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      (
        cd $HOME

        # Remove shims
        ${concatMapStrings (dep: ''
          rm node_modules/${dep.pkgName}/package.json
          rmdir node_modules/${dep.pkgName}
        '') (attrValues recursiveDependencies)}

        mkdir -p $out/lib/node_modules

        # Install manual
        mv node_modules/${pkgName} $out/lib/node_modules
        rm -fR $out/lib/node_modules/${pkgName}/node_modules
        cp -r node_modules $out/lib/node_modules/${pkgName}/node_modules

        if [ -e "$out/lib/node_modules/${pkgName}/man" ]; then
          mkdir -p $out/share
          for dir in "$out/lib/node_modules/${pkgName}/man/"*; do
            mkdir -p $out/share/man/$(basename "$dir")
            for page in "$dir"/*; do # */ unconfuse nix-mode
              ln -sv $page $out/share/man/$(basename "$dir")
            done
          done
        fi

        # Move peer dependencies to node_modules
        ${concatMapStrings (dep: ''
          mv node_modules/${dep.pkgName} $out/lib/node_modules
        '') (attrValues _peerDependencies.requiredDeps)}

        # Install binaries and patch shebangs
        mv node_modules/.bin $out/lib/node_modules 2>/dev/null || true
        if [ -d "$out/lib/node_modules/.bin" ]; then
          ln -sv $out/lib/node_modules/.bin $out/bin
          ${patchShebangs "$out/lib/node_modules/.bin/*"}
        fi
      )

      runHook postInstall
    '';

    preFixup = if builtins.isAttrs src then ''
      find $out -type f -print0 | xargs -0 sed -i 's|${src}|${src.name}|g'
    '' else "";

    shellHook = ''
      ${preShellHook}
      export PATH=${nodejs}/bin:$(pwd)/node_modules/.bin:$PATH
      mkdir -p node_modules
      ${concatMapStrings (dep: ''
        ln -sfv ${dep}/lib/node_modules/${dep.pkgName} node_modules/
      '') (attrValues requiredDependencies)}
      ${postShellHook}
    '';

    # Stipping does not make a lot of sense in node packages
    dontStrip = true;

    meta = {
      inherit platforms;
      maintainers = [ stdenv.lib.maintainers.offline ];
    };

    passthru.pkgName = pkgName;
    # Add an 'override' attribute, which will call `buildNodePackage` with the
    # given arguments overridden.
    passthru.override = overridingArgs:
      buildNodePackage (args // overridingArgs);
  } // (removeAttrs args ["deps" "resolvedDeps" "optionalDependencies"
                          "devDependencies"]) // {
    name = "${namePrefix}${name}-${version}";

    # Run the node setup hook when this package is a build input
    propagatedNativeBuildInputs = (args.propagatedNativeBuildInputs or []) ++ [ nodejs ];

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
