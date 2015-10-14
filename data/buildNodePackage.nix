{ pkgs, nodejs }:

let

pkg = {
  name,
  version,
  src,
  dependencies,
  devDependencies ? null,
  doCheck ? devDependencies != null,
  meta ? {},
  requireNodeVersion ? null, patchPhase ? "",
  preInstall ? "", postInstall ? "", shellHook ? ""
}@args:

let
  dependencies' = map (p: p.withoutTests) dependencies;
  devDependencies' = if devDependencies == null then []
                     else map (p: p.withoutTests) devDependencies;
  shouldTest = (devDependencies != null) && doCheck;
in

# Version must be present.
if version == "" then throw "No version specified for ${name}"
else

if requireNodeVersion != null && nodejs.version != requireNodeVersion
then throw ("package ${name}-${version} requires nodejs ${requireNodeVersion},"
            + " but passed in version is ${nodejs.version}")
else

let
  inherit (pkgs.stdenv) mkDerivation;
  inherit (pkgs.stdenv.lib) concatStringsSep flip optional;
  dependencies = dependencies';
  devDependencies = devDependencies';

  # Extract the nodejs sources to a folder. These will be used as an
  # argument to npm.
  sources = mkDerivation {
    name = "node-${nodejs.version}-sources";
    buildCommand = ''
      tar --no-same-owner --no-same-permissions -xf ${nodejs.src}
      mv $(find . -type d -mindepth 1 -maxdepth 1) $out
    '';
  };

  hasDependencies = (dependencies ++ devDependencies) == [];

  # Define a few convenience functions used by the installer.
  defineFunctions = ''
    setupDependencies() {
      if [ -n $hasDependencies ]; then
        ${concatStringsSep "\n  "
          (flip map dependencies
           (d: "ln -sv ${d} node_modules/${d.pkgName}"))}
        ${if !shouldTest then "" else concatStringsSep "\n  "
          (flip map devDependencies
           (d: "ln -sv ${d} node_modules/${d.pkgName}"))}
        echo "Finished installing dependencies"
      else
        echo "${name}@${version} has no dependencies"
      fi
    }
    runInstall() {
      echo ${nodejs}/bin/npm install ${npmFlags} $FIXED_SOURCE
      echo "Pwd: $PWD"
      ls -a
      ${nodejs}/bin/npm install ${npmFlags} $FIXED_SOURCE
    }
    fixPackageJson() {
      ${nodejs}/bin/node ${./purifyPkgJson.js}
    }
  '';

  npmFlags = concatStringsSep " " ([
    # Disable any user-level npm shenanigans.
    "--userconfig /dev/null"
    # This will make NPM fail if it tries to fetch a dependency.
    "--registry http://www.example.com"
    "--nodedir=${sources}"
    "--production"
    "--fetch-retries 0"
  ] ++
    # Run the tests if we have defined dev dependencies
    optional shouldTest "--npat");

  result = mkDerivation {
    inherit meta src npmFlags;
    name = "nodejs-${name}-${version}";
    # We need to make this available to packages which depend on this, so that we
    # know what folder to put them in.
    passthru.pkgName = name;
    passthru.version = version;
    passthru.withoutTests = pkg (args // {doCheck = false;});

    phases = ["setupPhase"
              "unpackPhase"
              "patchPhase"
              "buildPhase"
              "installPhase"];
    buildInputs = [pkgs.python nodejs] ++
                  (if shouldTest then devDependencies else []);
    propagatedBuildInputs = dependencies;

    shellHook = defineFunctions + shellHook;

    setupPhase = defineFunctions;

    unpackPhase = ''
      # Extract the package source if it is a tar file; else copy it.
      SOURCE=$TMPDIR/source-${name}-${version}
      if [ -d $src ]; then
        if [ ! -e $src/package.json ]; then
          echo "No package.json file found in source."
          exit 1
        fi
        cp -r $src $SOURCE
        chmod -R +w $SOURCE
      elif tar -tf $src 2>/dev/null; then
        echo "Source is a tarball."
        # We will unpack the tarball here, and then set SOURCE to be the
        # first folder that contains a package.json within it.
        UNPACK=$TMPDIR/unpack-${name}-${version}
        mkdir $UNPACK
        tar -xf $src -C $UNPACK
        SOURCE=$(dirname $(find $UNPACK -name package.json | head -n 1))
      else
        echo "Source is not a directory or a tarball. WTF?"
        exit 1
      fi
    '';

    # In the patch phase we will remove impure dependencies from the
    # package.json file, patch impure shebangs, and recompress into a
    # tarball.
    patchPhase = ''
      FIXED_SOURCE=$TMPDIR/fixed-source-${name}-${version}.tar.gz
      (
        cd $SOURCE
        patchShebangs $SOURCE
        fixPackageJson
        ${patchPhase}
        tar -cf $FIXED_SOURCE .
      )
    '';

    # In the build phase, we will prepare a node_modules folder with all
    # of the dependencies present, and then run npm install from the
    # fixed source tarball.
    buildPhase = ''
      BUILD=$TMPDIR/build-${name}-${version}
      # Prepare the build directory.
      (
        set -e
        mkdir -p $BUILD
        cd $BUILD
        setupDependencies
        runInstall
      )
    '';

    installPhase = ''
      ${preInstall}
      mkdir -p $out/lib/node_modules
      mv $BUILD/node_modules/${name} $out/lib/node_modules

      # Copy generated binaries
      if [ -d $BUILD/node_modules/.bin ]; then
        mkdir $out/bin
        find -xtype f $BUILD/node_modules/.bin -exec cp -v {} $out/bin \;
      fi

      # Copy man pages if they exist
      manpath="$out/lib/node_modules/${name}/man"
      if [ -e $manpath ]; then
        mkdir -p $out/share
        for dir in $(find -maxdepth 1 -type d $manpath); do
          mkdir -p $out/share/man/$(basename "$dir")
          for page in $(find -maxdepth 1 $dir); do
            ln -sv $page $out/share/man/$(basename "$dir")
          done
        done
      fi
      ${postInstall}
    '';
  };
in

result;
in
pkg
