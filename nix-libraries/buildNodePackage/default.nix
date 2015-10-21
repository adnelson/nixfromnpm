{ pkgs, nodejs }:

let

pkg = {
  name,
  version,
  src,
  deps,
  devDependencies ? null,
  doCheck ? devDependencies != null,
  meta ? {},
  requireNodeVersion ? null, patchPhase ? "",
  preInstall ? "", postInstall ? "", shellHook ? ""
}@args:

let
  dependencies' = map (p: p.withoutTests) deps;
  devDependencies' = if devDependencies == null then []
                     else map (p: p.withoutTests) devDependencies;
  shouldTest = (devDependencies != null) && doCheck;
in

# Version must be present.
if version == "" then throw "No version specified for ${name}"
else

let
  inherit (pkgs.lib) splitString head tail;
  nodejs_version = nodejs.version or (head (tail splitString "-" nodejs.name));
in

if requireNodeVersion != null && nodejs_version != requireNodeVersion
then throw ("package ${name}-${version} requires nodejs ${requireNodeVersion},"
            + " but passed in version is ${nodejs_version}")
else

let
  inherit (pkgs.stdenv) mkDerivation;
  inherit (pkgs.stdenv.lib) concatStringsSep flip optional optionals;
  dependencies = dependencies';
  devDependencies = devDependencies';

  # Extract the nodejs sources to a folder. These will be used as an
  # argument to npm.
  sources = mkDerivation {
    name = "${nodejs.name}-sources";
    buildCommand = ''
      tar --no-same-owner --no-same-permissions -xf ${nodejs.src}
      mv $(find . -type d -mindepth 1 -maxdepth 1) $out
    '';
  };

  hasDependencies = dependencies != [] ||
                    (shouldTest && devDependencies != []);

  symlinkDep = dep: let _name = dep.pkgName; in ''
    ln -sv ${dep}/lib/node_modules/${_name} node_modules/${_name}
  '';

  # Define a few convenience functions used by the installer.
  defineVariables = ''
    SOURCE=$TMPDIR/source-${name}-${version}
    UNPACK=$TMPDIR/unpack-${name}-${version}
    SOURCE_TARBALL=$TMPDIR/fixed-source-${name}-${version}.tar.gz
    BUILD=$TMPDIR/build-${name}-${version}

    setupDependencies() {
      ${if hasDependencies then ''
        mkdir -p node_modules
        ${concatStringsSep "\n  " (map symlinkDep dependencies)}
        ${if !shouldTest then "" else
          concatStringsSep "\n  " (map symlinkDep devDependencies)}
        echo "Installed dependencies"
      '' else "true"}
    }

    runInstall() {
      # NPM looks in the HOME folder so we set it here.
      HOME=$PWD npm install ${npmFlags} $SOURCE_TARBALL
    }

    fixPackageJson() {
      ${nodejs}/bin/node ${./removeImpureDependencies.js}
    }
  '';

  npmFlags = concatStringsSep " " ([
    # Disable any user-level npm configuration shenanigans.
    "--userconfig /dev/null"
    # This will make NPM fail if it tries to fetch a dependency.
    "--registry http://sdlfksjdlfksdlfkjj.sdfkjslkdfjakjsdh.com"
    # No retries!
    "--fetch-retries 0"
    #"--fetch-retry-maxtimeout=1"
    #"--fetch-retry-mintimout=0"
    "--nodedir=${sources}"
    "--production"
  ] ++
    # This flag will run the tests if enabled
    optional shouldTest "--npat");

  unpackPhase = ''
    # Extract the package source if it is a tar file; else copy it.
    if [ -d $src ]; then
      if [ ! -e $src/package.json ]; then
        echo "No package.json file found in source."
        exit 1
      fi
      cp -r $src $SOURCE
      chmod -R +w $SOURCE
    elif tar -tf $src 2>/dev/null 1>&2; then
      # We will unpack the tarball here, and then set SOURCE to be the
      # first folder that contains a package.json within it.
      [ -d $UNPACK ] || {
        mkdir -p $UNPACK
        tar -xf $src -C $UNPACK
      }
      SOURCE=$(dirname $(find $UNPACK -name package.json | head -n 1))
    else
      echo "Invalid source $src: not a directory or a tarball."
      exit 1
    fi
    echo $SOURCE
  '';

  # In the patch phase we will remove impure dependencies from the
  # package.json file, patch impure shebangs, and recompress into a
  # tarball.
  actualPatchPhase = ''
    (
      cd $SOURCE
      patchShebangs $SOURCE
      fixPackageJson
      ${patchPhase}
      tar -cf $SOURCE_TARBALL .
      echo $SOURCE_TARBALL
    )
  '';

  # In the build phase, we will prepare a node_modules folder with all
  # of the dependencies present, and then run npm install from the
  # fixed source tarball.
  buildPhase = ''
    # Prepare the build directory.
    echo BUILDING...........lalalalala
    (
      set -e
      mkdir -p $BUILD
      echo "YOOOOOOOOO '$BUILD'"
      ls -la $(dirname $BUILD)
      cd $BUILD
      setupDependencies
      runInstall
      echo "Done building!"
    )
  '';

  installPhase = ''
    ${preInstall}
    [ -d $BUILD ] && {
      echo "build folder '$BUILD' not created"
      exit 1
    }
    mkdir -p $out/lib/node_modules
    for pkg in $(find $BUILD/node_modules -type d -maxdepth 1); do
      echo "Transfering $pkg"
      mv $BUILD/node_modules/$pkg $out/lib/node_modules
    done

    # Symlink bin folder
    if [ -d $out/node_modules/.bin ]; then
      ln -sv $out/node_modules/bin $out/bin
    fi

    # Copy man pages if they exist
    manpath="$out/lib/node_modules/${name}/man"
    if [ -e $manpath ]; then
      mkdir -p $out/share
      ln -sv $out/lib/node_modules/${name}/man $out/share/man
    fi
    ${postInstall}
  '';


  result = mkDerivation {
    inherit meta src npmFlags;
    inherit unpackPhase buildPhase installPhase;
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
    buildInputs = [pkgs.python nodejs] ++ optionals shouldTest devDependencies;
    propagatedBuildInputs = dependencies;

    shellHook = ''
      ${defineVariables}
      ${unpackPhase}
      ${actualPatchPhase}
      ${shellHook}
    '';

    patchPhase = actualPatchPhase;
    setupPhase = defineVariables;

  };
in

result;
in
pkg
