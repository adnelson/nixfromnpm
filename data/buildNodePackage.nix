{ pkgs, nodejs }:

{
  name,
  version,
  src,
  deps ? [],
  meta ? {},
  requireNodeVersion ? null
}:

# Version must be present.
if version == "" then throw "No version specified for ${name}"
else

if requireNodeVersion != null && nodejs.version != requireNodeVersion
then throw ("package ${name}-${version} requires nodejs ${requireNodeVersion},"
            + " but passed in version is ${nodejs.version}")
else

let
  inherit (pkgs.stdenv) mkDerivation;
  inherit (pkgs.stdenv.lib) concatStringsSep flip;
  # Extract the nodejs sources to a folder. These will be used as an
  # argument to npm.
  sources = mkDerivation {
    name = "node-sources";
    buildCommand = ''
      tar --no-same-owner --no-same-permissions -xf ${nodejs.src}
      mv $(find . -type d -mindepth 1 -maxdepth 1) $out
    '';
  };

  preInstall = ''
    setupDeps() {
      # Symlink each dependency to the `node_modules` folder.
      ${if deps == [] then "" else "mkdir -p node_modules"}
      ${concatStringsSep "\n"
        (flip map deps (d: "ln -sv ${d} node_modules/${d.pkgName}"))}
      echo "Finished installing dependencies"
    }
  '';

  npmFlags = concatStringsSep " " [
    # Disable any user-level npm shenanigans
    "--userconfig /dev/null"
    "--registry http://www.example.com"
    "--nodedir=${sources}"
    "--production"
    "--fetch-retries 0"
  ];
in

mkDerivation {
  inherit meta src npmFlags;
  name = "nodejs-${name}-${version}";
  # We need to make this available to packages which depend on this, so that we
  # know what folder to put them in.
  passthru.pkgName = name;
  passthru.version = version;

  phases = ["unpackPhase" "patchPhase" "installPhase"];
  buildInputs = [nodejs];
  propagatedBuildInputs = deps;

  unpackPhase = ''
    if [ -d $src ]; then
      # case: this is not a tarball, but a directory. It must contain a
      # package.json file. We copy the whole thing into `src`.
      if [ ! -e $src/package.json ]; then
        echo "No package.json file found in source directory." 1>&2
        exit 1
      else
        # Remove the `node_modules` folder, so as not to
        cp -r $src $out
      fi
    else
      tar xf $src
      [ -d package ] || {
        echo "Tarball $src did not contain a `package` directory"
        exit 1
      }
      cp -r package $out
    fi
    cd $out
  '';

  patchPhase = ''
    patchShebangs $PWD
  '';

  shellHook = preInstall;

  installPhase = ''
    ${preInstall}
    setupDeps
    # Run the npm installer.
    ${nodejs}/bin/npm install ${npmFlags}
  '';
}
