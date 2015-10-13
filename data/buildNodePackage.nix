{ pkgs, nodejs }:

let

pkg = {
  name,
  version,
  src,
  deps ? [],
  devDeps ? [],
  devsDefined ? false,
  doCheck ? devsDefined,
  meta ? {},
  requireNodeVersion ? null
}@args:

let
  deps' = map (p: p.withoutTests) deps;
  devDeps' = map (p: p.withoutTests) devDeps;
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
  inherit (pkgs.stdenv.lib) concatStringsSep flip;
  deps = deps';
  devDeps = devDeps';
  shouldTest = devsDefined && doCheck;
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
      ${if (deps ++ devDeps) == [] then "" else "mkdir -p node_modules"}
      ${concatStringsSep "\n"
        (flip map deps (d: "ln -sv ${d} node_modules/${d.pkgName}"))}
      ${if !shouldTest then "" else concatStringsSep "\n"
        (flip map devDeps (d: "ln -sv ${d} node_modules/${d.pkgName}"))}
      echo "Finished installing dependencies"
    }
    runInstall() {
      ${nodejs}/bin/npm install ${npmFlags}
    }
    removePeerDependencies() {
      cat <<EOF | ${pkgs.python.executable}
    import json
    with open('package.json') as f:
        pkg = json.load(f)
    if 'peerDependencies' in pkg:
        print("Warning: removing peer dependencies of ${name}@${version}:")
        for name, ver in pkg['peerDependencies'].items():
            print(name + '@' + ver)
        del pkg['peerDependencies']
        with open('package.json', 'w') as f:
            f.write(json.dumps(pkg, indent=2))
    EOF
    }
  '';

  npmFlags = concatStringsSep " " ([
    # Disable any user-level npm shenanigans
    "--userconfig /dev/null"
    "--registry http://www.example.com"
    "--nodedir=${sources}"
    "--production"
    "--fetch-retries 0"
  ] ++
    # Run the tests if we have defined dev dependencies
    pkgs.lib.optional shouldTest "--npat");
  removePeerDependencies = ''
  '';

  result = mkDerivation {
    inherit meta src npmFlags;
    name = "nodejs-${name}-${version}";
    # We need to make this available to packages which depend on this, so that we
    # know what folder to put them in.
    passthru.pkgName = name;
    passthru.version = version;
    passthru.withoutTests = pkg (args // {doCheck = false;});

    phases = ["unpackPhase" "checkPhase" "patchPhase" "installPhase"];
    buildInputs = [pkgs.python nodejs];
    propagatedBuildInputs = deps;

    patchPhase = ''
      patchShebangs $PWD
    '';

    shellHook = preInstall;

    installPhase = ''
      cp -r $src $out
      chmod -R +w $out
      cd $out
      ${preInstall}
      setupDeps
      removePeerDependencies
      runInstall
    '';
  };
in

result;
in
pkg
