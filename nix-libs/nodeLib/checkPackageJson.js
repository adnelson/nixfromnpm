'use strict'
var fs = require('fs');
var semver = require(process.env.SEMVER_PATH);

// Load up the package object.
var packageObj = JSON.parse(fs.readFileSync('./package.json'));

// Exit with an error message.
function fail(msg) {
  console.error(msg);
  process.exit(1);
}

// Ensure that the declared package name matches the expected.
function checkPackageName(expectedName) {
  if (expectedName !== packageObj.name) {
    fail("Package name declared in package.json (" + packageObj.name +
         ") does not match expected name (" + expectedName + ")");
  }
}

// Returns true if a file path exists.
function exists(path) {
  try {
     fs.lstatSync(path);
     return true;
  } catch (e) {
     return false;
  }
}

// Ensure that the main entry point exists.
function checkMainEntryPoint() {
  var mainEntryPoint = packageObj.main || 'index.js';
  if ('main' in packageObj) {
    if (!(exists(mainEntryPoint) || exists(mainEntryPoint + ".js"))) {
      fail("Main entry point " + mainEntryPoint + " does not exist");
    }
  }
}

// Check that all dependencies of a package have been satisfied. This
// should only be called when npm fails, assuming it failed because of
// a missing dependency.
function checkDependencies() {
  // This will be keyed on the dependency name and version, and valued with
  // the error.
  var errorsFound = {}

  // Given the name and version range of a package, check:
  // * That a package with the given name exists in the node_modules folder.
  // * That its version satisfies the given version bounds.
  function checkDependency(name, versionRange, dependencyType) {
    process.stderr.write("Checking dependency " + name + "@" + versionRange +
                         "(from " + dependencyType + ")...");
    var dependencyPackageObj;
    var pkgJsonPath = process.cwd() + "/node_modules/" + name + "/package.json";
    var errorKey = name + "@" + versionRange;
    try {
      dependencyPackageObj = JSON.parse(fs.readFileSync(pkgJsonPath));
    } catch (e) {
      var message = "Not found in node_modules";
      // Case: the file didn't exist
      errorsFound[errorKey] = message;
      console.error("ERROR: " + message);
      return
    }
    // Check that the version matches
    var version = dependencyPackageObj.version;
    if (!semver.satisfies(version, versionRange)) {
      var message = "version " + version + " doesn't match range " + versionRange;
      errorsFound[errorKey] = message
      console.error("ERROR: " + message);
      return
    }
    console.error("OK");
  }

  // Verify that all of the declared dependencies in a package.json file
  // are satisfied by the environment.
  var depTypes = ["dependencies", "devDependencies", "peerDependencies"];
  for (var depTypeIdx in depTypes) {
    var depType = depTypes[depTypeIdx];
    if (depType === "devDependencies" && process.env.NO_DEV_DEPENDENCIES) {
      continue;
    }
    console.log("Checking " + depType);
    if (packageObj[depType]) {
      for (var depName in packageObj[depType]) {
        checkDependency(depName, packageObj[depType][depName], depType);
      }
    }
  }

  if (JSON.stringify(errorsFound) !== "{}") {
    console.error("Found the following errors:");
    for (var depName in errorsFound) {
      console.error(depName + ":  " + errorsFound[depName]);
    }
    fail("One or more dependencies were unsatisfied. :(");
  }
}

switch (process.argv[2]) {
  case 'checkPackageName': {
    if (!process.argv[3]) {
      fail("Need an argument for the package name.");
    }
    checkPackageName(process.argv[3]); break;
  }
  case 'checkMainEntryPoint': {
    checkMainEntryPoint(); break;
  }
  case 'checkDependencies': {
    checkDependencies(); break;
  }
}
