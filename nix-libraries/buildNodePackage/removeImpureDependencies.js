// These packages come packaged with nodejs.
var fs = require('fs');
var url = require('url');

function versionSpecIsImpure(versionSpec) {
  // Returns true if a version spec is impure.
  return (versionSpec == "latest" || versionSpec == "unstable" ||
          // file path references
          versionSpec.substr(0, 2) == ".." ||
          versionSpec.substr(0, 2) == "./" ||
          versionSpec.substr(0, 2) == "~/" ||
          versionSpec.substr(0, 1) == '/' ||
          // github owner/repo references
          /^[^/]+\/[^/]+(#.*)?$/.test(versionSpec) ||
          // is a URL
          url.parse(versionSpec).protocol);
}

// Load up the package object.
var packageObj = JSON.parse(fs.readFileSync('./package.json'));

// Purify dependencies.
var depTypes = ['dependencies', 'devDependencies', 'optionalDependencies'];
for (var i in depTypes) {
  var depType = depTypes[i];
  var depSet = packageObj[depType];
  if (depSet !== undefined) {
    for (var depName in depSet) {
      if (versionSpecIsImpure(depSet[depName])) {
        depSet[depName] = '*';
      }
    }
  }
}

/* Remove peer dependencies */
if (process.env.removePeerDependencies && packageObj.peerDependencies) {
  console.log("WARNING: removing the following peer dependencies:");
  for (key in packageObj.peerDependencies) {
    console.log("  " + key + ": " + packageObj.peerDependencies[key]);
  }
  delete packageObj.peerDependencies;
}

/* Write the fixed JSON file */
fs.writeFileSync("package.json", JSON.stringify(packageObj));
