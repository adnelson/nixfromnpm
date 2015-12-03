// Given an expected name of a package, checks that this matches the
// name declared in the package.json file.

// Pull the expected name out of the arguments. For example, the expected
// name might be 'foo' or '@foo/bar'.
var fs = require('fs');
var expectedName = process.argv[2];

// Load up the package object.
var packageObj = JSON.parse(fs.readFileSync('./package.json'));

// Ensure that they match.
if (expectedName !== packageObj['name']) {
  console.error("Package name declared in package.json ("
                + packageObj['name'] + ") does not match expected name ("
                + expectedName + ")");
  process.exit(1);
}
