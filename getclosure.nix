with import <nixpkgs> {};
with lib;
with builtins;
let
  for = flip map;

getClosure = results: pkgList:
    fold (a: b: a // b) {} (
      for pkgList (pkg: trace "${pkg.name} ${concatStringsSep " " (attrValues pkg.runtimeDependencies)}" (
        # Don't recur if we've already found this package.
        if hasAttr pkg.name results then {} else
        # Else add the subpackage to the results set and recur on it.
        let results' = results // {"${pkg.name}" = pkg;}; in
        results' // getClosure results' (attrValues pkg.runtimeDependencies)
    ))
);
np = (import ./test-nodepkgs {}).nodePackages;

x = getClosure {} [np.es5-ext];

in
# {inherit getClosure np x;}

x
