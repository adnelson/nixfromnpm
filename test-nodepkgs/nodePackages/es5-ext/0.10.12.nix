{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "es5-ext";
    version = "0.10.12";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/es5-ext/-/es5-ext-0.10.12.tgz";
      sha1 = "aa84641d4db76b62abba5e45fd805ecbab140047";
    };
    circularDependencies = with nodePackages; [
      es6-iterator_2-0-0
      es6-symbol_3-1-0
    ];
    meta = {
      homepage = "https://github.com/medikoo/es5-ext#readme";
      description = "ECMAScript extensions and shims";
      keywords = [
        "ecmascript"
        "ecmascript5"
        "ecmascript6"
        "es5"
        "es6"
        "extensions"
        "ext"
        "addons"
        "extras"
        "harmony"
        "javascript"
        "polyfill"
        "shim"
        "util"
        "utils"
        "utilities"
      ];
    };
  }
