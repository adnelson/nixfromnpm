{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "es6-symbol";
    version = "3.1.0";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/es6-symbol/-/es6-symbol-3.1.0.tgz";
      sha1 = "94481c655e7a7cad82eba832d97d5433496d7ffa";
    };
    circularDependencies = with nodePackages; [
      es5-ext_0-10-12
      d_0-1-1
    ];
    meta = {
      homepage = "https://github.com/medikoo/es6-symbol#readme";
      description = "ECMAScript 6 Symbol polyfill";
      keywords = [
        "symbol"
        "private"
        "property"
        "es6"
        "ecmascript"
        "harmony"
        "ponyfill"
        "polyfill"
      ];
    };
  }
