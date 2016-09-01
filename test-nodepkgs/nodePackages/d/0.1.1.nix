{ buildNodePackage, nodePackages, pkgs }:
buildNodePackage {
    name = "d";
    version = "0.1.1";
    src = pkgs.fetchurl {
      url = "https://registry.npmjs.org/d/-/d-0.1.1.tgz";
      sha1 = "da184c535d18d8ee7ba2aa229b914009fae11309";
    };
    circularDependencies = with nodePackages; [
      es5-ext_0-10-12
    ];
    meta = {
      homepage = "https://github.com/medikoo/d";
      description = "Property descriptor factory";
      keywords = [
        "descriptor"
        "es"
        "ecmascript"
        "ecma"
        "property"
        "descriptors"
        "meta"
        "properties"
      ];
    };
  }
