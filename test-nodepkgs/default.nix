{ nodejs ? pkgs.nodejs-4_x, npm3 ? false, pkgs ? import <nixpkgs> {} }:
let
    mkNodeLib = import ./nodeLib {
      self = mkNodeLib;
    };
    nodeLib = mkNodeLib {
      inherit pkgs npm3 nodejs;
    };
    in nodeLib.generatePackages {
      nodePackagesPath = ./nodePackages;
    }
