{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./project.nix {
  inherit (nixpkgs) curl cacert zsh;
}
