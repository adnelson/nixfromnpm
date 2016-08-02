{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
(import ./default.nix { inherit pkgs compiler; }).env
