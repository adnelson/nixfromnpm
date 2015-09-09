{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
(import ./project.nix { inherit nixpkgs compiler; }).env
