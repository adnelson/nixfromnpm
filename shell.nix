{
  nixpkgs ? import ./nix/nixpkgs.nix,
  compiler ? null
}:
  (import ./release.nix { inherit nixpkgs compiler; }).nixfromnpm.env
