{
  nixpkgs ? import ./nix/17_09.nix,
  compiler ? null
}:
  (import ./release.nix {inherit nixpkgs compiler;}).nixfromnpm.env
