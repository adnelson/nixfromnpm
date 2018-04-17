{nixpkgs ? import ./nix/17_09.nix}:
  (import ./release.nix {inherit nixpkgs;}).nixfromnpm.env
