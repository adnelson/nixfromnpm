# Start with the base nixos image
FROM nixos/nix

# Clone the nixfromnpm directory, then enter the shell. This will
# cause all of the dependencies to be built.
RUN nix-env -iA nixpkgs.git -iA nixpkgs.bashInteractive -iA nixpkgs.coreutils --max-jobs 8
RUN cd /tmp && git clone https://github.com/adnelson/nixfromnpm && cd nixfromnpm && nix-shell --max-jobs $(nproc) --command true

# Remove the nixfromnpm repo; no reason to keep it
RUN rm -rf /tmp/nixfromnpm
