#!/usr/bin/env bash
set -e

nix-build release.nix -A pkgs.python3Packages.nose
exec result/bin/nosetests test.py $@
