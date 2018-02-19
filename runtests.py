#!/usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.pyyaml
"""Run the tests in the circleci config, for local dev."""
from os.path import dirname, join
from subprocess import call
import yaml

with open(join(dirname(__file__), ".circleci", "config.yml")) as f:
    circle_cfg = yaml.load(f)

results = {}
failures = 0

for step in circle_cfg["jobs"]["build"]["steps"]:
    try:
        name, command = step["run"]["name"], step["run"]["command"]
    except (TypeError, KeyError):
        continue
    print("Running", name)
    if call(command, shell=True) == 0:
        results[name] = "PASSED"
    else:
        results[name] = "FAILED"
        failures += 1

print("\nRESULTS:\n")
for name, passed in results.items():
    print("*", name, "PASSED" if passed else "FAILED")

exit(failures)
