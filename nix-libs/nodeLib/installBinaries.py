# This script will read the package.json file and create a symlink for each
# item under the `bin` key. The bin can be a string or a dictionary.
# See: https://docs.npmjs.com/files/package.json#bin
import json
import os
from os.path import join, isdir, normpath
import sys

with open('package.json', 'r') as f:
    package = json.load(f)

if 'bin' not in package:
    sys.exit(0)

_bin = package['bin']

if isinstance(_bin, basestring):
    # This is equivalent to a singleton dictionary where the key is the
    # name of the package.
    _bin = {package['name']: _bin}
elif not isinstance(_bin, dict):
    # Otherwise it must be a dictionary.
    sys.exit("Expected `bin` key in package.json to point to a string "
             "or a dict, but it is '{}', of type '{}'"
             .format(_bin, type(_bin).__name__))

out_dir = os.environ['out']

# Create the .bin folder
bin_folder = join(out_dir, 'bin')
if not isdir(bin_folder):
    os.makedirs(bin_folder)

print("Creating binaries in {}".format(bin_folder))

for bin_name, bin_path in _bin.items():
    # Get the absolute path of the script being pointed to.
    bin_abs_path = normpath(join(out_dir, 'lib', 'node_modules',
                            package['name'], bin_path))
    print("Linking binary {} to {}".format(bin_name, bin_abs_path))
    os.symlink(bin_abs_path, join(bin_folder, bin_name))
