# Test nixfromnpm operation
set -ex

for command in nix-build nixfromnpm; do
  if ! type -p "$command"; then
    echo "Command $command was not found" >&2
    exit 1
  fi
done

# Confirm that the nixfromnpm help commmand works
nixfromnpm --help >/dev/null

PROJECT_DIR=$PWD

# Cleanup code
function cleanup() {
  cd $PROJECT_DIR
  [[ -d "$OUTPUT" ]] && rm -rf $OUTPUT
  [[ -d "$WORKING_DIR" ]] && rm -rf $WORKING_DIR
}
trap cleanup EXIT

# Create a directory to store generated expressions, and a working directory.
OUTPUT=$(mktemp -d)

# Make a current working directory
WORKING_DIR=$(mktemp -d)

cd $WORKING_DIR

function generate() { nixfromnpm -o $OUTPUT $@; }
function build() { nix-build $OUTPUT $@; }

# Generate expressions for and build lodash, which has no dependencies
generate -p lodash%4.17.0
build -A nodePackages.lodash_4-17-0
ls result/lib/node_modules/lodash >/dev/null

# Build a complicated package, which has dependencies
generate -p browserify
build -A nodePackages.browserify

# Build all of the top packages
cat $PROJECT_DIR/top-packages.txt | while read packagename; do
  generate -p $packagename
  build -A nodePackages.$packagename
done

# Build a package with dev dependencies

# Build a package in a namespace
generate -p '@types/node'
build -A nodePackages.namespaces.types.node

# Generate an extension library. Confirm that package definitions are
# not duplicated, and that things can be built.
