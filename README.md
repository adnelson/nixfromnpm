# nixfromnpm

Translate NPM packages to Nix expressions.

### What it does

Given the name of one or more packages and an output directory, queries NPM repositories for those packages' definitions and those of their dependencies, and generates a set of nix expressions in the given output directory which not only can be used to build the requested packages, but also to build any of their dependencies as desired.

* The packages generated are easily human readable.
* They can be modified as desired after they are built, and these modifications will remain in place. For example, if a package relies on non-NPM dependencies, or requires extra build steps such as patching, these changes can be added by hand and will be respected down the line.
* Since builds use pre-existing packages, repeated expression generation is fast. For example, if you generate the expression for one package, and then generate the expression for another package which shares dependencies with the first (even if the second is built at a later time), the shared dependencies will not be regenerated.

### Advantages over `npm2nix`

`npm2nix` generates the entire dependency tree for an npm package. This is inefficient, since duplicated packages are built multiple times. The resulting expression is a single monolithic file which is hard to grok, and hard to modify. Furthermore, any modifications performed would have to be done each time the package was regenerated. It also discourages committing of the resulting package into source control, since it's large and has to be continually regenerated whenever changes are made. This means that packages built with it are unlikely to be cached in a nix store or repo.

### Installation

First, nix needs to be installed. Then to install `nixfromnpm`, just use nix. Make sure `nixpkgs` is in your `NIX_PATH`, and then run:

```bash
$ nix-env -f /path/to/nixfromnpm -i
```

If you'd like to try out `nixfromnpm` or hack on it, you can use it in a `nix-shell`:

```bash
$ nix-shell /path/to/nixfromnpm
$ cabal configure
$ cabal run -- <arguments>
```

### Usage

#### Generating an expression for a package

The most basic usage is providing `-p` (`--package`) and `-o` (`--output`) flags:

```bash
$ nixfromnpm -p package_name -o /some/path
```

This will build the package called `package_name` and put all of the generated expressions in `/some/path`.

You can also specify a version bound on the package you are fetching, using `@`:

```bash
$ nixfromnpm -p package_name@version_bound -o /some/path
```

#### Generating an expression from a package.json file

**Note:** this feature is not yet implemented.

You can also generate an expression for a project on the local disk by passing in the path to a `package.json` file. The generated expressions will be placed by default in the same directory as the `package.json` file in a `nix` folder, and a `default.nix` will be created in the directory as well.

```bash
$ nixfromnpm -f /path/to/package.json
```

If a different output path is desired, `-o` can be used. The creation of `default.nix` can be disabled with `--no-default-nix`.

```bash
$ nixfromnpm -f /path/to/package.json -o /path/to/expressions --no-default-nix
```

This will build the package called `package_name` and put all of the generated expressions in `/some/path`.

#### Development Dependencies

NPM packages differentiate between dependencies needed at runtime and dependencies required only when developing on a package (e.g. packages for testing, transpilers for compile-to-javascript languages, etc). A package might have a great number of development dependencies, and there might be circular dependencies with development packages because the packages are not required at runtime. For this reason it's generally better to avoid generating expressions for development dependencies unless they're needed, because they add a lot of extra work and generate a lot of files.

`nixfromnpm` lets you generate expressions for development dependencies at a maximum depth. For example, depth `0` means don't make any development dependency expressions; depth `1` means create expressions for the package being built, but not any of their development dependencies, etc.

```bash
$ nixfromnpm -p package_name -o /some/path --dev-depth 1
```

#### Extra registries

For a package in a private registry located at `https://my.registry:2345`:

```bash
$ nixfromnpm -p private_package -o /some/path -r https://my.registry:2345
```

#### Github authorization

For npm packages which fetch from git, if an authorization token is required:

```bash
$ nixfromnpm -p package -o /some/path --github-token llnl23uinlaskjdno34nedhoaidjn5o48wugn
```

This can also be set by a `GITHUB_TOKEN` environment variable.

#### Caching of packages

By default, `nixfromnpm` will discover all existing packages in the specified output directory (provided via tha `-o` flag). However, if you would like to generate all of these from scratch, you can disable caching with `--no-cache`.

#### Extending package libraries

If there is an existing set of packages located in `/path/to/library`, you might want to build a new set of packages without modifying the existing set. For example, it might be read-only, or you might want to separate private packages from public. You can do this with extensions:


```bash
$ nixfromnpm -p package -o /some/path --extend /path/to/library
```

This will discover all of the packages in `/path/to/library` and make them available in the generated expressions at `/some/path`, but will not modify the library at all. Extensions can also be specified with `-e`

You can pass in multiple libraries with multiple invocations of `--extend`, but the libraries must have distinct base names (e.g. `/path/lib1`, `/path/lib2`). If two libraries share a base name, you can rename them with a `NAME=PATH` syntax, e.g. `-E lib1=/path1/lib -E lib2=/path2/lib`.
