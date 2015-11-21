# nixfromnpm

Translate NPM packages to Nix expressions.

### What it does

Given the name of one or more packages and an output directory, queries NPM repositories for those packages' definitions and those of their dependencies, and generates a set of nix expressions in the given output directory which not only can be used to build the requested packages, but also to build any of their dependencies as desired.

* The packages generated are easily human readable.
* They can be modified as desired after they are built, and these modifications will remain in place. For example, if a package relies on non-NPM dependencies, or requires extra build steps such as patching, these changes can be added by hand and will be respected down the line.
* Since builds use pre-existing packages, repeated expression generation is fast. For example, if you generate the expression for one package, and then generate the expression for another package which shares dependencies with the first (even if the second is built at a later time), the shared dependencies will not be regenerated.

### Advantages over `npm2nix`

`npm2nix` is another tool which can generate nix expressions from an npm package. However, it has several drawbacks. It generates the entire dependency tree for an npm package, without availing itself of results of previous invocations. This is inefficient, since duplicated packages are built multiple times. The resulting expression is a single monolithic file which is hard to grok, and hard to modify. Furthermore, any modifications performed would have to be done each time the package was regenerated. It also discourages committing of the resulting package into source control, since it's large and has to be continually regenerated whenever changes are made. This means that packages built with it are unlikely to be cached in a nix store or repo.

### Installation

Clone the `nixfromnpm` repo, and two repos containing dependencies not yet released in `nixpkgs`. Note that all of these repos need to share a parent directory.

```bash
$ git clone https://github.com/adnelson/nixfromnpm
$ git clone https://github.com/adnelson/semver-range
$ git clone https://github.com/jwiegley/hnix
```

Make sure you have nix installed, and `nixpkgs` is in your `NIX_PATH` environment variable. Then run:

```bash
$ nix-env -f ./nixfromnpm -i
```

If you'd like to try out `nixfromnpm` without installing it, or just hack on it, you can use it in a `nix-shell`:

```bash
$ cd /path/to/nixfromnpm
$ nix-shell --pure
[nix-shell:nixfromnpm]$ cabal run -- <arguments>
```

### Usage

#### Generating an expression for a package

The most basic usage is providing an `-o` (`--output`) flag and one or more `-p` flags:

```bash
$ nixfromnpm -o /some/path -p package_name -p other_package_name
```

This will build the packages called `package_name` and `other_package_name`, and put all of the generated expressions in `/some/path`.

You can also specify a version bound on the packages you are fetching, using `@`:

```bash
$ nixfromnpm -p package_name@version_bound -o /some/path
```

Any NPM version bound is valid; so for example:

```bash
$ nixfromnpm -p foo@0.8.6 -o /some/path
$ nixfromnpm -p 'foo@>=0.8 <0.9' -o /some/path
$ nixfromnpm -p 'foo@~1.0.0' -o /some/path
```

#### Generating an expression from a package.json file

You can also generate an expression for a project on the local disk by passing in the path to a `package.json` file, or a directory containing one. The generated expression will be placed in a file called `project.nix` in the same directory as the `package.json` file, and a `default.nix` will be created in the directory as well which calls into `project.nix`. As with normal usage, the `-o` flag is used to specify a path to where generated expressions will be placed; however, only the *downsteam* dependencies will be put here, while the expression itself will be in the `project.nix` file.

```bash
$ nixfromnpm -f /path/to/package.json -o /path/to/dependency/set
```

You can give multiple `-f` arguments to build multiple expressions on disk, and it can be used alongside `-p` arguments as well.

#### Extending package libraries

If there is an existing set of packages located in `/path/to/library`, you might want to build a new set of packages without modifying the existing set. For example, it might be read-only, or you might want to separate private packages from public. You can do this with extensions:


```bash
$ nixfromnpm -p package -o /some/path --extend /path/to/library
# OR
$ nixfromnpm -f /path/to/package -o /some/path --extend /path/to/library
```

This will discover all of the packages in `/path/to/library` and make them available in the generated expressions at `/some/path`, but will not modify the library at all. Extensions can also be specified with `-e`

You can pass in multiple libraries with multiple invocations of `--extend`, but the libraries must have distinct base names (e.g. `/path/lib1`, `/path/lib2`). If two libraries share a base name, you can rename them with a `NAME=PATH` syntax, e.g. `-E lib1=/path1/lib -E lib2=/path2/lib`.

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

#### Private NPM namespaces

NPM offers private packaging, where you can specify a *namespace* or
*scope* under which a package lives. For example, a package might be
designated as `@foo/bar`, where the package is called `bar` and the
namespace is called `foo`. `nixfromnpm` supports this, as long as you
have set up your NPM repo to use authorization tokens (see
documentation for details). To use this, set an environment variable
`NPM_AUTH_TOKENS`, with the following format:

```
mynamespace=mytoken:myothernamespace=myothertoken:...
```

Where tokens are keyed on namespaces. Then when building the
expression set, if a namespaced package is encountered, `nixfromnpm`
will look up the namespace in this environment variable to determine
what token to use for authentication. The same environment variable is
read when the packages are built with `nix-build`. The path to the
package in the generated expressions is slightly different:

```bash
$ export NPM_AUTH_TOKENS="foo=a1a1a1a1a1a1a1a1a1a"
$ nixfromnpm -o my_expressions -p '@foo/bar'
$ nix-build my_expressions -A namespaces.foo.bar
```

#### Caching of packages

By default, `nixfromnpm` will discover all existing packages in the specified output directory (provided via tha `-o` flag). However, if you would like to generate all of these from scratch, you can disable caching with `--no-cache`.
