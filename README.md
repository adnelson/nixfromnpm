Translates NPM packages to Nix expressions.

### Installation

Nix needs to be installed.

```
$ nix-env -i
```

### Usage

For a package on the central NPM repository:

```
$ nixfromnpm "package_name" /some/path
```

For a package in a private repo:

```
$ ADDITIONAL_NPM_REGISTRIES=https://my.registry:2345 nixfromnpm "private_package" /some/path
```

