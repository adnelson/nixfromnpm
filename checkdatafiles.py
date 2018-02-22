import re, os
cabal = open("nixfromnpm.cabal").read()
cabal = re.sub(r"--.*\n", "", cabal)
match = re.search("data-files: +(([\w/\.-]+) *(?:\n *, ([\w/\.-]+) *)+)", cabal)
nix_libs_files = set(match.group(1).replace(",", "").split())
discovered_files = set()

for directory, _, files in os.walk("nix-libs"):
    for fname in files:
        if not (fname.endswith("~") or fname == ".gitignore"):
            discovered_files.add(os.path.join(directory, fname))

if nix_libs_files != discovered_files:
    msg = ("The following files in were found in nix-libs but not listed "
           "in the data-files section of nixfromnpm.cabal: {}"
           .format(discovered_files - nix_libs_files))
    exit(msg)
