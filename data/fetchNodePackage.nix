{fetchurl, lib, stdenv}:

fetchurl

# {subPath ? "package", ...}@args:

# let
#   fetchResult = fetchurl (builtins.removeAttrs args ["subPath"]);
#   subpathName = lib.replaceChars ["/"] ["-"] subPath;
# in

# stdenv.mkDerivation {
#   name = "npmfetch-${subpathName}";
#   src = fetchResult;
#   buildCommand = ''
#     tar -xf $src -C $TMPDIR
#     cp -r $TMPDIR/${subPath} $out
#   '';
# }
