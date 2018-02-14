# A python-based fetchurl function, allowing the passage of an auth
# header via namespaceTokens.  Just calls into `requests` under the hood.
{
  pythonPackages, stdenv, namespaceTokens
}:


{ # URL to fetch.
  url ? ""

, # Additional curl options needed for the download to succeed.
  curlOpts ? ""

, # Name of the file.  If empty, use the basename of `url' (or of the
  # first element of `urls').
  name ? ""

  # Different ways of specifying the hash.
, outputHash ? ""
, outputHashAlgo ? ""
, md5 ? ""
, sha1 ? ""
, sha256 ? ""

, # Meta information, if any.
  meta ? {}

  # Namespace to use (checked for in namespaceTokens)
, namespace ? null
}:

let
  auth = if namespace != null && builtins.hasAttr namespace namespaceTokens
         then namespaceTokens.${namespace}
         else null;
  inherit (stdenv.lib) flip mapAttrs' nameValuePair;
  hasHash = (outputHash != "" && outputHashAlgo != "")
            || md5 != "" || sha1 != "" || sha256 != "";
in

if !hasHash
then throw "You must specify the output hash for ${url}"
else

stdenv.mkDerivation {
  inherit url auth;
  name = if name != "" then name else baseNameOf (toString url);

  outputHashAlgo = if outputHashAlgo != "" then outputHashAlgo else
      if sha256 != "" then "sha256" else if sha1 != "" then "sha1" else "md5";
  outputHash = if outputHash != "" then outputHash else
      if sha256 != "" then sha256 else if sha1 != "" then sha1 else md5;

  # Only flat hashing, which is the normal mode if you're fetching a file.
  outputHashMode = "flat";

  # Doing the download on a remote machine just duplicates network
  # traffic, so don't do that.
  preferLocalBuild = true;

  buildInputs = with pythonPackages; [python requests];
  buildCommand = ''
    python ${./fetch.py}
  '';
}
