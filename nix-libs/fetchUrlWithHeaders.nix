# A python-based fetchurl function, allowing the passage of custom headers.
# Just calls into `requests` under the hood.
{
  pythonPackages, stdenv
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

  # Headers to set, if any.
, headers ? {}
}:

let
  inherit (stdenv.lib) flip mapAttrs' nameValuePair;
  hasHash = (outputHash != "" && outputHashAlgo != "")
            || md5 != "" || sha1 != "" || sha256 != "";

  # Create an attribute set translating each header name and value into
  # the header name prefixed with __HTTP_HEADER. When the derivation is
  # evaluated, the script will pick up these environment variables and use
  # them to produce the actual headers.
  headerValues = flip mapAttrs' headers (headerName: headerValue:
    nameValuePair "__HTTP_HEADER_${headerName}" headerValue);
in

if !hasHash
then throw "You must specify the output hash for ${url}"
else

stdenv.mkDerivation ({
  inherit url;
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

  headerNames = builtins.attrNames headers;

  buildInputs = with pythonPackages; [python requests2];
  buildCommand = ''
    python ${./fetch.py}
  '';
} // headerValues)
