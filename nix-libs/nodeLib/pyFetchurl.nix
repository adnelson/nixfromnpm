# A python-based fetchurl function, allowing the use of authorization headers.
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
  # the header name prefixed with __CURL_HEADER.
  headerValues = flip mapAttrs' headers (headerName: headerValue:
   nameValuePair "__HTTP_HEADER_${headerName}" headerValue);
in

if !hasHash
then throw "You must specify the output hash for ${url}"
else

stdenv.mkDerivation ({
  inherit url;
  name = if name != "" then name
         else baseNameOf (toString url);

  outputHashAlgo = if outputHashAlgo != "" then outputHashAlgo else
      if sha256 != "" then "sha256" else if sha1 != "" then "sha1" else "md5";
  outputHash = if outputHash != "" then outputHash else
      if sha256 != "" then sha256 else if sha1 != "" then sha1 else md5;

  outputHashMode = "flat";

  # Doing the download on a remote machine just duplicates network
  # traffic, so don't do that.
  preferLocalBuild = true;

  headerNames = builtins.attrNames headers;

  buildInputs = with pythonPackages; [python requests2];
  buildCommand = ''
    cat <<EOF | python
    import os
    import requests
    out = os.environ['out']
    url = os.environ['url']
    headers = {"User-Agent": "nix-fetchurl"}
    header_names = os.environ.get("headerNames", "")
    for name in header_names.split():
        if "__HTTP_HEADER_{}".format(name) not in os.environ:
            exit("FATAL: no corresponding value set for header {}"
                 .format(name))
        headers[name] = os.environ["__HTTP_HEADER_{}".format(name)]
    print('GET {} with headers {}'.format(url, headers))
    response = requests.get('$url', headers=headers)
    if response.status_code != 200:
        exit("$url returned a status code of {}\nContent:\n"
             .format(response.status_code, response.content))
    with open(out, 'wb') as f:
        f.write(response.content)
    EOF
  '';
} // headerValues)
