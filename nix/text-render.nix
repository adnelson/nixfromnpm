{ mkDerivation, base, classy-prelude, mtl, parsec, stdenv, text }:
mkDerivation {
  pname = "text-render";
  version = "0.1.0.3";
  sha256 = "1p78xsr25qxmfgsl73lzfn7j32ni897667k48448fkihdsg0a15g";
  libraryHaskellDepends = [ base classy-prelude mtl parsec text ];
  homepage = "http://github.com/thinkpad20/text-render";
  description = "A type class for rendering objects as text, pretty-printing, etc";
  license = stdenv.lib.licenses.mit;
}
