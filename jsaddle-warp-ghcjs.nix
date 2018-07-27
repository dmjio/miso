{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "jsaddle-warp";
  version = "0.9.5.0";
  src = fetchgit {
    url = "https://github.com/ghcjs/jsaddle.git";
    sha256 = "0qdh5qdk23vcp1yp910zgw2hs4zpbx9ig25xgaax0iwj2m1ifh5x";
    rev = "34fe7d61b3f387b81aa748294ac8d993243f53b4";
  };
  postUnpack = "sourceRoot+=/jsaddle-warp; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base ];
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
