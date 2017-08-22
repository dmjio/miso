{ mkDerivation, base, ghcjs-base, ghcjs-three, miso, stdenv }:
mkDerivation {
  pname = "webgl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ghcjs-base ghcjs-three miso ];
  homepage = "https://webgl.haskell-miso.org";
  description = "https://webgl.haskell-miso.org";
  license = stdenv.lib.licenses.unfree;
}
