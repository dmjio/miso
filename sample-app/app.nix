{ mkDerivation, base, miso, stdenv }:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso ];
  description = "First miso app";
  license = stdenv.lib.licenses.bsd3;
}
