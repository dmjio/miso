{ mkDerivation, aeson, base, containers, miso, servant, stdenv, network-uri }:
mkDerivation {
  pname = "haskell-miso";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ aeson base containers miso servant network-uri ];
  homepage = "https://haskell-miso.org";
  description = "https://haskell-miso.org";
  license = stdenv.lib.licenses.bsd3;
}
