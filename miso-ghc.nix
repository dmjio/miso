{ mkDerivation, aeson, base, bytestring, containers, lucid
, stdenv, text, vector, BoundedChan
}:
mkDerivation {
  pname = "miso";
  version = "0.1.5.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers lucid text vector BoundedChan
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
