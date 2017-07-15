{ mkDerivation, aeson, base, bytestring, containers, lucid
, stdenv, text, vector, BoundedChan, servant, servant-lucid
}:
mkDerivation {
  pname = "miso";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers lucid text vector BoundedChan servant
    servant-lucid
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
