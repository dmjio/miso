{ mkDerivation, aeson, base, bytestring, containers, lucid
, stdenv, text, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers lucid text vector
  ];
  homepage = "http://github.com/miso-haskell/miso";
  description = "Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
