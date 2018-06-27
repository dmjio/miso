{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, http-api-data, http-types, network-uri, QuickCheck
, quickcheck-instances, scientific, servant, stdenv, text
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.20.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghcjs-base http-api-data
    http-types network-uri scientific servant text transformers
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson base bytestring containers ghcjs-base http-api-data
    http-types network-uri QuickCheck quickcheck-instances scientific
    servant text transformers unordered-containers vector
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
