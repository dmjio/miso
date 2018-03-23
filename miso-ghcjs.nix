{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, network-uri, scientific, stdenv, stm, text, transformers
, unordered-containers, vector, hspec, hspec-core, servant
, http-types, http-api-data, QuickCheck, quickcheck-instances
}:
mkDerivation {
  pname = "miso";
  version = "0.15.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghcjs-base network-uri scientific stm
    text transformers unordered-containers vector hspec hspec-core servant
    http-types http-api-data QuickCheck quickcheck-instances
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
