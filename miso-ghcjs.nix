{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, network-uri, scientific, stdenv, text, transformers
, unordered-containers, vector, hspec, hspec-core, servant
}:
mkDerivation {
  pname = "miso";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghcjs-base network-uri scientific
    text transformers unordered-containers vector hspec hspec-core servant
  ];
  homepage = "http://github.com/miso-haskell/miso";
  description = "Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
