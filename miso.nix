{ mkDerivation, aeson, base, bytestring, containers, lucid
, network-uri, stdenv, text, vector, ghcjs-base
}:
mkDerivation {
  pname = "miso";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers lucid network-uri text vector ghcjs-base
  ];
  homepage = "http://github.com/miso-haskell/miso";
  description = "Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
