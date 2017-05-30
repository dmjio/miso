{ mkDerivation, aeson, base, containers, bytestring
, lucid, servant, stdenv, text, vector, ghcjs-base, network-uri
}:
mkDerivation {
  pname = "miso";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers lucid text vector ghcjs-base
    bytestring network-uri
  ];
  homepage = "http://github.com/miso-haskell/miso";
  description = "Micro-isomorphic UI framework";
  license = stdenv.lib.licenses.bsd3;
}
