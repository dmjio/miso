{ mkDerivation, aeson, base, bytestring, containers, http-api-data
, http-types, lucid, network-uri, servant, servant-lucid, stdenv
, text, transformers, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.15.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers http-api-data http-types lucid
    network-uri servant servant-lucid text transformers vector
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
