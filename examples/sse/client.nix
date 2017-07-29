{ mkDerivation, aeson, base, containers, miso, network-uri, servant
, stdenv
}:
mkDerivation {
  pname = "sse";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers miso network-uri servant
  ];
  homepage = "https://sse.haskell-miso.org";
  description = "Server sent events example";
  license = stdenv.lib.licenses.bsd3;
}
