{ mkDerivation, aeson, base, containers, http-types, lucid, miso
, mtl, network-uri, servant, servant-lucid, servant-server, stdenv
, wai, wai-extra, warp
}:
mkDerivation {
  pname = "sse";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers http-types lucid miso mtl network-uri servant
    servant-lucid servant-server wai wai-extra warp
  ];
  homepage = "https://sse.haskell-miso.org";
  description = "Server sent events example";
  license = stdenv.lib.licenses.bsd3;
}
