{ mkDerivation, aeson, base, bytestring, containers, file-embed
, http-api-data, http-types, jsaddle, jsaddle-warp, network-uri
, scientific, servant, stdenv, text, transformers
, unordered-containers, vector, wai, wai-app-static, warp
, websockets
}:
mkDerivation {
  pname = "miso";
  version = "1.0.0.0";
  src = ./.;
  configureFlags = [ "-fexamples" "-fjsaddle" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers file-embed http-api-data
    http-types jsaddle network-uri scientific servant text transformers
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson base containers jsaddle-warp servant wai wai-app-static warp
    websockets
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
