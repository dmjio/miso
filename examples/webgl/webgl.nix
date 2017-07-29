{ mkDerivation, aeson, base, containers, ghcjs-three, miso, servant
, stdenv
}:
mkDerivation {
  pname = "webgl";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers ghcjs-three miso servant
  ];
  homepage = "https://webgl.haskell-miso.org";
  description = "https://webgl.haskell-miso.org";
  license = stdenv.lib.licenses.bsd3;
}
