{ mkDerivation, stdenv, miso, base, aeson, text, containers }:
mkDerivation {
  pname = "miso-todomvc";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    miso base aeson text containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
