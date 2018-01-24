{ mkDerivation, base, containers, miso, stdenv }:
mkDerivation {
  pname = "miso-mario";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers miso ];
  description = "Miso's Mario example";
  license = stdenv.lib.licenses.bsd3;
  postInstall = ''
    mkdir -p $out/bin/mario.jsexe/imgs
    cp -r ./imgs $out/bin/mario.jsexe/
    cp ./index.html $out/bin/mario.jsexe/
  '';
}
