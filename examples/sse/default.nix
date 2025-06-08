{}:
with (import ../.. {});
let
  sources = import ../../nix/source.nix pkgs;
  inherit (pkgs) runCommand closurecompiler;
  ghc = legacyPkgs.haskell.packages.ghc865;
  ghcjs = legacyPkgs.haskell.packages.ghcjs;
  client = ghcjs.callCabal2nix "sse" (sources.sse) {};
  server = ghc.callCabal2nix "sse" (sources.sse) {};
in
{
  sse-runner = runCommand "sse.haskell-miso.org" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    cp -v ${client}/bin/client.jsexe/all.js $out/static/all.js
  '';
  sse-client = client;
  sse-server = server;
  inherit pkgs;
}
