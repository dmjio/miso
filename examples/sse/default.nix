{}:
with (import ../.. {});
let
  inherit (pkgs) runCommand closurecompiler;
  ghc = pkgs.haskell.packages.ghc9122;
  ghcjs = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122;
  client = ghcjs.callCabal2nix "sse" ./. {};
  server = ghc.callCabal2nix "sse" ./. {};
in
{
  sse-runner = runCommand "sse.haskell-miso.org" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp -v ${server}/bin/* $out/bin
    cp -v ${client}/bin/client.jsexe/all.js $out/static/all.js
  '';
  sse-client = client;
  sse-server = server;
  inherit pkgs;
}
