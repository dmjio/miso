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
    cp ${server}/bin/* $out/bin
    ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/client.jsexe/all.externs.js \
      ${client}/bin/client.jsexe/all.js > temp.js
    mv temp.js $out/static/all.js
  '';
  sse-client = client;
  sse-server = server;
  inherit pkgs;
}
