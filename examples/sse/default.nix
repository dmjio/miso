{}:
with (import ../.. {});
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs8107 ghc982;
  client = ghcjs8107.callCabal2nix "sse" ./. {};
  server = ghc982.callCabal2nix "sse" ./. {};
in
{
  sse-runner = runCommand "sse.haskell-miso.org" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    ${pkgs.bun}/bin/bun build --production \
      ${client}/bin/client.jsexe/all.js > temp.js
    mv temp.js $out/static/all.js
  '';
  sse-client = client;
  sse-server = server;
  inherit pkgs;
}
