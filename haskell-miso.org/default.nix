{}:
with (import ../default.nix {});
let
  src = import ../nix/haskell/packages/source.nix pkgs;
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs810 ghc9101;
  client = ghcjs810.callCabal2nix "haskell-miso" (src.haskell-miso-src) {};
  server = ghc9101.callCabal2nix "haskell-miso" (src.haskell-miso-src) {};
  dev = ghc9101.callCabal2nixWithOptions "haskell-miso" (src.haskell-miso-src) "-fdev" {};
in
{ haskell-miso-client = client;
  haskell-miso-server = server;
  haskell-miso-dev = dev;
  haskell-miso-runner = 
    runCommand "haskell-miso.org" { inherit client server; } ''
      mkdir -p $out/{bin,static}
      cp ${server}/bin/* $out/bin
      ${pkgs.bun}/bin/bun build --production \
        ${client}/bin/client.jsexe/all.js > temp.js
      mv temp.js $out/static/all.js
    '';
}
