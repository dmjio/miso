{}:
with (import ../default.nix {});
let
  src = import ../nix/haskell/packages/source.nix pkgs;
  inherit (pkgs) runCommand closurecompiler;
  ghc = pkgs.haskell.packages.ghc9122;
  ghcjs = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122;
  client = ghcjs.callCabal2nix "haskell-miso" (src.haskell-miso-src) {};
  server = ghc.callCabal2nix "haskell-miso" (src.haskell-miso-src) {};
  dev = ghc.callCabal2nixWithOptions "haskell-miso" (src.haskell-miso-src) "-fdev" {};
in
{ haskell-miso-client = client;
  haskell-miso-server = server;
  haskell-miso-dev = dev;
  haskell-miso-runner = 
    runCommand "haskell-miso.org" { inherit client server; } ''
      mkdir -p $out/{bin,static}
      cp -v ${server}/bin/* $out/bin
      cp -v ${client}/bin/client.jsexe/all.js $out/static/all.js
    '';
}
