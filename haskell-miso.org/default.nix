{}:
with (import ../default.nix {});
let
  sources = import ../nix/source.nix pkgs;
  inherit (pkgs) runCommand closurecompiler;
  ghc = legacyPkgs.haskell.packages.ghc865;
  ghcjs = legacyPkgs.haskell.packages.ghcjs;
  client = ghcjs.callCabal2nix "haskell-miso" (sources.haskell-miso) {};
  server = ghc.callCabal2nix "haskell-miso" (sources.haskell-miso) {};
  dev = ghc.callCabal2nixWithOptions "haskell-miso" (src.haskell-miso-src) "-fdev" {};
in
{ haskell-miso-client = client;
  haskell-miso-server = server;
  haskell-miso-dev = dev;
  haskell-miso-runner = 
    runCommand "haskell-miso.org" { inherit client server; } ''
      mkdir -p $out/{bin,static}
      cp ${server}/bin/* $out/bin
      ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
        --jscomp_off=checkVars \
        --externs=${client}/bin/client.jsexe/all.js.externs \
        ${client}/bin/client.jsexe/all.js > temp.js
      mv temp.js $out/static/all.js
    '';
}
