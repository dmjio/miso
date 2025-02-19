{}:
with (import ../.. {});
let
  src = import ../../nix/haskell/packages/source.nix pkgs;
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs86 ghc865;
  client = ghcjs86.callCabal2nix "haskell-miso" (src.haskell-miso-src) {};
  server = ghc865.callCabal2nix "haskell-miso" (src.haskell-miso-src) {};
in
  runCommand "haskell-miso.org" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/client.jsexe/all.js.externs \
      ${client}/bin/client.jsexe/all.js > temp.js
    mv temp.js $out/static/all.js
  ''
