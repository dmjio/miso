{}:
let
  inherit (import ../../default.nix {}) pkgs;
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs86 ghc865;
  miso-ghc = ghc865.callPackage ./../../miso-ghc.nix { };
  miso-ghcjs = ghcjs86.callPackage ./../../miso-ghcjs.nix { };
  client = ghcjs86.callPackage ./client { miso = miso-ghcjs; };
  server = ghc865.callPackage ./server { miso = miso-ghc; };
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
