{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs ghc802;
  miso-ghc = ghc802.callPackage ./../../miso-ghc.nix { };
  miso-ghcjs = ghcjs.callPackage ./../../miso-ghcjs.nix { };
  server = ghc802.callPackage ./server.nix { miso = miso-ghc; };
  client = ghcjs.callPackage ./client.nix { miso = miso-ghcjs; };
in
  runCommand "sse.haskell-miso.org" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    ${closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js
    cp ${server}/bin/* $out/bin
  ''
