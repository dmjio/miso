{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs ghc802;
  miso-ghc = ghc802.callPackage ./../../miso-ghc.nix { };
  miso-ghcjs = ghcjs.callPackage ./../../miso-ghcjs.nix { };
  client = ghcjs.callPackage ./client { miso = miso-ghcjs; };
  server = ghc802.callPackage ./server { miso = miso-ghc; };
in
  runCommand "haskell-miso.org" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${client}/bin/client.jsexe/all.js $out/static/all.js
    cp ${server}/bin/* $out/bin
  ''


