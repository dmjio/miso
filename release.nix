{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;
in { miso-release = import ./default.nix { nixpkgs = pkgs; };
     miso-ghcjs8 = ghcjsHEAD.callPackage ./miso-ghcjs.nix {};
     haskell-miso.org = import ./examples/haskell-miso.org {};
     sse-example = import ./examples/sse {};
   }
