{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;
in { miso-release = import ./default.nix { nixpkgs = pkgs; };
     miso-ghcjs8 = ghcjsHEAD.callPackage ./miso-ghcjs.nix {};
   }
