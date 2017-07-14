{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD ghcjs;
  ghcjs7103-stack = ghcjs.override {
    initialPackages = import ./stack/ghcjs7103/stackage-packages.nix;
  };
  ghcjs801-stack = ghcjsHEAD.override {
    initialPackages = import ./stack/ghcjs801/stackage-packages.nix;
  };
in { miso-release = import ./default.nix { nixpkgs = pkgs; };
     miso-ghcjs8 = ghcjsHEAD.callPackage ./miso-ghcjs.nix {};
     miso-stack-ghcjs7103 = ghcjs7103-stack.callPackage ./miso-ghcjs.nix {};
     miso-stack-ghcjs801 = ghcjs801-stack.callPackage ./miso-ghcjs.nix {};
   }
