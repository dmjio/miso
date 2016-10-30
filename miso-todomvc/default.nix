{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
    let
       pkgs = nixpkgs.pkgs.haskell.packages.${compiler};
     in
       pkgs.callPackage ./miso-todomvc.nix {
          miso = pkgs.callPackage ./../miso/miso.nix { inherit compiler nixpkgs; };
       }
