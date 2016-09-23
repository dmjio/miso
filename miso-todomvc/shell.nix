{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
  (import ./default.nix { inherit nixpkgs compiler; }).env
