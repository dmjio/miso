{ pkgs ? import <nixpkgs> {} }:
  { miso-release = import ./default.nix { nixpkgs = pkgs; }; }
