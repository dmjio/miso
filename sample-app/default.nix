{ pkgs ? import <nixpkgs> {} }:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "18jhr1ihf0vwwvp134j24isvzq699x5iy7l9ihrah760zmcxi7d2";
    rev = "e3d3d874337a4a44adc4b6bdb8b18d907c6c1e34";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
