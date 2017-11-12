{ pkgs ? import <nixpkgs> {} }:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "13wfxxplnqajqnykyxdy7477kdfxshpv800b2shr1iib8m01lygn";
    rev = "91b7194c6e0baab3d367073449a8ada7ba8dc346";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
