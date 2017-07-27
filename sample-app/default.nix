{ pkgs ? import <nixpkgs> {} }:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "13ckz11gbfs047hl3phj7h6fm59wsg9zw2fiqjaqkxmxv17zj5yj";
    rev = "0834d5c0b309de24d836cbdcc25fd257de10be17";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
