{ pkgs ? import <nixpkgs> {} }:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "0cv9hr35a3w02fzc79mvhinazdmxv9fp5ir4k4brhdarbnjxv98n";
    rev = "91b7194c6e0baab3d367073449a8ada7ba8dc346";
  }) { tests = false; };
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
