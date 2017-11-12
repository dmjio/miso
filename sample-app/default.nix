{ pkgs ? import <nixpkgs> {} }:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "0cv9hr35a3w02fzc79mvhinazdmxv9fp5ir4k4brhdarbnjxv98n";
    rev = "82ae55f2cd81c6120fdfbb0bbe801a44a37bd282";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
