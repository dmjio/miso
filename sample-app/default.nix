{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "0bgw0bbydkh72kf303mcccl2xbjrydkp0mxhsrd3hvkgpacp4bh5";
    rev = "64b0790c035567af1ae7cb34f29d1c99f7ab69f6";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
