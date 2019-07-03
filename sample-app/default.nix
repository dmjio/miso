{ pkgs ? import <nixpkgs> {} }:

let

  pinnedPkgs = import (pkgs.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){};

  miso = pinnedPkgs.haskell.packages.ghcjs.callCabal2nix "miso" (pkgs.fetchFromGitHub {
    owner  = "dmjio";
    repo   = "miso";
    rev    = "bb2be3264ff3c6aa3b18e471d7cf04296024059b";
    sha256 = "07k1rlvl9g027fp2khl9kiwla4rcn9sv8v2dzm0rzf149aal93vn";
  }){};

in

  pinnedPkgs.haskell.packages.ghcjs.callPackage ./app.nix {
    inherit miso;
  }
