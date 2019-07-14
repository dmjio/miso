{ pkgs ? import <nixpkgs> {} }:

let

  pinnedPkgs = import (pkgs.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "4dd5c93998da55002fdec1c715c680531420381c";
    sha256 = "06paxakic36nbdnwkkb1094fzp3lpzxxb1r57gmb3py6pb6xrcnh";
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
