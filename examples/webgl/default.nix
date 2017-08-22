{ pkgs ? import <nixpkgs> {} }:
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs;
  ghcjs-three = ghcjs.callCabal2nix "three.js" (pkgs.fetchFromGitHub {
    rev = "4f502ea5bef584d354cb99ac43d18ff8fa4b1580";
    repo = "ghcjs-three";
    owner = "dmjio";
    sha256 = "0g73f1scz2i5fx39whvc4ikq0z8irb0hncda0i53i0k93pwy8w7z";
  }) {};
  miso-ghcjs = ghcjs.callPackage ./../../miso-ghcjs.nix { };
  webgl = ghcjs.callPackage ./webgl.nix { miso = miso-ghcjs; inherit ghcjs-three; };
in
  webgl
