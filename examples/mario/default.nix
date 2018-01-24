{ nixpkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  inherit (nixpkgs.haskell.packages) ghc802 ghcjs;
  miso = ghcjs.callPackage ./../../miso-ghcjs.nix { };
in
  ghcjs.callPackage ./mario.nix { inherit miso; }
