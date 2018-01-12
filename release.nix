{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  inherit (pkgs.haskell.packages) ghcjsHEAD;
in { miso-release = import ./default.nix { inherit pkgs; };
     miso-ghcjs8 = ghcjsHEAD.callPackage ./miso-ghcjs.nix { };
     haskell-miso.org = import ./examples/haskell-miso.org { inherit pkgs; };
     sse-example = import ./examples/sse { inherit pkgs; };
   }
