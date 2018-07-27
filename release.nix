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
     miso-ghcjs8 = (ghcjsHEAD.override(oldAttrs: {
       overrides = self: super: {
             jsaddle-warp =
               pkgs.haskell.lib.dontCheck
               ((super.callCabal2nix "jsaddle-warp" (pkgs.fetchFromGitHub {
                 owner = "ghcjs";
                 repo = "jsaddle";
                 rev = "76d969d62c0c125bf58927224cac0448b429cd38";
                 sha256 = "1fcw40w1x07daxwh4sbnf542v03p4858v8wbinsjw6vdabnm7aad";
               } + "/jsaddle-warp") {}).overrideAttrs(oldAttrs: {
                 patchPhase = ''
                   substituteInPlace jsaddle-warp.cabal --replace "aeson >=0.8.0.2 && <1.3" "aeson >=0.8.0.2 && <1.4";
                 '';
                 }));
       };
       })).callPackage ./miso-ghcjs.nix { };
     haskell-miso.org = import ./examples/haskell-miso.org { inherit pkgs; };
     sse-example = import ./examples/sse { inherit pkgs; };
   }
