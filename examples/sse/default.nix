{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghc802;
  ghcjs = pkgs.haskell.packages.ghcjs.override (oldAttrs: {
    overrides = self: super: {
      jsaddle-warp = super.callPackage ./../../jsaddle-warp-ghcjs.nix {};
    };
  });
  miso-ghc = ghc802.callPackage ./../../miso-ghc.nix { };
  miso-ghcjs = ghcjs.callPackage ./../../miso-ghcjs.nix { };
  server = ghc802.callPackage ./server.nix { miso = miso-ghc; };
  client = ghcjs.callPackage ./client.nix { miso = miso-ghcjs; };
in
  runCommand "sse.haskell-miso.org" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    ${closurecompiler}/bin/closure-compiler ${client}/bin/client.jsexe/all.js > $out/static/all.js
    cp ${server}/bin/* $out/bin
  ''
