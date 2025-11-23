{ overlays ? []
}:
with (import ./nix { inherit overlays; });

with pkgs.haskell.lib;
{
  inherit pkgs legacyPkgs;

  # hackage release
  release =
    with pkgs.haskell.packages.ghc9122;
    sdistTarball (buildStrictly miso);

  # ghcjs9122
  miso-ghcjs-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso;
  sample-app-js-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.sample-app-js;
  miso-tests = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso-tests;

  # ghcjs86
  miso-ghcjs = legacyPkgs.haskell.packages.ghcjs.miso;
  miso-ghcjs-prod = legacyPkgs.haskell.packages.ghcjs86.miso-prod;
  inherit (legacyPkgs.haskell.packages.ghcjs) sample-app-js;

  # miso x86
  miso-ghc = legacyPkgs.haskell.packages.ghc865.miso;
  miso-ghc-9122 = pkgs.haskell.packages.ghc9122.miso;
  miso-tests-ghc = pkgs.haskell.packages.ghc9122.miso;

  # sample app legacy build
  inherit (legacyPkgs.haskell.packages.ghc865)
    sample-app;

  # sample app
  sample-app-ghc9122 =
    pkgs.haskell.packages.ghc9122.sample-app;

  # Miso wasm examples
  inherit (pkgs)
    sampleWasm;

  # wasm utils
  inherit (pkgs)
    wasm-ghc
    ghc-wasm-meta;

  # ghciwatch
  inherit (pkgs)
    ghciwatch;

  # utils
  inherit (pkgs.haskell.packages.ghc9122)
    miso-from-html;

  # hls
  inherit (pkgs.haskell.packages.ghc9122)
    haskell-language-server;

  # dmj: make a NixOS test to ensure examples can be hosted
  # dry-running this ensures we catch the failure before deploy
  inherit (legacyPkgs)
    nginx-nixos-test;

  # bun
  inherit (pkgs)
    bun;

  # nurl
  # $ nurl https://github.com/nix-community/nurl
  #
  # fetchFromGitHub {
  #   owner = "nix-community";
  #   repo = "nurl";
  #   rev = "3a3ba7f0d14d92e1266395d826c6e229797d0044";
  #   hash = "sha256-WAFqmlsShuQngk6LMFlgz7Oyc41TAQeTa/49phhRizY=";
  # }
  #
  inherit (pkgs)
    nurl;

  # favicon.ico and miso.png
  miso-logos = pkgs.stdenv.mkDerivation {
    name = "miso-logos";
    src = ./logo;
    buildCommand = ''
      mkdir -p $out
      cp -v $src/* $out/
    '';
  };

}
