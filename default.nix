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

  # hackage release examples
  release-examples =
    with pkgs.haskell.packages.ghc9122;
    sdistTarball (buildStrictly miso-examples);

  # ghcjs9122
  miso-ghcjs-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso;
  miso-examples-ghcjs-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso-examples;
  sample-app-js-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.sample-app-js;

  # ghcjs86
  miso-ghcjs = legacyPkgs.haskell.packages.ghcjs.miso;
  miso-ghcjs-prod = legacyPkgs.haskell.packages.ghcjs86.miso-prod;
  inherit (legacyPkgs.haskell.packages.ghcjs) miso-examples sample-app-js;

  # miso x86
  miso-ghc = legacyPkgs.haskell.packages.ghc865.miso;
  miso-ghc-9122 = pkgs.haskell.packages.ghc9122.miso;

  # miso-examples x86
  miso-examples-ghc = legacyPkgs.haskell.packages.ghc865.miso-examples;
  miso-examples-ghc-9122 = pkgs.haskell.packages.ghc9122.miso-examples;

  # sample app legacy build
  inherit (legacyPkgs.haskell.packages.ghc865)
    sample-app;

  # sample app
  sample-app-ghc9122 =
    pkgs.haskell.packages.ghc9122.sample-app;

  # Miso wasm examples
  inherit (pkgs)
    wasmExamples
    svgWasm
    componentsWasm
    canvas2DWasm
    todoWasm;

  # wasm utils
  inherit (pkgs)
    wasm-ghc
    ghc-wasm-meta
    hello-world-web-wasm;

  # sse
  inherit (import ./examples/sse {})
    sse-runner
    sse-client
    sse-server;

  # website
  inherit (import ./haskell-miso.org {})
    haskell-miso-dev
    haskell-miso-client
    haskell-miso-server
    haskell-miso-runner;

  # code coverage
  inherit (pkgs)
    coverage;

  # haddocks
  inherit (pkgs)
    haddocks;

  # ci
  inherit (legacyPkgs)
    deploy
    nixops
    haskell-miso-org-test;

  # ghciwatch
  inherit (pkgs)
    ghciwatch;

  # utils
  inherit (pkgs.haskell.packages.ghc9122)
    miso-from-html;

  # misc. examples
  inherit (legacyPkgs)
    more-examples;

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
}
