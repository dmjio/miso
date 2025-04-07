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
  miso-examples-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso-examples;
  sample-app-js-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.sample-app-js;

  # ghcjs86
  miso-ghcjs = legacyPkgs.haskell.packages.ghcjs.miso;
  inherit (legacyPkgs.haskell.packages.ghcjs) miso-examples sample-app-js;
  
  # x86
  miso-ghc = legacyPkgs.haskell.packages.ghc865.miso;
  miso-ghc-9122 = pkgs.haskell.packages.ghc9122.miso;

  # x86
  miso-examples-ghc = legacyPkgs.haskell.packages.ghc865.miso-examples;
  miso-examples-ghc-9122 = pkgs.haskell.packages.ghc9122.miso-examples;

  inherit (legacyPkgs.haskell.packages.ghc865)
    sample-app;

  inherit (pkgs.haskell.packages.ghc9122)
    sample-app-ghc9122;

  # Miso wasm examples
  inherit (pkgs)
    wasmExamples
    svgWasm
    componentsWasm
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

  # bun
  inherit (pkgs)
    bun;
}
