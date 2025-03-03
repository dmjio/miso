with (builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json));
{ haddock ? true, tests ? false, overlays ? [] }:
let
  pkgs = import ./nix {
    inherit haddock tests overlays;
  };
in with pkgs.haskell.lib;
{
  inherit pkgs;

  # hackage miso release
  release =
    sdistTarball (buildStrictly pkgs.miso-ghc);

  # hackage miso release examples
  release-examples =
    sdistTarball (buildStrictly pkgs.miso-examples);

  #js
  inherit (pkgs)
    miso-ghcjs
    miso-examples-ghcjs;
  
  #native
  inherit (pkgs)
    miso-ghc
    miso-examples-ghc;

  inherit (pkgs.haskell.packages.ghc865) sample-app-jsaddle;

  # |=========================
  # |   Miso wasm examples
  # |=========================
  # | nix-build -A svgWasm
  # |  && ./result/bin/build.sh
  # |  && nix-build -A svgWasm
  # |  && http-server ./result/svg.wasmexe
  # | =========================
  inherit (pkgs)
    wasmExamples
    svgWasm
    simpleWasm
    componentsWasm
    todoWasm;

  #wasm utils
  inherit (pkgs)
    wasm-ghc
    ghc-wasm-meta
    hello-world-web-wasm;

  # tagged releases
  inherit (pkgs)
    sample-app-tagged
    sample-app-jsaddle-tagged-release
    sample-app-jsaddle-tagged-dev;

  #website
  inherit (pkgs)
    haskell-miso-client
    haskell-miso-server
    haskell-miso-runner;

  # testsing
  inherit (pkgs)
    testsWasm;

  #code covergae
  inherit (pkgs) coverage;

  #ci
  deploy = pkgs.deploy rev;
  inherit (pkgs) haskell-miso-org-test;

  # utils
  inherit (pkgs.haskell.packages.ghc865) miso-from-html;
}
