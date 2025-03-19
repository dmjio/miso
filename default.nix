with (builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json));
{ haddock ? true, tests ? false, overlays ? [] }:
let
  pkgs = import ./nix {
    inherit haddock tests overlays;
  };
in with pkgs.haskell.lib;
{
  inherit pkgs;

  # hacakge release
  release =
    with pkgs.haskell.packages.ghc865;
    sdistTarball (buildStrictly miso);

  release-examples =
    with pkgs.haskell.packages.ghcjs;
    sdistTarball (buildStrictly miso-examples);

  #js
  miso-ghcjs = pkgs.haskell.packages.ghcjs86.miso;
  inherit (pkgs.haskell.packages.ghcjs86) miso-examples sample-app-js;
  
  #native
  miso-ghc = pkgs.haskell.packages.ghc865.miso;
  miso-examples-ghc = pkgs.haskell.packages.ghc865.miso-examples;
  inherit (pkgs.haskell.packages.ghc865) sample-app;

  # miso wasm examples
  # nix-build -A wasmExamples && ./result/bin/build.sh && nix-build -A svgWasm && http-server ./result/svg.wasmexe
  inherit (pkgs)
    wasmExamples
    svgWasm
    componentsWasm
    todoWasm;

  #wasm utils
  inherit (pkgs)
    wasm-ghc
    ghc-wasm-meta
    hello-world-web-wasm;

  # sse
  inherit (pkgs.ssePkgs)
    sse-runner
    sse-client
    sse-server;

  #website
  inherit (pkgs)
    haskell-miso-client
    haskell-miso-server
    haskell-miso-runner;

  #code covergae
  inherit (pkgs) coverage;

  #ci
  deploy = pkgs.deploy rev;
  inherit (pkgs) haskell-miso-org-test;

  # ghciwatch
  inherit (pkgs) ghciwatch;

  # utils
  inherit (pkgs.haskell.packages.ghc865) miso-from-html;

  # misc. examples
  inherit (pkgs) more-examples;
}
