{
  ### Welcome to the Haskell Miso Flake ###
  description = "🍜 Haskell Miso flake 🍜";

  ## Config
  nixConfig = {

    # Miso's cachix cache
    extra-substituters = [
      "https://haskell-miso-cachix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
    ];

  };

  # Miso's flake inputs
  inputs = {

    # Miso's nixpkgs hash, this is used for acquiring the GHCJS-9122 backend
    # and native backend
    nixpkgs.url =
      "github:nixos/nixpkgs?rev=9e2e8a7878573d312db421d69e071690ec34e98c";

    # Some light utils
    flake-utils.url = "github:numtide/flake-utils";

    # Miso uses this compiling for WebAssembly
    ghc-wasm-meta.url =
      "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";

    # dmj: (leave commented for now, we might cache our own copy of ghc-wasm if we need to)
    #
    # ghc-wasm-meta.inputs.nixpkgs.follows =
    #   "nixpkgs";

  };

  outputs = { self, nixpkgs, flake-utils, ... } @ inputs:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          # Miso's overlays
          overlays = [ (import ./nix/overlay.nix) ];
        };
      in
      {
        # Miso's packages
        packages = rec {
          # Default package is vanilla GHC 9.12.2 miso
          default = miso-ghc-9122;

          # GHCJS miso
          miso-ghcjs-9122 =
            pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso;

          # GHC
          miso-ghc-9122 =
            pkgs.haskell.packages.ghc9122.miso;

          # Util
          inherit (pkgs.haskell.packages.ghc9122)
            miso-from-html;

        };

        # Miso's dev shells
        devShells = rec {

          # Default GHC shell
          default =
            pkgs.haskell.packages.ghc9122.miso.env.overrideAttrs (drv: {
              buildInputs = with pkgs;
                drv.buildInputs ++
                  [ just bun ormolu cabal-install ghcid tailwindcss_4
                  ];
            });

          # Shell for hls dev
          hls =
            pkgs.haskell.packages.ghc9122.miso.env.overrideAttrs (drv: {
              buildInputs = with pkgs; with haskell.packages.ghc9122;
                drv.buildInputs ++
                  [ just bun ormolu haskell-language-server cabal-install ghcid tailwindcss_4
                  ];
            });

          # Shell for JavaScript / TypeScript development
          typescript =
            pkgs.mkShell {
              name = "The miso ${system} JavaScript / TypeScript shell";
              packages = with pkgs; [ bun ];
            };

          # WASM shell
          wasm =
            pkgs.mkShell {
              name = "The miso ${system} GHC WASM 9.12.2 shell";
              packages = with pkgs; [
                 inputs.ghc-wasm-meta.packages.${system}.all_9_12
                 gnumake bun
                 http-server
                 cabal-install
                 tailwindcss_4
               ];
              shellHook = ''
                function build () {
                   wasm32-wasi-cabal build $1
                }
                function clean () {
                   wasm32-wasi-cabal clean
                }
                function update () {
                   wasm32-wasi-cabal update
                }
                function repl () {
                   wasm32-wasi-cabal repl $1 -finteractive \
                     --repl-options='-fghci-browser -fghci-browser-port=8080'
                }
              '';
            };

          # GHCJS9122 shell
          ghcjs =
            pkgs.mkShell {
              name = "The miso ${system} GHC JS 9.12.2 shell";
              shellHook = ''
                export CC=${pkgs.emscripten}/bin/emcc
                mkdir -p ~/.emscripten_cache
                chmod u+rwX -R ~/.emscripten_cache
                cp -r ${pkgs.emscripten}/share/emscripten/cache ~/.emscripten_cache
                export EM_CACHE=~/.emscripten_cache

                function build () {
                   cabal build $1 \
                     --with-compiler=javascript-unknown-ghcjs-ghc \
                     --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
                }
                function clean () {
                   cabal clean
                }
                function update () {
                   cabal update
                }
              '';
              packages = with pkgs; [
                 pkgsCross.ghcjs.haskell.packages.ghc9122.ghc
                 bun
                 gnumake
                 http-server
                 cabal-install
                 nodejs
                 emscripten
                 tailwindcss_4
              ];
            };

          # GHCJS shell for building iOS / Android apps targeting LynxJS.org
          native =
            pkgs.mkShell {
              name = "The miso-native ${system} GHC JS 9.12.2 shell";
              shellHook = ''
                function build () {
                   cabal build $1 \
                     --with-compiler=javascript-unknown-ghcjs-ghc \
                     --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
                }
                function clean () {
                   cabal clean
                }
                function update () {
                   cabal update
                }
              '';
              packages = with pkgs; [
                 pkgsCross.ghcjs.haskell.packages.ghcNative.ghc
                 gnumake
                 http-server
                 cabal-install
                 emscripten
                 tailwindcss_4
              ];
            };
          };
      });
}
