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

          # miso with -fnative (LynxJS dual-thread arch)
          miso-native-ghcjs-9122 =
            pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.miso-native;

          # GHC
          miso-ghc-9122 =
            pkgs.haskell.packages.ghc9122.miso;

          # Sample app (native / LynxJS)
          sample-app-native-ghcjs-9122 =
            pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.sample-app-native;

          # rspeedy (LynxJS bundle builder, wraps rspack)
          inherit (pkgs) rspeedy;

          # Lynx bundle for sample-app-native
          sample-app-native-bundle =
            let
              hsPkg = pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.sample-app-native;
              lynxConfig = pkgs.writeText "lynx.config.ts" ''
                import { defineConfig } from '@lynx-js/rspeedy';
                import { pluginReactLynx } from '@lynx-js/react-rsbuild-plugin';
                export default defineConfig({
                  source: { entry: './all.js' },
                  plugins: [ pluginReactLynx() ],
                });
              '';
            in
            pkgs.stdenv.mkDerivation {
              name = "sample-app-native-bundle";
              phases = [ "buildPhase" "installPhase" ];
              nativeBuildInputs = [ pkgs.bun pkgs.rspeedy pkgs.nodejs ];
              buildPhase = ''
                export HOME=$TMPDIR
                mkdir -p build
                ${pkgs.bun}/bin/bun build \
                  --minify-whitespace \
                  --target=bun \
                  --outfile=build/all.js \
                  ${hsPkg}/bin/app-native.jsexe/all.js
                ln -s ${pkgs.rspeedy}/lib/node_modules build/node_modules
                cp ${lynxConfig} build/lynx.config.ts
                cd build
                ${pkgs.rspeedy}/bin/rspeedy build
              '';
              installPhase = ''
                mkdir -p $out
                cp -r dist/. $out/
              '';
            };

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
          wasm-latest =
            pkgs.mkShell {
              name = "The miso ${system} GHC HEAD WASM shell";
              packages = with pkgs; [
                 inputs.ghc-wasm-meta.packages.${system}.all_gmp
                 gnumake bun
                 http-server
                 cabal-install
                 tailwindcss_4
                 ghciwatch
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
                function repl-watch () {
                   ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch . --debounce 50ms --command 'wasm32-wasi-cabal repl app -finteractive --repl-options="-fghci$
                }
              '';
            };

          # WASM shell
          wasm =
            pkgs.mkShell {
              name = "The miso ${system} GHC WASM 9.14 shell";
              packages = with pkgs; [
                 inputs.ghc-wasm-meta.packages.${system}.all_9_14
                 gnumake bun
                 http-server
                 cabal-install
                 tailwindcss_4
                 ghciwatch
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
                function repl-watch () {
                   ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch . --debounce 50ms --command 'wasm32-wasi-cabal repl app -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"'
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
                 rspeedy
                 bun
                 watchexec
                 just
              ];
            };
          };
      });
}
