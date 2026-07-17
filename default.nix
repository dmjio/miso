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

  # js tooling
  inherit (pkgs) rspeedy;

  # ghcjs9122
  miso-ghcjs-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso;
  miso-native-ghcjs-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.miso-native;
  sample-app-js-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.sample-app-js;
  sample-app-native-9122 = pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.sample-app-native;

  # Lynx bundle: compiles sample-app-native through rspeedy into a .lynx.bundle
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

        # Minify the GHC JS output (same as `bun build --minify-whitespace out.js` in reload)
        ${pkgs.bun}/bin/bun build \
          --minify-whitespace \
          --target=bun \
          --outfile=build/all.js \
          ${hsPkg}/bin/app-native.jsexe/all.js

        # Wire up rspeedy's node_modules so config imports resolve
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
  miso-tests = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso-tests;

  # ghcjs86
  miso-ghcjs = legacyPkgs.haskell.packages.ghcjs.miso;
  miso-ghcjs-prod = legacyPkgs.haskell.packages.ghcjs86.miso-prod;
  inherit (legacyPkgs.haskell.packages.ghcjs) sample-app-js;

  # miso x86
  miso-ghc = legacyPkgs.haskell.packages.ghc865.miso;
  miso-ghc-9122 = pkgs.haskell.packages.ghc9122.miso;
  miso-native-ghc-9122 = pkgs.haskell.packages.ghc9122.miso-native;
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

  playwright-ghcjs = pkgs.writeScriptBin "playwright" ''
    #!${pkgs.stdenv.shell}
    export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
    export PATH="${pkgs.lib.makeBinPath [ pkgs.http-server pkgs.bun ]}:$PATH"
    bun install playwright@1.53
    http-server ${legacyPkgs.haskell.packages.ghcjs.miso-tests}/bin/component-tests.jsexe &
    bun run ts/echo-server.ts &
    cd tests
    bun run ../ts/playwright.ts
    exit_code=$?
    pkill http-server
    pkill -f echo-server
    exit "$exit_code"
  '';

  playwright-js = pkgs.writeScriptBin "playwright" ''
    #!${pkgs.stdenv.shell}
    export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
    export PATH="${pkgs.lib.makeBinPath [ pkgs.http-server pkgs.bun ]}:$PATH"
    bun install playwright@1.53
    http-server ${pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso-tests}/bin/component-tests.jsexe &
    bun run ts/echo-server.ts &
    cd tests
    bun run ../ts/playwright.ts
    exit_code=$?
    pkill http-server
    pkill -f echo-server
    exit "$exit_code"
  '';

  playwright-wasm = pkgs.writeScriptBin "playwright" ''
    #!${pkgs.stdenv.shell}
    export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
    export PATH="${pkgs.lib.makeBinPath [ pkgs.http-server pkgs.bun ]}:$PATH"
    bun install playwright@1.53
    cd tests
    nix develop .#wasm --command bash -c 'make'
    http-server ./public &
    bun run ../ts/echo-server.ts &
    bun run ../ts/playwright.ts
    exit_code=$?
    pkill http-server
    pkill -f echo-server
    exit "$exit_code"
  '';

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
