{ overlays ? []
}:
with (import ./nix { inherit overlays; });

with pkgs.haskell.lib;
rec {
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
  # (via the shared mkLynxBundle helper). The showcase uses remote image URLs,
  # so it only needs styles.css compiled in.
  sample-app-native-bundle =
    pkgs.mkLynxBundle {
      name = "sample-app-native-bundle";
      jsDrv = pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.sample-app-native;
      exeName = "app-native";
      styles = ./sample-app-native/styles.css;
    };

  # Core-elements-only dual-thread conformance fixture. It is a separate
  # executable so gallery feature coverage remains intact.
  sample-app-native-conformance-bundle =
    pkgs.mkLynxBundle {
      name = "sample-app-native-conformance-bundle";
      jsDrv = pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.sample-app-native;
      exeName = "app-native-conformance";
      styles = ./sample-app-native/conformance.css;
    };

  # Android host APK (Kotlin + Lynx SDK), built reproducibly via nixpkgs
  # androidenv + the gradle mitm-cache dep fetcher. Embeds the freshly built
  # sample-app-native-bundle in assets. See nix/android.nix.
  sample-app-native-android =
    import ./nix/android.nix {
      inherit pkgs;
      bundle = sample-app-native-bundle;
    };

  # One-command emulator smoke test: boots an x86_64 AVD (uses KVM), installs the
  # APK, and launches the gallery. Run: ./result/bin/run-test-emulator
  sample-app-native-android-emulator =
    pkgs.androidenv.emulateApp {
      name = "run-miso-android";
      platformVersion = "34";
      abiVersion = "x86_64";
      systemImageType = "google_apis";
      app = sample-app-native-android;
      package = "io.dmj.miso";
      activity = ".MainActivity";
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
