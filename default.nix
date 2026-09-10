{ overlays ? []
}:
with (import ./nix { inherit overlays; });

with pkgs.haskell.lib;
rec {
  inherit pkgs legacyPkgs;

  # hackage release
  release =
    with pkgs.haskell.packages.ghc9141;
    sdistTarball (buildStrictly miso);

  # js tooling
  inherit (pkgs) rspeedy;

  # ghcjs9141
  miso-ghcjs-9141 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9141.miso;
  miso-native-ghcjs-9141 = pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.miso-native;
  sample-app-js-9141 = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9141.sample-app-js;
  sample-app-native-9141 = pkgs.pkgsCross.ghcjs.haskell.packages.ghcNative.sample-app-native;

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

  miso-tests = pkgs.pkgsCross.ghcjs.haskell.packages.ghc9141.miso-tests;

  # wasm32-wasi, via a real callCabal2nix-capable Haskell package set --
  # see nix/wasm/package-set.nix. Distinct from playwright-wasm below, which
  # still uses `nix develop .#wasm --command make` for the browser-side
  # integration tests.
  miso-wasm-ghc9141 = pkgs.wasmPkgs.haskell.packages.ghc9141.miso;
  sample-app-wasm-ghc9141 = pkgs.wasmPkgs.haskell.packages.ghc9141.sample-app;
  miso-tests-wasm-ghc9141 = pkgs.wasmPkgs.haskell.packages.ghc9141.miso-tests;
  miso-wasm-aeson-ghc9141 = pkgs.wasmPkgs.haskell.packages.ghc9141.miso-aeson;
  miso-wasm-aeson-text-ghc9141 = pkgs.wasmPkgs.haskell.packages.ghc9141.miso-aeson-text;
  miso-tests-wasm-aeson-ghc9141 = pkgs.wasmPkgs.haskell.packages.ghc9141.miso-tests-aeson;
  miso-tests-wasm-aeson-text-ghc9141 = pkgs.wasmPkgs.haskell.packages.ghc9141.miso-tests-aeson-text;

  # Browser-loadable bundles (the wasm32-wasi analogue of a .jsexe) --
  # see nix/wasm/mk-wasm-bundle.nix.
  sample-app-wasm-bundle-ghc9141 = pkgs.wasmWebBundle {
    name = "sample-app-wasm-bundle";
    drv = sample-app-wasm-ghc9141;
    exeName = "app";
  };
  miso-tests-wasm-bundle-ghc9141 = pkgs.wasmWebBundle {
    name = "miso-tests-wasm-bundle";
    drv = miso-tests-wasm-ghc9141;
    exeName = "component-tests";
  };
  miso-tests-aeson-wasm-bundle-ghc9141 = pkgs.wasmWebBundle {
    name = "miso-tests-aeson-wasm-bundle";
    drv = miso-tests-wasm-aeson-ghc9141;
    exeName = "component-tests";
  };
  miso-tests-aeson-text-wasm-bundle-ghc9141 = pkgs.wasmWebBundle {
    name = "miso-tests-aeson-text-wasm-bundle";
    drv = miso-tests-wasm-aeson-text-ghc9141;
    exeName = "component-tests";
  };

  # ghcjs86
  miso-ghcjs = legacyPkgs.haskell.packages.ghcjs.miso;
  miso-ghcjs-prod = legacyPkgs.haskell.packages.ghcjs86.miso-prod;
  inherit (legacyPkgs.haskell.packages.ghcjs) sample-app-js;

  # miso x86
  miso-ghc = legacyPkgs.haskell.packages.ghc865.miso;
  miso-ghc-9141 = pkgs.haskell.packages.ghc9141.miso;
  miso-native-ghc-9141 = pkgs.haskell.packages.ghc9141.miso-native;
  miso-tests-ghc = pkgs.haskell.packages.ghc9141.miso;

  # sample app legacy build
  inherit (legacyPkgs.haskell.packages.ghc865)
    sample-app;

  # sample app
  sample-app-ghc9141 =
    pkgs.haskell.packages.ghc9141.sample-app;

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
  inherit (pkgs.haskell.packages.ghc9141)
    miso-from-html;

  # hls
  inherit (pkgs.haskell.packages.ghc9141)
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
    bun install playwright@${pkgs.playwright-driver.version}
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
    bun install playwright@${pkgs.playwright-driver.version}
    http-server ${pkgs.pkgsCross.ghcjs.haskell.packages.ghc9141.miso-tests}/bin/component-tests.jsexe &
    bun run ts/echo-server.ts &
    cd tests
    bun run ../ts/playwright.ts
    exit_code=$?
    pkill http-server
    pkill -f echo-server
    exit "$exit_code"
  '';

  # Nix-native (via wasmPkgs -- see nix/wasm/package-set.nix), no more
  # `nix develop .#wasm --command make`.
  playwright-wasm = pkgs.writeScriptBin "playwright" ''
    #!${pkgs.stdenv.shell}
    export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
    export PATH="${pkgs.lib.makeBinPath [ pkgs.http-server pkgs.bun ]}:$PATH"
    bun install playwright@${pkgs.playwright-driver.version}
    http-server ${miso-tests-wasm-bundle-ghc9141}/component-tests.wasmexe &
    bun run ts/echo-server.ts &
    cd tests
    bun run ../ts/playwright.ts
    exit_code=$?
    pkill http-server
    pkill -f echo-server
    exit "$exit_code"
  '';

  # Same as playwright-wasm, but miso is built with the 'aeson' cabal flag
  # (Miso.JSON defined in terms of Data.Aeson). Nix-native now (see
  # miso-tests-aeson-wasm-bundle-ghc9141 above) -- unblocked by the nixpkgs
  # bump to a revision whose hashable (1.5.1.0) no longer depends on
  # ghc-bignum at all, so the old ghc-bignum-1.4 version conflict is moot.
  playwright-wasm-aeson = pkgs.writeScriptBin "playwright" ''
    #!${pkgs.stdenv.shell}
    export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
    export PATH="${pkgs.lib.makeBinPath [ pkgs.http-server pkgs.bun ]}:$PATH"
    bun install playwright@${pkgs.playwright-driver.version}
    http-server ${miso-tests-aeson-wasm-bundle-ghc9141}/component-tests.wasmexe &
    bun run ts/echo-server.ts &
    cd tests
    bun run ../ts/playwright.ts
    exit_code=$?
    pkill http-server
    pkill -f echo-server
    exit "$exit_code"
  '';

  # Same as playwright-wasm, but miso is built with the 'aeson' and 'text'
  # cabal flags (Miso.JSON defined in terms of Data.Aeson, Miso.String
  # backed by Data.Text). Nix-native now -- see playwright-wasm-aeson.
  playwright-wasm-aeson-text = pkgs.writeScriptBin "playwright" ''
    #!${pkgs.stdenv.shell}
    export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
    export PATH="${pkgs.lib.makeBinPath [ pkgs.http-server pkgs.bun ]}:$PATH"
    bun install playwright@${pkgs.playwright-driver.version}
    http-server ${miso-tests-aeson-text-wasm-bundle-ghc9141}/component-tests.wasmexe &
    bun run ts/echo-server.ts &
    cd tests
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
