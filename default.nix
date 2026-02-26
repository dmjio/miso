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
  miso-tests-ghc = pkgs.haskell.packages.ghc9122.miso-tests;

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
    cd tests
    bun run ../ts/playwright.ts
    exit_code=$?
    pkill http-server
    exit "$exit_code"
  '';

  playwright-js = pkgs.writeScriptBin "playwright" ''
    #!${pkgs.stdenv.shell}
    set -e
    export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
    export PATH="${pkgs.lib.makeBinPath [ pkgs.http-server pkgs.bun ]}:$PATH"
    bun install playwright@1.53
    http-server -p 8061 ${miso-tests}/bin/component-tests.jsexe &
    HTTP_SERVER_PID=$!
    echo $HTTP_SERVER_PID
    PW_API_PORT=8060
    PORT=$PW_API_PORT bun run ./ts/playwright.ts &
    PLAYWRIGHT_SERVER_PID=$!
    echo $PLAYWRIGHT_SERVER_PID
    until curl -sf "http://localhost:$PW_API_PORT/ready"; do sleep 0.1; done
    echo "Playwright server ready, asking it to start test."
    curl --fail "http://localhost:$PW_API_PORT/test?port=8061&wait=true"
    exit_code=$?
    PORT=8062 \
      PLAYWRIGHT_PORT=$PW_API_PORT \
      STATIC_DIR="${miso-tests}/bin/integration-tests-client.jsexe/" \
      BACKEND=GHCJS \
      ${miso-tests-ghc}/bin/integration-tests-server
    exit_code=$?
    kill $HTTP_SERVER_PID
    kill $PLAYWRIGHT_SERVER_PID
    exit "$exit_code"
  '';

  playwright-wasm = pkgs.writeScriptBin "playwright" ''
    #!${pkgs.stdenv.shell}
    set -e
    export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
    export PATH="${pkgs.lib.makeBinPath [ pkgs.http-server pkgs.bun ]}:$PATH"
    bun install playwright@1.53
    pushd tests
    nix develop .#wasm --command bash -c 'make'
    http-server -p 8061 ./public &
    HTTP_SERVER_PID=$!
    echo $HTTP_SERVER_PID
    PW_API_PORT=8060
    echo pwd:
    pwd
    PORT=$PW_API_PORT bun run ../ts/playwright.ts &
    PLAYWRIGHT_SERVER_PID=$!
    echo $PLAYWRIGHT_SERVER_PID
    until curl -sf "http://localhost:$PW_API_PORT/ready"; do sleep 0.1; done
    echo "Playwright server ready, asking it to start test."
    curl --fail "http://localhost:$PW_API_PORT/test?port=8061&wait=true"
    PORT=8062 \
      PLAYWRIGHT_PORT=$PW_API_PORT \
      STATIC_DIR=./public \
      BACKEND=WASM \
      ${miso-tests-ghc}/bin/integration-tests-server
    exit_code=$?
    kill $HTTP_SERVER_PID
    kill $PLAYWRIGHT_SERVER_PID
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
