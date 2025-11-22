#!/usr/bin/env bash
bun run build-test

cabal update \
  --with-compiler=javascript-unknown-ghcjs-ghc \
  --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

cabal build miso-tests \
  --with-compiler=javascript-unknown-ghcjs-ghc \
  --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

bun run \
    ./dist-newstyle/*/javascript-ghcjs/ghc-*/miso-tests-*/x/miso-tests/build/miso-tests/miso-tests.jsexe/all.js
