#!/usr/bin/env bash
bun run build-test

cabal update \
  --with-compiler=javascript-unknown-ghcjs-ghc \
  --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

cabal build component-tests \
  --with-compiler=javascript-unknown-ghcjs-ghc \
  --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

bun run \
    ../dist-newstyle/*/javascript-ghcjs/ghc-*/*/x/*/build/*/component-tests.jsexe/all.js
