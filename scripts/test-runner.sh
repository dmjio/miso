#!/usr/bin/env bash

bun run build-test

cabal update \
      --with-compiler=javascript-unknown-ghcjs-ghc \
      --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

cabal configure -ftests
cabal build miso-tests \
      --with-compiler=javascript-unknown-ghcjs-ghc \
      --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

bun run ./dist-newstyle/*/*/ghc-9.12.2/miso-1.9.0.0/x/*/build/*/*.jsexe/all.js
