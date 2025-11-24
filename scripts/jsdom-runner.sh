#!/usr/bin/env bash

bun run build-test

cabal update \
      --with-compiler=javascript-unknown-ghcjs-ghc \
      --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

cabal configure -ftests
cabal build miso-tests \
      --with-compiler=javascript-unknown-ghcjs-ghc \
      --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

if test -d ./dist-newstyle; then
    bun run ./dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/miso-tests-0.1.0.0/x/component-tests/build/component-tests/component-tests.jsexe/all.js
else
    bun run ../dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/miso-tests-0.1.0.0/x/component-tests/build/component-tests/component-tests.jsexe/all.js
fi
