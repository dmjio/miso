.PHONY= update build optim

all: update build optim

js: update-js build-js

js86: configure-js86 update-js86 build-js86

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin app | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

watch:
	ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch *.hs --debounce 50ms --command 'wasm32-wasi-cabal repl app -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"'

serve:
	http-server public

repl: update
	wasm32-wasi-cabal repl app -finteractive --repl-options='-fghci-browser -fghci-browser-port=8080'

clean:
	rm -rf ../dist-newstyle public

update-js:
	cabal update --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

build-js:
	cabal build --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
	cp -v ../dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/*/x/app/build/app/app.jsexe/all.js .
	rm -rf public
	cp -rv static public
	bunx swc ./all.js -o public/index.js

configure-js86:
	cabal configure --ghcjs

update-js86:
	cabal update --ghcjs

build-js86:
	cabal build --ghcjs
	cp -v ../dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/app-0.1.0.0/x/app/build/app/app.jsexe/all.js .
	rm -rf public
	cp -rv static public

# Build with MicroHs (mhs) for the browser, via emscripten.
# MHS_TARGET is a section of MicroHs's mhs.conf: browser (WebAssembly)
# or browser_js (JavaScript only, -sWASM=0).
MHS_TARGET=browser
# -js embeds js/miso.js in the output (like js-sources in miso.cabal when built with mcabal)
MHS_FLAGS=-XCPP -DMHS -i../src -i../ffi/mhs -i../ffi/jsstring -js ../js/miso.js

mhs:
	mkdir -p public-mhs
	cp static-mhs/index.html public-mhs/
	mhs $(MHS_FLAGS) -t$(MHS_TARGET) Main -opublic-mhs/app.js

# The same via mcabal (needs 'mcabal install' of miso first): js-sources are embedded automatically.
mhs-mcabal:
	mcabal --options=-t$(MHS_TARGET) build
	mkdir -p public-mhs
	cp static-mhs/index.html public-mhs/
	cp dist-mcabal/bin/mhs/app public-mhs/app.js

serve-mhs:
	http-server public-mhs
