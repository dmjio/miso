# The 2D canvas benchmark: build for one backend, serve it, then run
# bench.mjs against http://localhost:8080 (see bench.mjs).
.PHONY: build optim serve build-js mhs serve-mhs clean

all: build optim

build:
	wasm32-wasi-cabal build canvas-bench
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin canvas-bench | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/app.wasm

# --disable-compact-imports: Chromium rejects compact imports ("Invalid import kind").
optim:
	wasm-opt -all --disable-compact-imports -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

serve:
	http-server public

build-js:
	cabal build canvas-bench --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
	rm -rf public
	mkdir -p public
	sed 's#<script src="index.js" type="module"></script>#<script src="index.js"></script>#' static/index.html > public/index.html
	cp -v $$(cabal list-bin canvas-bench --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg).jsexe/all.js public/index.js

# MicroHs for the browser, via emscripten (see sample-app/Makefile).
MHS_TARGET=browser
MHS_FLAGS=-XCPP -DMHS -i../src -i../ffi/mhs -i../ffi/jsstring -js ../js/miso.js

mhs:
	mkdir -p public-mhs
	cp static-mhs/index.html public-mhs/
	mhs $(MHS_FLAGS) -t$(MHS_TARGET) Main -opublic-mhs/app.js

serve-mhs:
	http-server public-mhs

clean:
	rm -rf public public-mhs
