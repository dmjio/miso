.PHONY= update build optim

all: update build optim

js: update-js build-js

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

serve:
	http-server public

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
