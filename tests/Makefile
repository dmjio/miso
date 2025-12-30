.PHONY= update build optim

all: update build optim

update:
	cabal update

build:
	cabal build component-tests
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell cabal list-bin component-tests | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/component-tests.wasm -o public/component-tests.wasm
	wasm-tools strip -o public/component-tests.wasm public/component-tests.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public
