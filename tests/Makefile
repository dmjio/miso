.PHONY= update build build-aeson optim aeson

all: update build optim

# Same as 'all', but with miso built against aeson (the 'aeson' cabal flag)
aeson: update build-aeson optim

update:
	wasm32-wasi-cabal update

build-aeson: CABAL_OPTS = --constraint="miso +aeson"
build-aeson: build

build:
	wasm32-wasi-cabal build component-tests $(CABAL_OPTS)
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin component-tests $(CABAL_OPTS) | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/

optim:
	wasm-opt -all -O2 public/component-tests.wasm -o public/component-tests.wasm
	wasm-tools strip -o public/component-tests.wasm public/component-tests.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public
