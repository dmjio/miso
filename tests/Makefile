.PHONY= update build optim

all: update build optim

update:
	wasm32-wasi-cabal update

build:
	wasm32-wasi-cabal build --allow-newer
	rm -rf public
	cp -r static public
	bash -c '\
		libdir=$$(wasm32-wasi-ghc --print-libdir); \
		my_wasm=$$(wasm32-wasi-cabal list-bin component-tests --allow-newer); \
		$$libdir/post-link.mjs -i "$$my_wasm" -o public/ghc_wasm_jsffi.js; \
		cp -v "$$my_wasm" public/; \
		more_wasm=$$(wasm32-wasi-cabal list-bin integration-tests-client --allow-newer); \
		echo "Processing integration-tests-client: $$more_wasm"; \
		$$libdir/post-link.mjs -i "$$more_wasm" -o public/wasm.js; \
		cp -v "$$more_wasm" public/integration-client.wasm \
	'

optim:
	wasm-opt -all -O2 public/component-tests.wasm -o public/component-tests.wasm
	wasm-tools strip -o public/component-tests.wasm public/component-tests.wasm

serve:
	http-server public

clean:
	rm -rf dist-newstyle public
