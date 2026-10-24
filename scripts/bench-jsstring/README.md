# Data.JSString benchmark and property check

`ffi/jsstring/Data/JSString.hs` is `MisoString` on the WASM and MicroHs
backends. `wasm/` builds one program, with GHC and with MicroHs, that

- checks every function against `Data.Text` on random strings (letters,
  every kind of space, emoji), and
- times every exported function in the current module against the one in
  miso master `8bc7848` (`wasm/OldJSString.hs`), plus two workloads from
  haskell-miso.org's full-text search.

GHC (WASM in Node; 1 KB and 10 KB inputs), from the repository root:

```sh
nix develop .#wasm --command bash scripts/bench-jsstring/wasm/build.sh dist-bench
npm install --no-save --prefix scripts/bench-jsstring/wasm @bjorn3/browser_wasi_shim@0.3.0
node scripts/bench-jsstring/wasm/run.mjs dist-bench/bench.wasm dist-bench/ghc_wasm_jsffi.js all > results.txt
node scripts/bench-jsstring/wasm/summarize.mjs results.txt report.md
```

MicroHs (WASM in headless Chromium; 300-character inputs, because the old
module's lazy accumulators overflow MicroHs's stack on a few KB):

```sh
nix develop .#mhs --command bash scripts/bench-jsstring/wasm/build-mhs.sh dist-bench-mhs
npm install --no-save --prefix scripts/bench-jsstring/wasm playwright@1.48.2
npx --prefix scripts/bench-jsstring/wasm playwright install --with-deps chromium
node scripts/bench-jsstring/wasm/run-browser.mjs dist-bench-mhs/app.js 300 300 > results.txt
BENCH_LABEL=mhs node scripts/bench-jsstring/wasm/summarize.mjs results.txt report.md
```

`run.mjs` takes `props` or `bench` instead of `all` to run one half. The
old module's `lines` over the 110 KB search index takes about two minutes on
GHC.

Known differences in the property check, none of them in `Data.JSString`:
GHC's and MicroHs's `toJSString` (re-exported from `GHC.Wasm.Prim`) drop a
leading U+FEFF; on MicroHs, `Data.Text.replicate 0` throws, and `read` /
`show` for `Double` and `Float` lose the last digit.
