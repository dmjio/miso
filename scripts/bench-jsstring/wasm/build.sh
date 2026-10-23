#!/usr/bin/env bash
# Builds bench.wasm and ghc_wasm_jsffi.js into $1 (default: dist-bench).
# Run inside `nix develop .#wasm`, from the repository root.
set -euo pipefail
out=${1:-dist-bench}
mkdir -p "$out"
wasm32-wasi-ghc -O1 \
  -package text -package ghc-experimental \
  -iffi/jsstring -iscripts/bench-jsstring/wasm \
  -outputdir "$out/build" \
  -no-hs-main -optl-mexec-model=reactor '-optl-Wl,--export=hs_start' \
  scripts/bench-jsstring/wasm/Main.hs -o "$out/bench.wasm"
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" --input "$out/bench.wasm" --output "$out/ghc_wasm_jsffi.js"
