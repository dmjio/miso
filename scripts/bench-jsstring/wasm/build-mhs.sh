#!/usr/bin/env bash
# Builds the benchmark with MicroHs (browser target: emscripten, WASM in one
# app.js) into $1 (default: dist-bench-mhs). Run inside `nix develop .#mhs`,
# from the repository root.
set -euo pipefail
out=${1:-dist-bench-mhs}
mkdir -p "$out"
mhs -XCPP -iffi/jsstring -iscripts/bench-jsstring/wasm -tbrowser Main -o"$out/app.js"
