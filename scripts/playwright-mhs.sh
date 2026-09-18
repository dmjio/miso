#!/usr/bin/env bash
# Build the sample app with MicroHs (mhs, emscripten target) and run the
# browser smoke test in ts/playwright-mhs.ts.
#
# Needs on the PATH: mhs (with the emscripten target configured), emcc, node,
# http-server; and the playwright npm package (bun install / npm install) with
# PLAYWRIGHT_BROWSERS_PATH set (e.g. to nixpkgs' playwright-driver.browsers).
#
# Usage: scripts/playwright-mhs.sh [browser|browser_js]   (default: browser)
set -euo pipefail
cd "$(dirname "$0")/.."
target="${1:-browser}"
make -C sample-app mhs MHS_TARGET="$target"
http-server sample-app/public-mhs -p 8080 -s &
server=$!
trap 'kill $server 2>/dev/null' EXIT
sleep 1
NODE_PATH="$PWD/node_modules" node ts/playwright-mhs.ts
