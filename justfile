# Native (Lynx) dev loop for sample-app-native.
#
# Produces a real Lynx bundle (dist/main.lynx.bundle) via rspeedy, so the Lynx
# explorer can load it directly. Incremental:  edit -> save -> rebuild -> refetch.
#
# Run inside the native toolchain shell:
#     nix develop .#native
#     just watch-native
#
# The Lynx explorer stays pointed at http://<host>:{{port}}/main.lynx.bundle and
# re-fetches on reload, so hosting happens once; no manual re-host/re-open needed.
#
# Pipeline (order matters):
#   1. bun run js        -> js/miso-native.js  (the TS runtime)
#   2. cabal build       -> all.js             (GHC JS backend links miso-native.js
#                                                in via miso.cabal js-sources)
#   3. rspeedy build     -> dist/main.lynx.bundle
#   4. http-server dist/

set shell := ["bash", "-uc"]

# rspeedy build/staging dir; its dist/ is what the explorer loads.
build_dir := "sample-app-native/build"
serve_dir := "sample-app-native/build/dist"
port      := "8080"

# GHC JS backend selection (same flags as the `native` devshell `build`).
jsflags := "--with-compiler=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg"

# List recipes
default:
    @just --list

# 1. Build the TS runtime (js/miso-native.js). cabal links this into all.js, so
#    it must run before build-native-js.
bundle-runtime:
    bun run js

# 2. Compile the Haskell app with the GHC JS backend, emitting all.js.
build-native-js:
    cabal build app-native {{jsflags}}

# 3. Bundle all.js into a Lynx bundle (dist/main.lynx.bundle) via rspeedy.
bundle-lynx:
    #!/usr/bin/env bash
    set -euo pipefail
    alljs="$(cabal list-bin app-native {{jsflags}}).jsexe/all.js"
    rm -rf {{build_dir}}
    mkdir -p {{build_dir}}
    # Pre-bundle all.js so rspeedy's client-side build doesn't choke on the GHC
    # JS RTS's Node shims (e.g. child_process).
    bun build --minify-whitespace --target=bun --outfile={{build_dir}}/all.js "$alljs"
    # rspeedy resolves @lynx-js/* from node_modules next to lynx.config.ts.
    ln -sfn "$(dirname "$(command -v rspeedy)")/../lib/node_modules" {{build_dir}}/node_modules
    cat > {{build_dir}}/lynx.config.ts <<'EOF'
    import { defineConfig } from '@lynx-js/rspeedy';
    import { pluginReactLynx } from '@lynx-js/react-rsbuild-plugin';
    export default defineConfig({
      source: { entry: './all.js' },
      plugins: [ pluginReactLynx() ],
    });
    EOF
    cd {{build_dir}} && rspeedy build

# One full rebuild: runtime -> haskell -> lynx bundle.
rebuild-native: bundle-runtime build-native-js bundle-lynx

# Serve the bundle once (explorer re-fetches on reload).
serve-native:
    http-server {{serve_dir}} -p {{port}}

# (ghciwatch isn't used here: the GHC JS backend has no working `cabal repl`,
# and a bytecode `:r` can't emit all.js anyway.)
# Watch src/, sample-app-native/ (Haskell) and ts/ (TypeScript); rebuild + reserve.
# watchexec runs the command once on startup, so no `rebuild-native` prerequisite
# here (that would build twice).
watch-native:
    #!/usr/bin/env bash
    set -euo pipefail
    http-server {{serve_dir}} -p {{port}} >/dev/null 2>&1 &
    server=$!
    trap "kill $server 2>/dev/null || true" EXIT
    watchexec --restart \
      -w sample-app-native -w src -w ts \
      -e hs,ts \
      -- just rebuild-native
