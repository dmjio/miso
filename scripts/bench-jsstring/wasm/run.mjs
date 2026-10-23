// Runs bench.wasm (built by build.sh) with the same WASI shim as miso's
// tests, printing its PROP| and BENCH| lines.
// Usage: node run.mjs bench.wasm ghc_wasm_jsffi.js [all|props|bench]
// Needs @bjorn3/browser_wasi_shim next to it:
//   npm install --no-save --prefix scripts/bench-jsstring/wasm @bjorn3/browser_wasi_shim@0.3.0
import { WASI, OpenFile, File, ConsoleStdout } from '@bjorn3/browser_wasi_shim';
import { readFileSync } from 'node:fs';
import { resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

const [wasmPath, jsffiPath, mode = 'all'] = process.argv.slice(2);
globalThis.benchMode = mode;
console.log('RUNNER|start|' + mode);
const fds = [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered(msg => console.log(msg)),
  ConsoleStdout.lineBuffered(msg => console.error(msg)),
];
const wasi = new WASI([], ['GHCRTS=-H64m'], fds, { debug: false });
const ghc_wasm_jsffi = (await import(pathToFileURL(resolve(jsffiPath)).href)).default;
const instance_exports = {};
const { instance } = await WebAssembly.instantiate(readFileSync(wasmPath), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);
wasi.initialize(instance);
console.log('RUNNER|initialized');
await instance.exports.hs_start();
console.log('RUNNER|finished');
