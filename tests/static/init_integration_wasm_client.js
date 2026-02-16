import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
import make_ffi from "./wasm.js";

const args = [];
const env = ["GHCRTS=-H64m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const jsffi = make_ffi(instance_exports);

const { instance } = await WebAssembly.instantiateStreaming(fetch("/static/integration-client.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: jsffi,
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await jsffi.ZC0ZCmisozm1zi9zi0zi0zminplaceZCMisoZC();
await instance.exports.hs_start(globalThis.example);
