# The HTML/JS harness needed to actually load and run a wasm32-wasi
# executable in a browser via @bjorn3/browser_wasi_shim. Self-contained
# copies of the same small templates already in ../wasm/default.nix (the
# ad-hoc overlay backing the legacy sampleWasm/wasmWebBuilder path) --
# duplicated rather than shared because that overlay isn't applied on the
# flake's pkgs, only on the legacy nix/default.nix's.
pkgs:
{
  indexHtml = title: name: scripts:
    pkgs.writeTextFile {
      name = "index.html";
      text = ''<!DOCTYPE html>
               <html>
                 <head>
                   <meta charset="utf-8">
                   <meta name="viewport" content="width=device-width, initial-scale=1">
                   <title>${title}</title>
                   ${scripts}
                 </head>
                 <body>
                   <script>globalThis.example = "${name}";</script>
                   <script src="index.js" type="module"></script>
                 </body>
               </html>
             '';
    };

  indexJs = name:
    pkgs.writeTextFile {
      name = "index.js";
      text =
        ''
        import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
        import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

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
        const { instance } = await WebAssembly.instantiateStreaming(fetch("${name}.wasm"), {
          wasi_snapshot_preview1: wasi.wasiImport,
          ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
        });
        Object.assign(instance_exports, instance.exports);

        wasi.initialize(instance);
        await instance.exports.hs_start(globalThis.example);
        '';
    };
}
