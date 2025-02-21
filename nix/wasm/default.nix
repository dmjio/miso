self: super:
{

  ghc-wasm-meta =
    (builtins.getFlake "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org")
      .outputs.packages."${super.system}";

  wasm-cabal =
    self.ghc-wasm-meta.wasm32-wasi-cabal-9_12;

  wasm-ghc =
    self.ghc-wasm-meta.wasm32-wasi-ghc-9_12;

  wasmWebBuilder = args: with self;
    # dmj: note, only works for single files (no cabal / hackage support)
    super.stdenv.mkDerivation {
      inherit (args) src name;
      buildInputs = [ ghc-wasm-meta.all_9_12 ];
      buildCommand = with args; ''
         mkdir -p $out/${name}.jsexe/

         wasm32-wasi-ghc \
            -no-hs-main -optl-mexec-model=reactor "-optl-Wl,--export=hs_start" \
            ${src} -o ${name}.wasm

         $(wasm32-wasi-ghc --print-libdir)/post-link.mjs \
              --input ${name}.wasm \
              --output ghc_wasm_jsffi.js

         cp -v ghc_wasm_jsffi.js            $out/${name}.jsexe/
         mv -v ${name}.wasm                 $out/${name}.jsexe/
         cat ${wasmIndexJs name}          > $out/${name}.jsexe/index.js
         cat ${wasmIndexHtml title name}  > $out/${name}.jsexe/index.html
      '';
    };

  # dmj: ghc-wasm-meta already provides
  nodeLatest =
    super.stdenv.mkDerivation rec {
      name = "nodejs";
      src = builtins.fetchTarball
        "https://nodejs.org/dist/v22.14.0/node-v22.14.0-linux-x64.tar.xz";
      installPhase = ''
        mkdir $out
        cp -r $src/bin $out
        cp -r $src/include $out
        cp -r $src/lib $out
        cp -r $src/share $out
      '';
    };

  wasmHelloWorld = super.writeTextFile {
    name = "Main.hs";
    text = builtins.readFile ../../examples/wasm-hello-world/Main.hs;
  };

  wasmIndexHtml = title: name:
    super.writeTextFile {
      name = "index.html";
      text = ''<!DOCTYPE html>
               <html>
                 <head>
                   <meta charset="utf-8">
                   <meta name="viewport" content="width=device-width, initial-scale=1">
                   <title>${title}</title>
                 </head>
                 <body>
                   <script>globalThis.example = "${name}";</script>
                   <script src="index.js" type="module"></script>
                 </body>
               </html>
             '';
    };

  wasmIndexJs = name:
    super.writeTextFile {
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
