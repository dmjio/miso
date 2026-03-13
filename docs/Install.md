Installation
==========================

## Compilation

When done developing, we can compile to Web Assembly or JavaScript for distribution. This is done by acquiring a `GHC` that supports WebAssembly or JavaScript. We recommend acquiring these backends using `GHCUp` or `Nix`.

> [!TIP]
> For new Haskell users we recommend using [GHCup](https://www.haskell.org/ghcup/) to acquire the [WASM](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) and [JS](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html) backends.

## ![Image](https://github.com/user-attachments/assets/c57d96b2-368b-410e-b968-28dfe22bf1b1) Web Assembly

> [!TIP]
> The Haskell `miso` team currently recommends using the WASM backend as the default backend for compilation.

Using [GHCup](https://www.haskell.org/ghcup/) you should be able to acquire the `GHC` `WASM` compiler.

For instructions on how to add a third-party channel with [GHCup](https://www.haskell.org/ghcup/), please see their official [README.md](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#using-ghcup)

> [!TIP]
> For [Nix](nixos.org) users it is possible to acquire the WASM backend via a [Nix flake](https://nixos-and-flakes.thiscute.world/)

```bash
$ nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org'
```

> [!NOTE]
> This will put `wasm32-wasi-cabal` in your `$PATH`, along with `wasm32-wasi-ghc`. Since the WASM backend is relatively new, the ecosystem is not entirely patched to support it. Therefore, we will need to use patched packages from time to time.

> [!TIP]
> Instead of using a `nix shell`, it's possible to install the GHC WASM Flake into your environment so it will always be present on `$PATH`

```bash
$ nix profile install 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org'
```

Update your `cabal.project` to the following

- `cabal.project`

```yaml
packages:
  .

with-compiler:
  wasm32-wasi-ghc

with-hc-pkg:
  wasm32-wasi-ghc-pkg

source-repository-package
  type: git
  location: https://github.com/dmjio/miso
  branch: master

if arch(wasm32)
  -- Required for TemplateHaskell. When using wasm32-wasi-cabal from
  -- ghc-wasm-meta, this is superseded by the global cabal.config.
  shared: True
```

Call `wasm32-wasi-cabal build --allow-newer` and a `WASM` payload should be created in `dist-newstyle/` directory.

```bash
$ wasm32-wasi-cabal update
$ wasm32-wasi-cabal build --allow-newer
```

```bash
Configuration is affected by the following files:
- cabal.project
Resolving dependencies...
Build profile: -w ghc-9.12.2.20250327 -O1
In order, the following will be built (use -v for more details):
 - app-0.1.0.0 (exe:app) (configuration changed)
Configuring executable 'app' for app-0.1.0.0...
Preprocessing executable 'app' for app-0.1.0.0...
Building executable 'app' for app-0.1.0.0...
[1 of 1] Compiling Main             ( Main.hs, dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/app-0.1.0.0/x/app/build/app/app-tmp/Main.o )
[2 of 2] Linking dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/app-0.1.0.0/x/app/build/app/app.wasm
```

You have now successfully compiled a Haskell `miso` application to WebAssembly 🔥

***

But, we're not done yet. In order to view this in the browser there are still a few more steps. We need to add some additional files that emulate the [WASI interface](https://github.com/WebAssembly/WASI) in the browser ([A browser WASI shim](https://github.com/bjorn3/browser_wasi_shim)).

> [!NOTE]
> The GHC WASM backend can execute any Haskell program in a WASI-compliant runtime (e.g. [wasmtime](https://github.com/bytecodealliance/wasmtime))
> See the [official documentation](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) for more information.

To start, we recommend creating an `app.wasmexe` folder to store the additional artifacts required.

> [!TIP]
> We recommend using an up-to-date `node` version (currently tested with `v24.2.0`) to ensure `post-link.mjs` works properly.

```bash
# Creates the directory for hosting
$ mkdir -v app.wasmexe
mkdir: created directory 'app.wasmexe'

# This command produces `ghc_wasm_jsffi.js`, which ensures our FFI works properly.
$ $(wasm32-wasi-ghc --print-libdir)/post-link.mjs \
   --input $(wasm32-wasi-cabal list-bin app --allow-newer) \
   --output app.wasmexe/ghc_wasm_jsffi.js

# This copies the `app.wasm` payload into `app.wasmexe`
$ cp -v $(wasm32-wasi-cabal list-bin app --allow-newer) app.wasmexe
Configuration is affected by the following files:
- cabal.project
'/home/dmjio/Desktop/miso/sample-app/dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/app-0.1.0.0/x/app/build/app/app.wasm' -> 'app.wasmexe'
```

> [!NOTE]
> Along with the above `ghc_wasm_jsffi.js` and `app.wasm` artifacts, we also need to include an `index.html` and an `index.js` for loading the WASM payload into the browser.

- `index.html`

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Sample miso WASM counter app</title>
  </head>
  <body>
    <script src="index.js" type="module"></script>
  </body>
</html>
```

- `index.js`

```javascript
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
const { instance } = await WebAssembly.instantiateStreaming(fetch("app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start(globalThis.example);
```

The `app.wasmexe` folder will now look like:

```
❯ ls app.wasmexe
 app.wasm
 ghc_wasm_jsffi.js
 index.html
 index.js
```

Now you can host and view the `app.wasm` payload in a web browser.

```
$ http-server app.wasmexe
```

> [!TIP]
> You can inspect the WASM payload in the `Sources` tab of your browser by right-clicking and then clicking `Inspect`.

![Image](https://github.com/user-attachments/assets/41af274b-2c25-4b26-acb1-d2a5266cfa8a)

## JavaScript

Using [GHCup](https://www.haskell.org/ghcup/) you should be able to acquire the latest GHC JS-backend compiler.

> [!TIP]
> For [Nix](https://nixos.org) users it is possible to acquire the latest JS backend (that `miso` uses) via Nix.
> Use [cachix](https://cachix.org) to ensure you're not building dependencies unnecessarily `cachix use haskell-miso-cachix`

```bash
nix-shell -p pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.ghc -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/65f179f903e8bbeff3215cd613bdc570940c0eab.tar.gz
```

> [!NOTE]
> This will put `javascript-unknown-ghcjs-ghc` in your `$PATH`, along with `javascript-unknown-ghcjs-ghc-pkg`. You might also need to specify in your `cabal.project` file that you are using the JS backend.

> [!TIP]
> Alternatively, if you'd like to install the compiler into your global environment (so you don't need to develop inside a `bash` shell) you can use the following command.
>
> ```bash
> nix-env -iA pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.ghc -f https://github.com/NixOS/nixpkgs/archive/65f179f903e8bbeff3215cd613bdc570940c0eab.tar.gz
> ```

- `cabal.project`

```yaml
packages:
  .

source-repository-package
  type: git
  location: https://github.com/dmjio/miso
  branch: master

with-compiler:
  javascript-unknown-ghcjs-ghc

with-hc-pkg:
  javascript-unknown-ghcjs-ghc-pkg
```

> [!NOTE]
> `cabal` will use the `ghc` specified above in `with-compiler`


```bash
$ cabal update && cabal build --allow-newer
```

```bash
Configuring executable 'app' for app-0.1.0.0...
Preprocessing executable 'app' for app-0.1.0.0...
Building executable 'app' for app-0.1.0.0...
[1 of 1] Compiling Main             ( Main.hs, dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/app-0.1.0.0/x/app/build/app/app-tmp/Main.o )
[2 of 2] Linking dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/app-0.1.0.0/x/app/build/app/app.jsexe

```

> [!TIP]
> To view the JavaScript in your browser, you can use `cabal list-bin` and `http-server`

```bash
$ http-server $(cabal list-bin app --allow-newer).jsexe
Configuration is affected by the following files:
- cabal.project
Starting up http-server, serving /home/dmjio/Desktop/miso/sample-app/dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/app-0.1.0.0/x/app/build/app/app.jsexe

http-server version: 14.1.1

http-server settings:
CORS: disabled
Cache: 3600 seconds
Connection Timeout: 120 seconds
Directory Listings: visible
AutoIndex: visible
Serve GZIP Files: false
Serve Brotli Files: false
Default File Extension: none

Available on:
  http://127.0.0.1:8080
  http://192.168.1.114:8080
Hit CTRL-C to stop the server
```
