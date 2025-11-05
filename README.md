<h1 align="center">miso</h1>

<p align="center">

<a href="https://haskell-miso.org">
  <img width=10% src="https://em-content.zobj.net/thumbs/240/apple/325/steaming-bowl_1f35c.png">
   </a>
<p align="center">A <i>tasty</i> <a href="https://www.haskell.org/"><strong>Haskell</strong></a> web and <a href="https://github.com/haskell-miso/miso-lynx">mobile</a> framework 🍜</p>
</p>

<p align="center">
  <a href="https://matrix.to/#/#haskell-miso:matrix.org">
    <img src="https://img.shields.io/badge/matrix.org-miso-FF4B33.svg?style=for-the-badge" alt="Matrix #haskell-miso:matrix.org">
  </a>
  <a href="https://haskell-miso-cachix.cachix.org">
    <img src="https://img.shields.io/badge/build-cachix-yellow.svg?style=for-the-badge" alt="Cachix">
  </a>
  <a href="https://actions-badge.atrox.dev/dmjio/miso/goto?ref=master">
    <img alt="Build Status" src="https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fdmjio%2Fmiso%2Fbadge%3Fref%3Dmaster&style=for-the-badge" />
  </a>
  <a href="http://hackage.haskell.org/package/miso">
    <img src="https://img.shields.io/hackage/v/miso.svg?style=for-the-badge" alt="Hackage">
  </a>
</p>

##

**Miso** is a small, production-ready, component-oriented, [reactive](https://github.com/haskell-miso/miso-reactive), [isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/) [Haskell](https://www.haskell.org/) front-end framework for quickly building highly interactive single-page web and [mobile](https://github.com/dmjio/miso-native) applications. It features a virtual-dom, recursive diffing / patching algorithm, attribute and property normalization, event delegation, event batching, SVG, 2D/3D Canvas (WebGL), Server-sent events ([SSE](https://github.com/haskell-miso/miso-sse)), [Websockets](https://github.com/haskell-miso/miso-websocket), type-safe [servant](https://haskell-servant.github.io/)-style routing and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/) and [React](http://react.dev/). **Miso** is pure by default, but side effects can be introduced into the system via the `Effect` data type.

**Miso** makes heavy use of the [GHC Javascript FFI](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#javascript-ffi-in-the-wasm-backend) and therefore has minimal dependencies. **Miso** can be considered a shallow [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language) for modern web programming.

**Miso** supports compilation to both [JavaScript](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html) and [WebAssembly](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) using [GHC](https://www.haskell.org/ghc/). For hot-reload, `miso` uses the [jsaddle](https://github.com/ghcjs/jsaddle) library. When used with [ghcid](https://github.com/ndmitchell/ghcid) and [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch) this enables a rapid development workflow.

> [!IMPORTANT]
> Check out the new [Haskell Miso Organization](https://github.com/haskell-miso) 🍜

## Table of Contents
- [History](#history-)
- [Quick Start](#quick-start-)
- [Getting Started](#getting-started)
- [Setup](#setup-%EF%B8%8F)
- [Hot Reload](#hot-reload-)
- [Compilation](#compilation)
- [WebAssembly](#-web-assembly)
- [JavaScript](#JavaScript)
- [Haddocks](#haddocks)
- [Architecture](#architecture)
- [Internals](#internals-%EF%B8%8F)
- [Examples](#examples)
- [Building examples](#building-examples)
- [HTTP](#interacting-with-http-apis-)
- [Coverage](#coverage-)
- [Isomorphic](#isomorphic-%EF%B8%8F)
- [Native](#native)
- [Benchmarks](#benchmarks-%EF%B8%8F)
- [Nix](#nix-)
  - [Pinning nixpkgs](#pinning-nixpkgs-)
  - [Binary cache](#binary-cache)
- [Community](#community-)
- [Maintainers](#maintainers)
- [Commercial](#commercial-)
- [Contributing](#contributing)
- [Contributors](#contributors-)
- [Partnerships](#partnerships-)
- [Backers](#backers)
- [Organizations](#organizations)
- [License](#license)

# History 📜

> **miso** is a play on the words ***micro*** and ***isomorphic***.

[miso](https://haskell-miso.org) began in 2016 as research in:
  - Expressing the [Elm architecture](https://elm-lang.org) in [GHCJS](https://github.com/ghcjs/ghcjs) as an [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language).
  - Implementing modern JavaScript frontend techniques as found in [react](https://react.dev) (e.g. [reconciliation](https://legacy.reactjs.org/docs/reconciliation.html), [isomorphic](https://en.wikipedia.org/wiki/Isomorphic_JavaScript))

Miso aims to [bridge the gap](https://wiki.haskell.org/The_JavaScript_Problem) between modern JavaScript frameworks (such as [React](https://reactjs.org), [Vue.js](https://vuejs.org), etc.) and functional programming in [Haskell](https://haskell.org). It has since grown to encompass more features from the JavaScript community like [Components](https://react.dev/learn/your-first-component) and [Renderers](https://github.com/chentsulin/awesome-react-renderer). Miso also now supports [native development](https://github.com/haskell-miso/miso-lynx) for [iOS](https://www.apple.com/ios/), [Android](https://www.android.com/) and [HarmonyOS](https://device.harmonyos.com/en/) devices via [LynxJS](https://lynxjs.org) and targets additional backends like [Web Assembly](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html).

> [!Note]
> [React-style Components](https://github.com/dmjio/miso/pull/766) are now added to `miso` as of version `1.9`. This has not yet been released, we recommend developing against `master` if you'd like to use latest features.

## Quick start ⚡

> [!TIP]
> We have a [template repository](https://github.com/haskell-miso/miso-sampler) that includes a sample counter application and build scripts for all platforms.

The fastest way to get started is to clone and build the [miso sampler](https://github.com/haskell-miso/miso-sampler) repository 🍱 This has build scripts for Web Assembly, JS and vanilla [GHC](https://haskell.org/ghc). This requires [Nix Flakes](https://wiki.nixos.org/wiki/Flakes) usage. See also the section on using [miso's binary cache](#binary-cache).

## Getting started

To start developing applications with `miso` you will need to acquire [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/). This can be done via [GHCup](https://www.haskell.org/ghcup/) or [Nix](https://nixos.org/).

> [!TIP]
> For new Haskell users we recommend using [GHCup](https://www.haskell.org/ghcup/) to acquire both [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/)

## Setup 🏗️

To develop and build your first `miso` application you will need 3 files:

  - `cabal.project`
  - `app.cabal`
  - `Main.hs`

### `cabal.project`

```yaml
packages:
  .

source-repository-package
  type: git
  location: https://github.com/dmjio/miso
  branch: master
```

### `app.cabal`

We recommend using at least `cabal-version: 2.2`, this will give you the [common sections](https://vrom911.github.io/blog/common-stanzas) feature which we will use later to allow multiple compilers to build our project (so we can target `WASM` and `JS` backends)

```yaml
cabal-version: 2.2
name: app
version: 0.1.0.0
synopsis: Sample miso app
category: Web

common options
  if arch(wasm32)
    ghc-options:
      -no-hs-main
      -optl-mexec-model=reactor
      "-optl-Wl,--export=hs_start"
    cpp-options:
      -DWASM

  if arch(javascript)
     ld-options:
       -sEXPORTED_RUNTIME_METHODS=HEAP8

executable app
  import:
    options
  main-is:
    Main.hs
  build-depends:
    base, miso
  default-language:
    Haskell2010
```

### `Main.hs`

This file contains a simple `miso` counter application.

```haskell
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _counter :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run (startApp app)
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component emptyModel updateModel viewModel
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
  AddOne        -> counter += 1
  SubtractOne   -> counter -= 1
  SayHelloWorld -> io_ $ do
    alert "Hello World"
    consoleLog "Hello World"
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel x =
  H.div_
    [ H.className "counter"
    ]
    [ H.button_ [ H.onClick AddOne ] [ text "+" ]
    , text $ ms (x ^. counter)
    , H.button_ [ H.onClick SubtractOne ] [ text "-" ]
    , H.br_ []
    , H.button_ [ H.onClick SayHelloWorld ] [ text "Alert Hello World!" ]
  ]
----------------------------------------------------------------------------
```

Now that your project files are populated, development can begin.

## Hot Reload 🔥

With `GHC` and `cabal` on `$PATH`, call `cabal repl`

```bash
$ cabal repl
```

You should see the following output in your terminal.

```bash
[1 of 2] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
ghci>
```

Now call the `main` function in the `GHCi` REPL.

```bash
ghci> main
Running on port 8008...
<a href="http://localhost:8008">run</a>
ghci>
```

> [!NOTE]
> The code running in this example is not compiled to JavaScript or WebAssembly, rather it is running the client side application on the server. It works by sending commands to a small javascript interpreter over a websocket to render elements on the page. This is provided by the [jsaddle](https://github.com/ghcjs/jsaddle) library.

If you visit [http://localhost:8008](http://localhost:8008), the application will be live. You can now edit `Main.hs`, call `:r` and `main` in `GHCi`, and the application will update on the screen.

> [!NOTE]
> Instead of typing `:r` and `main` manually inside of `GHCi` on every file change, you can use [ghcid](https://github.com/ndmitchell/ghcid) or [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch) tools to do it automatically.

> [!TIP]
> For users accustomed to a react.js worfklow, we highly recommend using either `ghcid` or `ghciwatch`.

Below is an example of usage with `ghcid`

```bash
$ ghcid -c 'cabal repl app' -T=Main.main
```

This screenshot shows the hot-reload functionality in action. This is using `ghcid`, `jsaddle` and `miso`.

![Image](https://github.com/user-attachments/assets/4c5e7191-e4a9-4270-a28b-2f5f71ad6f40)

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

## Haddocks

Offical [Haskell](https://haskell.org) documentation of the [Miso](https://haskell-miso.org) web framework.

| Platform | URL |
|------|-------------|
| GHCJS | [Link](https://haddocks.haskell-miso.org/) |
| GHC | [Link](http://hackage.haskell.org/package/miso) |


## Architecture

For constructing client and server applications, we recommend using one `cabal` file with two executable sections, where the `buildable` attribute set is contingent on the compiler. An example of this layout is [here](https://github.com/dmjio/miso/blob/master/haskell-miso.org/haskell-miso.cabal#L24-L32).

> [!TIP]
> For more information on how to use `nix` with a `client`/`server` setup, see the [nix scripts](https://github.com/haskell-miso/haskell-miso.org/blob/master/default.nix) for [https://haskell-miso.org](https://haskell-miso.org).

## Internals ⚙️

For some details of the internals and general overview of how `miso` works, see the [Internals](docs/Internals.md).

## Examples

For real-world examples of Haskell `miso` applications, see below.

| Name                  | Description                               | Source Link                                                   | Live Demo Link                                            | Author                                            |
|-----------------------|-------------------------------------------|---------------------------------------------------------------|-----------------------------------------------------------|---------------------------------------------------|
| **TodoMVC**           | A classic TodoMVC implementation          | [Source](https://github.com/haskell-miso/miso-todomvc)        | [Demo](https://todomvc.haskell-miso.org)       | [@dmjio](https://github.com/dmjio)                |
| **2048**              | A clone of the 2048 game                  | [Source](https://github.com/haskell-miso/miso-2048)           | [Demo](https://2048.haskell-miso.org/)         | [@ptigwe](https://github.com/ptigwe)              |
| **Flatris**           | A Tetris-like game                        | [Source](https://github.com/haskell-miso/miso-flatris)        | [Demo](https://flatris.haskell-miso.org/)      | [@ptigwe](https://github.com/ptigwe)              |
| **Plane**             | A flappy-birds-like game                  | [Source](https://github.com/haskell-miso/miso-plane)          | [Demo](https://plane.haskell-miso.org/)        | [@Lermex](https://github.com/Lermex)              |
| **Snake**             | The classic Snake game                    | [Source](https://github.com/haskell-miso/miso-snake)          | [Demo](https://snake.haskell-miso.org/)        | [@lbonn](https://github.com/lbonn)                |
| **SVG**               | An example showcasing SVG rendering       | [Source](https://github.com/haskell-miso/miso-svg)            | [Demo](https://svg.haskell-miso.org/)          | [@dmjio](https://github.com/dmjio)                |
| **Fetch**             | An example demonstrating AJAX requests    | [Source](https://github.com/haskell-miso/miso-fetch)          | [Demo](https://fetch.haskell-miso.org)         | [@dmjio](https://github.com/dmjio)                |
| **File Reader**       | A FileReader API example                  | [Source](https://github.com/haskell-miso/miso-filereader)     | [Demo](https://filereader.haskell-miso.org)    | [@dmjio](https://github.com/dmjio)                |
| **Mario**             | A Super Mario physics example             | [Source](https://github.com/haskell-miso/miso-mario)          | [Demo](https://mario.haskell-miso.org)         | [@dmjio](https://github.com/dmjio)                |
| **WebSocket**         | A simple WebSocket example                | [Source](https://github.com/haskell-miso/miso-websocket)      | [Demo](https://websocket.haskell-miso.org)     | [@dmjio](https://github.com/dmjio)                |
| **Router**            | A client-side routing example             | [Source](https://github.com/haskell-miso/miso-router)         | [Demo](https://router.haskell-miso.org)        | [@dmjio](https://github.com/dmjio)                |
| **Canvas 2D**         | A 2D Canvas rendering example             | [Source](https://github.com/haskell-miso/miso-canvas2d)       | [Demo](https://canvas.haskell-miso.org)      | [@dmjio](https://github.com/dmjio)                |
| **Components**        | A simple pub/sub setup for Component      | [Source](https://github.com/haskell-miso/miso-components)     | [Demo](https://components.haskell-miso.org)    | [@dmjio](https://github.com/dmjio)                |
| **MathML**            | A MathML example                          | [Source](https://github.com/haskell-miso/miso-mathml)         | [Demo](https://mathml.haskell-miso.org)        | [@dmjio](https://github.com/dmjio)                |
| **Simple**            | A simple counter example                  | [Source](https://github.com/haskell-miso/miso-simple)         | [Demo](https://simple.haskell-miso.org)        | [@dmjio](https://github.com/dmjio)                |
| **SSE**               | SSE (Server-sent events) Example          | [Source](https://github.com/haskell-miso/miso-sse)            | [Demo](https://sse.haskell-miso.org)           | [@dmjio](https://github.com/dmjio)                |
| **Three.js**          | A 3D rendering example using Three.JS     | [Source](https://github.com/haskell-miso/three-miso)          | [Demo](https://threejs.haskell-miso.org/)        | [@juliendehos](https://github.com/juliendehos)    |
| **Space Invaders**    | A Space-Invaders-like game                | [Source](https://github.com/haskell-miso/miso-invaders)       | [Demo](https://space-invaders.haskell-miso.org/)     | [@juliendehos](https://github.com/juliendehos)    |
| **Audio**             | Audio examples                            | [Source](https://github.com/haskell-miso/miso-audio)          | [Demo](https://audio.haskell-miso.org/)        | [@juliendehos](https://github.com/juliendehos)    |
| **Video**             | Video examples                            | [Source](https://github.com/haskell-miso/miso-video)          | [Demo](https://video.haskell-miso.org/)        | [@juliendehos](https://github.com/juliendehos)    |
| **WebVR**             | WebVR examples                            | [Source](https://github.com/haskell-miso/miso-aframe)         | [Demo](https://aframe.haskell-miso.org/)        | [@dmjio](https://github.com/dmjio)    |
| **Reactivity**             | Reactive examples                            | [Source](https://github.com/haskell-miso/miso-reactive)         | [Demo](https://reactive.haskell-miso.org/)        | [@dmjio](https://github.com/dmjio)    |


## Building examples

The easiest way to build the examples is with the [`nix`](https://nixos.org/nix/) package manager.

> [!TIP]
> Use [cachix](https://cachix.org) to ensure you're not building dependencies unnecessarily `cachix use haskell-miso-cachix`

See the [@HaskellMiso](https://github.com/haskell-miso) organization for all examples, and how to build then.

## Interacting with HTTP APIs 🔌

If you want to interact with an HTTP API, we recommend one of the following approaches:

  1. For a simple JSON-based API, you can use Miso's [Fetch](https://haddocks.haskell-miso.org/miso/Miso-Fetch.html) module.

  2. In more complex cases, you can define a [Servant](https://www.servant.dev/) API and automatically obtain client functions via [servant-miso-client](https://github.com/haskell-miso/servant-miso-client).

     The Fetch example ([Source](https://github.com/haskell-miso/miso-fetch), [Demo](https://fetch.haskell-miso.org/)) demonstrates the necessary ingredients. Make sure to add the following to your `cabal.project`:

     ```cabal
     source-repository-package
       type: git
       location: https://github.com/amesgen/servant-client-js
       tag: 2853fb4f26175f51ae7b9aaf0ec683c45070d06e
     ```

## Coverage ✅

The core engine of `miso` is the [diff](https://github.com/dmjio/miso/blob/master/ts/miso/dom.ts) function. It is responsible for all DOM manipulation that occurs in a miso application and has [100% code coverage](http://coverage.haskell-miso.org). Tests and coverage made possible using [bun](https://github.com/oven-sh/bun).

> [!NOTE]
> To run the tests and build the coverage report ensure [bun](https://github.com/oven-sh/bun) is installed.

```bash
$ curl -fsSL https://bun.sh/install | bash
```
or

```bash
$ nix-env -iA bun -f '<nixpkgs>'
```

and

```bash
$ bun install && bun run test
```

```bash
--------------------|---------|---------|-------------------
File                | % Funcs | % Lines | Uncovered Line #s
--------------------|---------|---------|-------------------
All files           |   92.37 |   85.48 |
 ts/happydom.ts     |  100.00 |  100.00 |
 ts/miso/dom.ts     |  100.00 |  100.00 |
 ts/miso/event.ts   |   90.91 |   81.62 |
 ts/miso/hydrate.ts |   80.00 |   91.24 |
 ts/miso/smart.ts   |  100.00 |  100.00 |
 ts/miso/util.ts    |   83.33 |   40.00 |
--------------------|---------|---------|-------------------

 84 pass
 0 fail
```

## Isomorphic ☯️

[Isomorphic javascript](https://en.wikipedia.org/wiki/Isomorphic_JavaScript) is a technique for increased SEO, code-sharing and perceived page load times. It works in two parts. First, the server sends a pre-rendered HTML body to the client's browser. Second, after the client javascript application loads, the pointers of the pre-rendered DOM are copied into the virtual DOM (a process known as [hydration](https://en.wikipedia.org/wiki/Hydration_(web_development))), and the application proceeds as normal. All subsequent page navigation is handled locally by the client, while avoiding full-page postbacks.

> [!NOTE]
> The [miso](https://haddocks.haskell-miso.org/miso/Miso.html#v:miso) function is used to facilitate the pointer-copying behavior client-side.

## Native📱
Miso supports the creation of iOS and Android applications via [LynxJS](https://lynxjs.org). See the [miso-lynx](https://github.com/haskell-miso/miso-lynx) repository for more information.

## Benchmarks 🏎️

[According to benchmarks](https://krausest.github.io/js-framework-benchmark/current.html), `miso` is among the fastest functional programming web frameworks, second only to [Elm](http://elm-lang.org).

<a target="_blank" href="https://krausest.github.io/js-framework-benchmark/current.html"><img src="https://cdn-images-1.medium.com/max/1600/1*6EjJTf1mhlTxd4QWsygCwA.png" width="500" height="600" /></a>

## Nix <img src="https://raw.githubusercontent.com/NixOS/nixos-artwork/refs/heads/master/logo/nix-snowflake-colours.svg" alt="nixos-snowflake" width="25"/>

`Nix` is a powerful option for building web applications with `miso` since it encompasses development workflow, configuration management, and deployment. The source code for [`haskell-miso.org`](https://github.com/dmjio/miso/tree/master/haskell-miso.org) is an example of this.

> [!TIP]
> If unfamiliar with `nix`, we recommend [@Gabriella439](https://github.com/Gabriella439)'s ["Nix and Haskell in production"](https://github.com/Gabriella439/haskell-nix) guide.

### Pinning nixpkgs 📌

By default `miso` uses a known-to-work, pinned version of [`nixpkgs`](https://github.com/dmjio/miso/blob/master/nix/nixpkgs.json) known as `pkgs`.

> [!NOTE]
> `miso` also maintains a legacy version of nixpkgs known as `legacyPkgs` so we can use tools like `nixops` for deployment and to build `miso` with the original `GHCJS 8.6` backend.

### Binary cache

`nix` users on a Linux or OSX distros can take advantage of a [binary cache](https://haskell-miso-cachix.cachix.org) for faster builds. To use the binary cache follow the instructions on [cachix](https://haskell-miso-cachix.cachix.org/).

> [!TIP]
> We highly recommend nix users consume the [cachix](https://cachix.org) cache. `cachix use haskell-miso-cachix`.

```bash
$ cachix use haskell-miso-cachix
```

## Community :octocat:

- [Github](https://github.com/haskell-miso)
- [Matrix](https://matrix.to/#/#haskell-miso:matrix.org)
- [Discord](https://discord.gg/QVDtfYNSxq)
- [Slack](https://haskell-miso.slack.com/join/shared_invite_confirmed/zt-37vusrcdw-HH6~hY0DGT7MLCjNWZvLDQ#/email-invite/credentials)
- [IRC](https://www.irccloud.com/invite?channel=%23haskell-miso&hostname=irc.libera.chat&port=6697&ssl=1)

## Maintainers

[@dmjio](https://github.com/dmjio)

## Commercial 🚀

Since it's launch, `miso` has been used in a variety of industries, including but not limited to:

  - SF bay-area startups
  - Quantitative finance
  - Network security
  - Defense research
  - Academia
  - Public sector
  - Non-profit sector
  - etc.

The largest `miso` installation known was ~200,000 lines of `miso` code with 10,000+ users.

## Contributing

Feel free to dive in! [Open an issue](https://github.com/dmjio/miso/issues/new) or a submit [Pull Request](https://github.com/dmjio/miso/pulls).

See [CONTRIBUTING](https://github.com/dmjio/miso/blob/master/CONTRIBUTING.md) for more info.

## Contributors 🦾

> [!NOTE]
> This project exists thanks to all the people who [contribute](CONTRIBUTING.md)

<a href="https://github.com/dmjio/miso/graphs/contributors"><img src="https://opencollective.com/miso/contributors.svg?width=890&button=false" /></a>

## Partnerships 🤝

If you'd like to support this project financially, be it through requesting feature development, or a corporate partnership, please drop us a line and we will be in touch shortly. <p><a href="mailto:support@haskell-miso.org">support@haskell-miso.org</a></p>

## Backers

Become a [financial contributor](https://opencollective.com/miso/contribute) and help us sustain our project and community. We are very grateful and thankful for our individual sponsors.

  - Moses Tschanz
  - [@MaxGabriel](https://github.com/MaxGabriel)
  - [@DigitalOcean](https://github.com/DigitalOcean)
  - [@maybetonyfu](https://github.com/maybetonyfu)
  - [@jhrcek](https://github.com/jhrcek)
  - etc.

<a href="https://opencollective.com/miso"><img src="https://opencollective.com/miso/individuals.svg?width=890"></a>

## Organizations

[Support this project](https://opencollective.com/miso/contribute) with your organization. Your logo will show up here with a link to your website. We are also very grateful and thankful for our corporate sponsors.

<a target="_blank" href="https://opencollective.com/miso/organization/0/website"><img src="https://opencollective.com/miso/organization/0/avatar.svg"></a>

## License

[BSD3](LICENSE) © dmjio
