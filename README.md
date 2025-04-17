<h1 align="center">miso</h1>
<p align="center">

<a href="https://haskell-miso.org">
  <img width=10% src="https://em-content.zobj.net/thumbs/240/apple/325/steaming-bowl_1f35c.png">
   </a>
<p align="center">A <i>tasty</i> <a href="https://www.haskell.org/"><strong>Haskell</strong></a> front-end web framework 🍜</p>
</p>

<p align="center">
  <a href="https://matrix.to/#/#haskell-miso:matrix.org">
    <img src="https://img.shields.io/badge/matrix.org-miso-E01563.svg?style=flat-square" alt="Matrix #haskell-miso:matrix.org">
  </a>
  <a href="https://haskell.org">
    <img src="https://img.shields.io/badge/language-Haskell-orange.svg?style=flat-square" alt="Haskell">
  </a>
  <a href="https://haskell-miso-cachix.cachix.org">
    <img src="https://img.shields.io/badge/build-cachix-yellow.svg?style=flat-square" alt="Cachix">
  </a>
  <a href="https://actions-badge.atrox.dev/dmjio/miso/goto?ref=master">
    <img alt="Build Status" src="https://img.shields.io/endpoint.svg?url=https%3A%2F%2Factions-badge.atrox.dev%2Fdmjio%2Fmiso%2Fbadge%3Fref%3Dmaster&style=flat-square" />
  </a>
  <a href="https://discord.gg/QVDtfYNSxq">
    <img alt="Discord" src="https://img.shields.io/discord/1302720467232096358?style=flat-square&label=discord&logoColor=7289da">
  </a>
  <a href="http://hackage.haskell.org/package/miso">
    <img src="https://img.shields.io/hackage/v/miso.svg?style=flat-square" alt="Hackage">
  </a>
  <a href="https://github.com/dmjio/miso/blob/master/LICENSE">
    <img src="http://img.shields.io/badge/license-BSD3-blueviolet.svg?style=flat-square" alt="LICENSE">
  </a>
</p>

**Miso** is a small, production-ready, component-oriented, [isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/) [Haskell](https://www.haskell.org/) front-end framework for quickly building highly interactive single-page web applications. It features a virtual-dom, recursive diffing / patching algorithm, attribute and property normalization, event delegation, event batching, SVG, Server-sent events ([SSE](https://html.spec.whatwg.org/multipage/server-sent-events.html)), [Websockets](https://websockets.spec.whatwg.org/), type-safe [servant](https://haskell-servant.github.io/)-style routing and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/) and [React](http://react.dev/). **Miso** is pure by default, but side effects can be introduced into the system via the `Effect` data type. **Miso** makes heavy use of the [GHC Javascript FFI](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#javascript-ffi-in-the-wasm-backend) and therefore has minimal dependencies. **Miso** can be considered a shallow [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language) for modern web programming.

**Miso** supports compilation to both [JavaScript](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html) and [WebAssembly](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) using [GHC](https://www.haskell.org/ghc/). For hot-reload, `miso` uses the [jsaddle](https://github.com/ghcjs/jsaddle) library. When used with [ghcid](https://github.com/ndmitchell/ghcid) and [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch) this enables a rapid development workflow.

> [!TIP]
> [React-style Components](https://github.com/dmjio/miso/pull/766) are now added to `miso` as of version `1.9`. This has not yet been released, we recommend developing against `master` if you'd like to use latest features.

## Table of Contents
- [Quick Start](#quick-start)
- [Setup](#setup)
- [Hot Reload](#hot-reload)
- [Compilation](#compilation)
- [WASM](#wasm)
- [JS](#JS)
- [Architecture](#architecture)
- [Internals](#internals)
- [Examples](#examples)
- [Haddocks](#haddocks)
- [Building examples](#building-examples)
- [Coverage](#coverage)
- [Isomorphic](#isomorphic)
- [Benchmarks](#benchmarks)
- [Nix](#nix)
  - [Pinning nixpkgs](#pinning-nixpkgs)
  - [Binary cache](#binary-cache)
- [Maintainers](#maintainers)
- [Commercial Users](#commercial-users)
- [Contributing](#contributing)
- [Contributors](#contributors)
- [License](#license)

## Quick start
To start developing applications with `miso` you will need to acquire [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/). This can be done via [GHCup](https://www.haskell.org/ghcup/) or [Nix](https://nixos.org/).

> [!TIP]
> For new Haskell users we recommend using [GHCup](https://www.haskell.org/ghcup/) to acquire both [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/)

## Setup

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

We recommend using at least `cabal-version: 2.2`, this will give you the [common sections](https://vrom911.github.io/blog/common-stanzas) feature which we will use later to allow multiple compilers to build our project (so we can target WASM / JS, etc.)

```yaml
cabal-version: 2.2
name: app
version: 0.1.0.0
synopsis: Sample miso app
category: Web

common wasm
  if arch(wasm32)
    ghc-options:
      -no-hs-main
      -optl-mexec-model=reactor
      "-optl-Wl,--export=hs_start"
    cpp-options:
      -DWASM

executable app
  import:
    wasm
  main-is:
    Main.hs
  build-depends:
    base, miso, lens
  default-language:
    Haskell2010
```

### `Main.hs`

This file contains a simple `miso` counter application.

```haskell
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import Miso
import Miso.String
import Miso.Lens
----------------------------------------------------------------------------
-- | Application model state
newtype Model = Model
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
-- | `defaultApp` takes as arguments the initial model, update function, view function
app :: App Effect Model Action ()
app = defaultApp emptyModel updateModel viewModel
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect Model Action ()
updateModel = \case
  AddOne        -> counter += 1
  SubtractOne   -> counter -= 1
  SayHelloWorld -> io $ do
    consoleLog "Hello World"
    alert "Hello World"
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ []
  [ button_ [ onClick AddOne ] [ text "+" ]
  , text $ ms (x ^. counter)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , button_ [ onClick SayHelloWorld ] [ text "Alert Hello World!" ]
  ]
----------------------------------------------------------------------------
```

Now that your project files are populated, development can begin. 

## Hot Reload

With `GHC` and `cabal` on `$PATH`, call `cabal repl`

```bash
$ cabal repl
```

You should see the following output in your terminal.

```
[1 of 2] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
ghci> main
Running on port 8008...
<a href="http://localhost:8008">run</a>
ghci>
```

If you visit [http://localhost:8008](http://localhost:8008), the application will be live. You can now edit the `Main.hs` file and call `:r` and `main` in the repl, and it should update the screen. Instead of typing `:r` and `main` manually inside of `GHCi` on every file change, you can use [ghcid](https://github.com/ndmitchell/ghcid) or [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch) tools to do it automatically.

> [!TIP]
> For users accustomed to a react.js worfklow, we highly recommend using either `ghcid` or `ghciwatch`.

Below is an example of usage with `ghcid`

```bash
$ ghcid -c 'cabal repl app' -T=Main.main
```

This screenshot shows the hot-reload functionality in action. This is using `ghcid`, `jsaddle` and `miso`.

![Image](https://github.com/user-attachments/assets/4c5e7191-e4a9-4270-a28b-2f5f71ad6f40)

## Compilation

When you're done developing your application, you will want to compile it to Web Assembly or JavaScript for distribution. This can be done by acquiring a `GHC` that supports Web Assembly or JavaScript. We recommend acquiring these backends using `GHCUp` or `Nix`.

> [!TIP]
> For new Haskell users we recommend using [GHCup](https://www.haskell.org/ghcup/) to acquire the [WASM](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) and [JS](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html) backends.

## WASM

> [!NOTE]
> The Haskell `miso` team currently recommends using the WASM backend as the default backend for compilation.

Using [GHCup](https://www.haskell.org/ghcup/) you should be able to acquire the `GHC` `WASM` compiler.

For instructions on how to add a third-party channel with [GHCup](https://www.haskell.org/ghcup/), please see their official [README.md](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#using-ghcup)

> [!TIP]
> For [Nix](nixos.org) users it is possible to acquire the WASM backend via a Nix flake

```bash
$ nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org'
```

> [!NOTE]
> This will put `wasm32-wasi-cabal` in your `$PATH`, along with `wasm32-wasi-ghc`. Since the WASM backend is relatively new, the ecosystem is not entirely patched to support it. Therefore, we will need to use patched packages, from time to time. 

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

  -- https://github.com/haskellari/time-compat/issues/37
  -- Older versions of time don't build on WASM.
  constraints: time installed
  allow-newer: time

  -- https://github.com/haskellari/splitmix/pull/73
  source-repository-package
    type: git
    location: https://github.com/amesgen/splitmix
    tag: 5f5b766d97dc735ac228215d240a3bb90bc2ff75
```

Call `wasm32-wasi-cabal build --allow-newer` and a `WASM` payload should be created in `dist-newstyle/` directory.

```bash
$ wasm32-wasi-cabal build --allow-newer
Configuration is affected by the following files:
- cabal.project
Resolving dependencies...
Build profile: -w ghc-9.12.2.20250327 -O1
In order, the following will be built (use -v for more details):
 - app-0.1.0.0 (exe:app) (configuration changed)
Created semaphore called cabal_semaphore_b with 12 slots.
Configuring executable 'app' for app-0.1.0.0...
Preprocessing executable 'app' for app-0.1.0.0...
Building executable 'app' for app-0.1.0.0...
[1 of 1] Compiling Main             ( Main.hs, dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/app-0.1.0.0/x/app/build/app/app-tmp/Main.o ) [Miso.Lens package changed]
[2 of 2] Linking dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/app-0.1.0.0/x/app/build/app/app.wasm
```

You have now successfully compiled Haskell `miso` code to Web Assembly. Congratulations. But, we're not done yet. In order to view this in the browser there are still a few more steps. We need to add some additional files that emulate the [WASI interface](https://github.com/WebAssembly/WASI) in the browser (A [browser WASI shim](https://github.com/bjorn3/browser_wasi_shim)). 

> [!NOTE]
> The GHC WASM backend can execute any Haskell program in a WASI-compliant runtime (e.g. [wasmtime](https://github.com/bytecodealliance/wasmtime))

To start, we recommend creating an `app.wasmexe` folder to store the additional artifacts required.

```bash
$ mkdir -v app.wasmexe
mkdir: created directory 'app.wasmexe'

$ $(wasm32-wasi-ghc --print-libdir)/post-link.mjs \ 
   --input $(wasm32-wasi-cabal list-bin app --allow-newer) \
   --output app.wasmexe/ghc_wasm_jsffi.js

Configuration is affected by the following files:
- cabal.project

$ cp -v $(wasm32-wasi-cabal list-bin app --allow-newer) app.wasmexe
Configuration is affected by the following files:
- cabal.project
'/home/dmjio/Desktop/miso/sample-app/dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/app-0.1.0.0/x/app/build/app/app.wasm' -> 'app.wasmexe'
```

Along with the above `ghc_wasm_jsffi.js`, `app.wasm` artifacts we also need to include an `index.html` and an `index.js` for loading the WASM payload into the browser.

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

Now you can host and view your examples in a web browser.

```
$ http-server app.wasmexe
```

## JS

Using [GHCup](https://www.haskell.org/ghcup/) you should be able to acquire the JS-backend compiler.

> [!TIP]
> For [Nix](https://nixos.org) users it is possible to acquire the latest JS backend via Nix

```bash
❯ nix-shell -p pkgs.pkgsCross.ghcjs.haskell.packages.ghc9121.ghc '<nixpkgs>'
```

This will put `javascript-unknown-ghcjs-ghc` in your `$PATH`, along with `javascript-unknown-ghcjs-ghc-pkg`. You might also need to specify in your `cabal.project` file that you are using the JS backend.

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

> [!NOTE] `cabal` will use the `ghc` specified above in `with-compiler`


```bash
$ cabal update && cabal build --allow-newer
```


## Architecture

For constructing client and server applications, we recommend using one `cabal` file with two executable sections, where the `buildable` attribute set is contingent on the compiler. An example of this layout is [here](https://github.com/dmjio/miso/blob/master/haskell-miso.org/haskell-miso.cabal#L24-L32). For more information on how to use `nix` with a `client`/`server` setup, see the [nix scripts](https://github.com/dmjio/miso/blob/master/haskell-miso.org/default.nix) for [https://haskell-miso.org](https://haskell-miso.org).

## Internals

For some details of the internals and general overview of how `miso` works, see the [Internals](docs/Internals.md).

## Examples

| Name | Description | Source Link | Live Demo Link | Author |
|------|-------------|-------------|----------------|--------|
| **TodoMVC** | A classic TodoMVC implementation | [Source](https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs) | [Demo](https://todo-mvc.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **2048** | A clone of the 2048 game | [Source](https://github.com/ptigwe/hs2048/) | [Demo](https://2048.haskell-miso.org/) | [@ptigwe](https://github.com/ptigwe) |
| **Flatris** | A Tetris-like game | [Source](https://github.com/ptigwe/hs-flatris/) | [Demo](https://flatris.haskell-miso.org/) | [@ptigwe](https://github.com/ptigwe) |
| **Plane** | A flappy-birds-like game | [Source](https://github.com/Lermex/miso-plane) | [Demo](https://miso-plane.haskell-miso.org/) | [@Lermex](https://github.com/Lermex) |
| **Snake** | The classic Snake game | [Source](https://github.com/lbonn/miso-snake) | [Demo](https://snake.haskell-miso.org/) | [@lbonn](https://github.com/lbonn) |
| **SVG** | An example showcasing SVG rendering | [Source](https://github.com/dmjio/miso/blob/master/examples/svg/Main.hs) | [Demo](https://svg.haskell-miso.org/) | [@dmjio](https://github.com/dmjio) |
| **Fetch** | An example demonstrating AJAX requests | [Source](https://github.com/dmjio/miso/blob/master/examples/fetch/Main.hs) | [Demo](https://fetch.haskell-miso.org/) | [@dmjio](https://github.com/dmjio) |
| **File Reader** | A FileReader API example | [Source](https://github.com/dmjio/miso/blob/master/examples/file-reader/Main.hs) | [Demo](https://file-reader.haskell-miso.org/) | [@dmjio](https://github.com/dmjio) |
| **WebGL** | A 3D rendering example using Three.JS | [Source](https://github.com/dmjio/miso/blob/master/examples/three/Main.hs) | [Demo](https://threejs.haskell-miso.org/) | [@dmjio](https://github.com/dmjio) |
| **Mario** | A Super Mario physics example | [Source](https://github.com/dmjio/miso/blob/master/examples/mario/Main.hs) | [Demo](https://mario.haskell-miso.org/) | [@dmjio](https://github.com/dmjio) |
| **WebSocket** | A simple WebSocket example | [Source](https://github.com/dmjio/miso/blob/master/examples/websocket/Main.hs) | [Demo](https://websocket.haskell-miso.org/) | [@dmjio](https://github.com/dmjio) |
| **Router** | A client-side routing example | [Source](https://github.com/dmjio/miso/blob/master/examples/router/Main.hs) | [Demo](https://router.haskell-miso.org/) | [@dmjio](https://github.com/dmjio) |
| **Canvas 2D** | A 2D Canvas rendering example | [Source](https://github.com/dmjio/miso/blob/master/examples/canvas2d/Main.hs) | [Demo](https://canvas.haskell-miso.org/) | [@dmjio](https://github.com/dmjio) |

## Building examples

The easiest way to build the examples is with the [`nix`](https://nixos.org/nix/) package manager

> [!TIP]
> Use [cachix](https://cachix.org) to ensure you're not building dependencies unnecassarily.
> `$ cachix use haskell-miso-cachix`


```
git clone https://github.com/dmjio/miso && cd miso && nix-build -A miso-examples
```

## Haddocks

Offical [Haskell](https://haskell.org) documentation of the [Miso](https://haskell-miso.org) web framework.

| Platform | URL |
|------|-------------|
| GHCJS | [Link](https://haddocks.haskell-miso.org/) |
| GHC | [Link](http://hackage.haskell.org/package/miso) |

This will compile all the examples to JavaScript into a folder named `result`.

```
➜ tree -d ./result/bin
./result/bin
|-- canvas2d.jsexe
|-- file-reader.jsexe
|-- mario.jsexe
|   `-- imgs
|-- mathml.jsexe
|-- router.jsexe
|-- simple.jsexe
|-- svg.jsexe
|-- tests.jsexe
|-- threejs.jsexe
|-- todo-mvc.jsexe
|-- websocket.jsexe
`-- fetch.jsexe
```

> [!NOTE] To see examples, we recommend hosting them with a web server (we use [http-server](https://github.com/http-party/http-server))

```
cd result/bin/todo-mvc.jsexe && http-sever
Serving HTTP on 0.0.0.0 port 8000 ...
```

## Coverage

The core algorithmic component of `miso` is the [diff](https://github.com/dmjio/miso/blob/master/ts/dom.ts) function. It is responsible for all DOM manipulation that occurs in a miso application and has [100% code coverage](http://coverage.haskell-miso.org). Tests and coverage made possible using [bun](https://github.com/oven.sh/bun).

To run the tests and build the coverage report ensure [bun](https://github.com/oven.sh/bun) is installed.

```bash 
$ curl -fsSL https://bun.sh/install | bash
```

```bash
nix `nix-env -iA bun -f '<nixpkgs>'
```

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
 ts/miso/event.ts   |   90.91 |   81.62 | 30,66-73,81,94,109-110,122-126,132-136,147-148
 ts/miso/hydrate.ts |   80.00 |   91.24 | 39,66-69,88-93,202-203,231-233
 ts/miso/smart.ts   |  100.00 |  100.00 |
 ts/miso/util.ts    |   83.33 |   40.00 | 22-42
--------------------|---------|---------|-------------------

 84 pass
 0 fail
```

## Isomorphic

[Isomorphic javascript](https://en.wikipedia.org/wiki/Isomorphic_JavaScript) is a technique for increased SEO, code-sharing and perceived page load times. It works in two parts. First, the server sends a pre-rendered HTML body to the client's browser. Second, after the client javascript application loads, the pointers of the pre-rendered DOM are copied into the virtual DOM (a process known as [hydration](https://en.wikipedia.org/wiki/Hydration_(web_development))), and the application proceeds as normal. All subsequent page navigation is handled locally by the client, while avoiding full-page postbacks.

> [!NOTE] The `miso` function is used to facilitate the pointer-copying behavior client-side.

## Benchmarks

[According to benchmarks](https://krausest.github.io/js-framework-benchmark/current.html), `miso` is among the fastest functional programming web frameworks, second only to [Elm](http://elm-lang.org).

<a target="_blank" href="https://krausest.github.io/js-framework-benchmark/current.html"><img src="https://cdn-images-1.medium.com/max/1600/1*6EjJTf1mhlTxd4QWsygCwA.png" width="500" height="600" /></a>

## Nix

`Nix` is a powerful option for building web applications with `miso` since it encompasses development workflow, configuration management, and deployment. The source code for [`haskell-miso.org`](https://github.com/dmjio/miso/tree/master/haskell-miso.org) is an example of this.

If unfamiliar with `nix`, we recommend [@Gabriella439](https://github.com/Gabriella439)'s ["Nix and Haskell in production"](https://github.com/Gabriella439/haskell-nix) guide.

### Pinning nixpkgs

By default `miso` uses a known-to-work, pinned version of [`nixpkgs`](https://github.com/dmjio/miso/blob/master/nix/nixpkgs.json) known as `pkgs`.
We also maintain a legacy version of nixpkgs known as `legacyPkgs` so we can use tools like `nixops` and build `miso` with the original `GHCJS 8.6` backend.

### Binary cache

`nix` users on a Linux or OSX distro can take advantage of a [binary cache](https://haskell-miso-cachix.cachix.org) for faster builds. To use the binary cache follow the instructions on [cachix](https://haskell-miso-cachix.cachix.org/). We highly recommend using [cachix]() when using `miso` with a `nix` workflow.

> [!TIP]
> We highly recommend nix users consume the [cachix](https://cachix.org) cache that `cachix use haskell-miso-cachix`.


```bash
cachix use haskell-miso-cachix
```

## Maintainers

[@dmjio](https://github.com/dmjio)

## Contributing

Feel free to dive in! [Open an issue](https://github.com/dmjio/miso/issues/new) or submit [PRs](https://github.com/dmjio/miso/pulls).

See [CONTRIBUTING](https://github.com/dmjio/miso/blob/master/CONTRIBUTING.md) for more info.

## Contributors
This project exists thanks to all the people who contribute. [[Contribute](CONTRIBUTING.md)].
<a href="https://github.com/dmjio/miso/graphs/contributors"><img src="https://opencollective.com/miso/contributors.svg?width=890&button=false" /></a>

## Financial Contributors

Become a financial contributor and help us sustain our project and community. [[Contribute](https://opencollective.com/miso/contribute)]

We are very grateful and thankful for our corporate and individual sponsors.

  - Moses Tschanz
  - [@MaxGabriel](https://github.com/MaxGabriel)
  - [@DigitalOcean](https://github.com/DigitOcean)
  - [@maybetonyfu](https://github.com/maybetonyfu)
  - etc.

## Partnerships

If you'd like to support this project financially, be it through requesting feature development, or a corporate partnership, please drop us a line and we will be in touch shortly. <p><a href="mailto:code@dmj.io">code@dmj.io</a></p> 

## Individuals
    
<a href="https://opencollective.com/miso"><img src="https://opencollective.com/miso/individuals.svg?width=890"></a>

## Organizations

Support this project with your organization. Your logo will show up here with a link to your website. [[Contribute](https://opencollective.com/miso/contribute)]

<a href="https://opencollective.com/miso/organization/0/website"><img src="https://opencollective.com/miso/organization/0/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/1/website"><img src="https://opencollective.com/miso/organization/1/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/2/website"><img src="https://opencollective.com/miso/organization/2/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/3/website"><img src="https://opencollective.com/miso/organization/3/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/4/website"><img src="https://opencollective.com/miso/organization/4/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/5/website"><img src="https://opencollective.com/miso/organization/5/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/6/website"><img src="https://opencollective.com/miso/organization/6/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/7/website"><img src="https://opencollective.com/miso/organization/7/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/8/website"><img src="https://opencollective.com/miso/organization/8/avatar.svg"></a>
<a href="https://opencollective.com/miso/organization/9/website"><img src="https://opencollective.com/miso/organization/9/avatar.svg"></a>

## License

[BSD3](LICENSE) © dmjio
