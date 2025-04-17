<h1 align="center">miso</h1>
<p align="center">

<a href="https://haskell-miso.org">
  <img width=10% src="https://em-content.zobj.net/thumbs/240/apple/325/steaming-bowl_1f35c.png">
   </a>
<p align="center">A <i>tasty</i> <a href="https://www.haskell.org/"><strong>Haskell</strong></a> front-end web framework</p>
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
  <a href="https://github.com/dmjio/miso/actions">
    <img src="https://github.com/dmjio/miso/workflows/Miso%20CI/badge.svg" alt="GitHub Actions">
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

**Miso** is a small, production-ready, component-oriented, [isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/) [Haskell](https://www.haskell.org/) front-end framework for quickly building highly interactive single-page web applications. It features a virtual-dom, recursive diffing / patching algorithm, attribute and property normalization, event delegation, event batching, SVG, Server-sent events, Websockets, type-safe [servant](https://haskell-servant.github.io/)-style routing and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/) and [React](http://react.dev/). **Miso** is pure by default, but side effects can be introduced into the system via the `Effect` data type. **Miso** makes heavy use of the [GHC Javascript FFI](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#javascript-ffi-in-the-wasm-backend) and therefore has minimal dependencies. **Miso** can be considered a shallow [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language) for modern web programming.

**Miso** supports compilation with both the latest [JavaScript](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html) and [WebAssembly](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) backends for [GHC](https://www.haskell.org/ghc/). For development with hot-reload, `miso` supports the [jsaddle](https://github.com/ghcjs/jsaddle) package that enables fast iteration of development over a WebSocket with vanilla [GHC](https://www.haskell.org/ghc/). Combined with [ghcid](https://github.com/ndmitchell/ghcid) or [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch), your development environment should reload your changes on every file save in your editor of choice.

> [!Info]
> React-style Components are now added to Miso as of version 1.9. This has not yet been released, we recommend developing against 'master' if you'd like these latest features

## Table of Contents
- [Quick Start](#quick-start)
  - [Scaffolding](#scaffolding)
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

Now you should be able to setup a development environment.

### Scaffolding

To create the necessary files, you can call `cabal init` to make a new project. Ensure your new project has 3 files, `cabal.project` (with a `miso` entry), a `app.cabal` file, and a `Main.hs` for the `miso` code. The three files are shown below.

- `cabal.project`

This example includes `miso` at the tip of `master`, but you can use any hash you'd like. We recommend a tagged release.

```yaml
packages:
  .

source-repository-package
  type: git
  location: https://github.com/dmjio/miso
  branch: master
```
- `miso.cabal`

We recommend using at least `cabal-version: 2.2`, this will give you the "common sections" feature which we will use to allow multiple compilers to build our project (so we can target WASM / JS, etc.)

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

- `Main.hs`

This file contains a simple `miso` counter application.

```haskell
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
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
  , text . ms $ x^.counter
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , button_ [ onClick SayHelloWorld ] [ text "Alert Hello World!" ]
  ]
----------------------------------------------------------------------------
```

Now that your project files are setup you can now begin development. If you call `cabal repl` and `main` you should now see the following:

```
[1 of 2] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
ghci> main
Running on port 8008...
<a href="http://localhost:8008">run</a>
ghci>
```

If you visit the URL at [http://localhost:8008](http://localhost:8008), you should see the [Sample Application](#sample-application) live.

To reload the application after making changes you can edit the `Main.hs` file and call `:r` and `main` in the repl, and it should update the screen.

Instead of typing `:r` and `main` manually inside of `GHCi` on every file change, you can use [ghcid](https://github.com/ndmitchell/ghcid) or [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch) tools, and they will do it for you instead.

Below an example of usage with `ghcid`.

```bash
$ ghcid -c 'cabal repl app' -T=Main.main
```

Below is a screenshot of what hot-reload iterative development looks like with `ghcid`, `jsaddle` and `miso`.

![Image](https://github.com/user-attachments/assets/4c5e7191-e4a9-4270-a28b-2f5f71ad6f40)


### Compilation

When you're done developing your application, you will want to compile it to JavaScript or Web Assembly for distribution. This can be done by acquiring a `GHC` that supports WebAssembly or JavaScript. We recommend acquiring these backends using `GHCUp` or `Nix`.

> [!TIP]
> For new Haskell users we recommend using [GHCup](https://www.haskell.org/ghcup/) to acquire the [WASM](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) and [JS]() backends.

#### WASM

Using [GHCup](https://www.haskell.org/ghcup/) you should be able to acquire the GHC-WASM compiler.

For instructions on how to add a third-party channel with [GHCup](https://www.haskell.org/ghcup/), please see their [official README.md](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#using-ghcup)

> [!TIP]
> For [Nix](nixos.org) users it is possible to acquire the WASM backend via a Nix flake

```bash
$ nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org'
```

This will put `wasm32-wasi-cabal` in your `$PATH`, along with `wasm32-wasi-ghc`. To build:

```bash
$ wasm32-wasi-cabal update && wasm32-wasi-cabal build --allow-newer
```

#### JS

Using [GHCup](https://www.haskell.org/ghcup/) you should be able to acquire the JS-backend compiler.

> [!TIP]
> For nix users it is possible to acquire the WASM backend via a Nix flake


## Architecture

For constructing client and server applications, we recommend using one `cabal` file with two executable sections, where the `buildable` attribute set is contingent on the compiler. An example of this layout is [here](https://github.com/dmjio/miso/blob/master/haskell-miso.org/haskell-miso.cabal#L24-L32). For more information on how to use `nix` with a `client`/`server` setup, see the [nix scripts](https://github.com/dmjio/miso/blob/master/haskell-miso.org/default.nix) for [https://haskell-miso.org](https://haskell-miso.org).

## Internals

For details of the internals and general overview of how `miso` works, see the [Internals](docs/Internals.md).

## Examples

| Name | Description | Source Link | Live Demo Link | Author |
|------|-------------|-------------|----------------|--------|
| **TodoMVC** | A classic TodoMVC implementation | [Source](https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs) | [Demo](https://todo-mvc.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **2048** | A clone of the 2048 game | [Source](https://github.com/ptigwe/hs2048/) | [Demo](https://2048.haskell-miso.org/) | @[ptigwe](https://github.com/ptigwe) |
| **Flatris** | A Tetris-like game | [Source](https://github.com/ptigwe/hs-flatris/) | [Demo](https://flatris.haskell-miso.org/) | @[ptigwe](https://github.com/ptigwe) |
| **Plane** | A flappy-birds-like game | [Source](https://github.com/Lermex/miso-plane) | [Demo](https://miso-plane.haskell-miso.org/) | @[Lermex](https://github.com/Lermex) |
| **Snake** | The classic Snake game | [Source](https://github.com/lbonn/miso-snake) | [Demo](https://snake.haskell-miso.org/) | @[lbonn](https://github.com/lbonn) |
| **SVG** | An example showcasing SVG rendering | [Source](https://github.com/dmjio/miso/blob/master/examples/svg/Main.hs) | [Demo](https://svg.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **Fetch** | An example demonstrating AJAX requests | [Source](https://github.com/dmjio/miso/blob/master/examples/fetch/Main.hs) | [Demo](https://fetch.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **File Reader** | A FileReader API example | [Source](https://github.com/dmjio/miso/blob/master/examples/file-reader/Main.hs) | [Demo](https://file-reader.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **WebGL** | A 3D rendering example using Three.JS | [Source](https://github.com/dmjio/miso/blob/master/examples/three/Main.hs) | [Demo](https://threejs.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **Mario** | A Super Mario physics example | [Source](https://github.com/dmjio/miso/blob/master/examples/mario/Main.hs) | [Demo](https://mario.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **WebSocket** | A simple WebSocket example | [Source](https://github.com/dmjio/miso/blob/master/examples/websocket/Main.hs) | [Demo](https://websocket.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **Router** | A client-side routing example | [Source](https://github.com/dmjio/miso/blob/master/examples/router/Main.hs) | [Demo](https://router.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |
| **Canvas 2D** | A 2D Canvas rendering example | [Source](https://github.com/dmjio/miso/blob/master/examples/canvas2d/Main.hs) | [Demo](https://canvas.haskell-miso.org/) | @[dmjio](https://github.com/dmjio) |

## Haddocks

| Platform | URL |
|------|-------------|
| GHCJS | [Link](https://haddocks.haskell-miso.org/) |
| GHC | [Link](http://hackage.haskell.org/package/miso) |

## Building examples

The easiest way to build the examples is with the [`nix`](https://nixos.org/nix/) package manager

```
git clone https://github.com/dmjio/miso && cd miso && nix-build -A miso-examples
```

This will compile all the examples to JavaScript into a folder named `result`.

```
➜  miso git:(master) ✗ tree -d ./result/bin
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

To see examples, we recommend hosting them with a webserver

```
cd result/bin/todo-mvc.jsexe && http-sever
Serving HTTP on 0.0.0.0 port 8000 ...
```

## Coverage

The core algorithmic component of `miso` is the [diff](https://github.com/dmjio/miso/blob/master/ts/dom.ts) function. It is responsible for all DOM manipulation that occurs in a miso application and has [100% code coverage](http://coverage.haskell-miso.org). Tests and coverage made possible using [bun](https://github.com/oven.sh/bun).

To run the tests and build the coverage report ensure [bun](https://github.com/oven.sh/bun) is installed. You can do this with `curl -fsSL https://bun.sh/install | bash` or nix `nix-env -iA bun -f '<nixpkgs>'`.

```bash
$ bun install && bun run test
```

## Isomorphic

[Isomorphic javascript](https://en.wikipedia.org/wiki/Isomorphic_JavaScript) is a technique for increased SEO, code-sharing and perceived page load times. It works in two parts. First, the server sends a pre-rendered HTML body to the client's browser. Second, after the client javascript application loads, the pointers of the pre-rendered DOM are copied into the virtual DOM (a process known as [hydration](https://en.wikipedia.org/wiki/Hydration_(web_development))), and the application proceeds as normal. All subsequent page navigation is handled locally by the client, while avoiding full-page postbacks.

The `miso` function is used to facilitate the pointer-copying behavior client-side.

For more information on how `miso` handles isomorphic javascript, we recommend [this tutorial](https://github.com/FPtje/miso-isomorphic-example).

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

```bash
cachix use haskell-miso-cachix
```

## Maintainers

[@dmjio](https://github.com/dmjio)

## Contributing

Feel free to dive in! [Open an issue](https://github.com/dmjio/miso/issues/new) or submit [PRs](https://github.com/dmjio/miso/pulls).

See [CONTRIBUTING](https://github.com/dmjio/miso/blob/master/CONTRIBUTING.md) for more info.

## Contributors

### Code Contributors

This project exists thanks to all the people who contribute. [[Contribute](CONTRIBUTING.md)].
<a href="https://github.com/dmjio/miso/graphs/contributors"><img src="https://opencollective.com/miso/contributors.svg?width=890&button=false" /></a>

### Financial Contributors

Become a financial contributor and help us sustain our community. [[Contribute](https://opencollective.com/miso/contribute)]

We are very thankful and grateful for our corporate and individual sponsors.

  - Moses Tschanz
  - [@MaxGabriel](https://github.com/MaxGabriel)
  - [@DigitalOcean](https://github.com/MaxGabriel)
  - TonyFu
  - etc.

#### Partnerships

If you'd like to support this project financially, be it through requesting feature development, or a corporate partnership / funding situation, please drop us a line at
[Miso Opportunities](mailto:code@dmj.io?subject=[Miso]%20Opportunities) regarding what you have in mind and we will be in touch shortly.

#### Individuals

<a href="https://opencollective.com/miso"><img src="https://opencollective.com/miso/individuals.svg?width=890"></a>

#### Organizations

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

[BSD3](LICENSE) © David Johnson
