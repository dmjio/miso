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

## Table of Contents
- [Quick Start](#quick-start)
  - [Begin](#begin)
  - [Nix](#nix)
  - [Architecture](#architecture)
  - [Under the hood](#under-the-hood)
- [Examples](#examples)
- [Haddocks](#haddocks)
- [Sample Application](#sample-application)
- [Transition Application](#transition-application)
- [Live reload with JSaddle](#live-reload-with-jsaddle)
- [Docker](#docker)
- [Building examples](#building-examples)
- [Coverage](#coverage)
- [Isomorphic](#isomorphic)
- [Pinning nixpkgs](#pinning-nixpkgs)
- [Binary cache](#binary-cache)
- [Benchmarks](#benchmarks)
- [Maintainers](#maintainers)
- [Commercial Users](#commercial-users)
- [Contributing](#contributing)
- [Contributors](#contributors)
- [License](#license)

## Quick start
To start building applications with `miso` you will need to acquire [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/). This can be done via [GHCUP](https://www.haskell.org/ghcup/) or [Nix](https://nixos.org/). 

> [!TIP]
> For new Haskell users we recommend using [GHCUP](https://www.haskell.org/ghcup/) to acquire both [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/)

### Begin
To build and run the sample-app with `nix`, execute the commands below:

```bash
git clone https://github.com/dmjio/miso
cd miso/sample-app
cabal run
# now open http://localhost:8008 in your browser and you should see the +/- app
```

The above commands will add miso's binary cache to your nix installation (support for both Linux and OSX).
`nix-build` will fetch the dependencies from miso's cache and build the sample application.

### Nix
`Nix` is a more powerful option for building web applications with `miso` since it encompasses development workflow, configuration management, and deployment. The source code for [`haskell-miso.org`](https://github.com/dmjio/miso/tree/master/haskell-miso.org) is an example of this.

If unfamiliar with `nix`, we recommend [@Gabriella439](https://github.com/Gabriella439)'s ["Nix and Haskell in production"](https://github.com/Gabriella439/haskell-nix) guide.

To begin, make the following directory layout:
```bash
➜  mkdir app && touch app/{Main.hs,app.cabal,default.nix} && tree app
app
|-- Main.hs
|-- app.cabal
`-- default.nix
```

Add a `cabal` file
```bash
➜  cat app/*.cabal
name:                app
version:             0.1.0.0
synopsis:            First miso app
category:            Web
build-type:          Simple
cabal-version:       >=1.10

executable app
  main-is:             Main.hs
  ghcjs-options:
    -dedupe
  build-depends:       base, miso
  default-language:    Haskell2010
```

Write a `default.nix` (this will fetch a recent version of `miso`). `miso` will provide you with a working `nixpkgs` named `pkgs`. `callCabal2nix` will automatically produce a nix expression that builds your cabal file.

```nix
with (import (builtins.fetchGit {
  url = "https://github.com/dmjio/miso";
  ref = "refs/tags/1.8";
}) {});
pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {}
```

Add the source from [Sample Application](#sample-application) to `app/Main.hs`

Build the project
```
nix-build
```

Open the result
```
open ./result/bin/app.jsexe/index.html
```

For development with `nix`, it can be nice to have `cabal` present for building. This command will make it available in your `PATH`.
```
nix-env -iA cabal-install -f '<nixpkgs>'
```

To be put into a shell w/ `GHCJS` and all the dependencies for this project present, use `nix-shell`.
```
nix-shell -A env
```

To view the dependencies for your project, call `ghcjs-pkg list` when inside the shell.
```
nix-shell -A env --run 'ghcjs-pkg list'
```

To build the project with `cabal` after entering the `nix-shell`
```
nix-shell -A env --run 'cabal configure --ghcjs && cabal build'
```

For incremental development inside of the `nix-shell` we recommend using a tool like [`entr`](http://eradman.com/entrproject/) to automatically rebuild on file changes, or roll your own solution with `inotify`.
```
ag -l | entr sh -c 'cabal build'
```

### Architecture
For constructing client and server applications, we recommend using one `cabal` file with two executable sections, where the `buildable` attribute set is contingent on the compiler. An example of this layout is [here](https://github.com/dmjio/miso/blob/master/haskell-miso.org/haskell-miso.cabal#L16-L60). For more info on how to use `stack` with a `client`/`server` setup, see this [link](https://docs.haskellstack.org/en/stable/ghcjs/#project-with-both-client-and-server). For more information on how to use `nix` with a `client`/`server` setup, see the [nix scripts](https://github.com/dmjio/miso/blob/master/haskell-miso.org/default.nix) for [https://haskell-miso.org](https://haskell-miso.org).

## Under the hood

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

## Sample application

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

## Live reload with JSaddle

It is possible to build `miso` applications with `ghcid`, `jsaddle` that allow live reloading of your application in reponse to changes in application code. To accomplish this, run the `sample-app/` with `ghcid -c 'cabal repl app' -T=Main.main`. Then open your browser to http:localhost:8080. Whenever you edit the code, you should see the browser page refresh with an updated page.

## Docker

Developing miso applications inside a Docker container is supported (allows applications to be built on Windows). See the [README](https://github.com/dmjio/miso/blob/master/docker/README.md) in the `docker` folder for more information.

## Building examples

The easiest way to build the examples is with the [`nix`](https://nixos.org/nix/) package manager
```
git clone https://github.com/dmjio/miso && cd miso && nix-build --arg examples true
```

This will build all examples and documentation into a folder named `result`
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
cd result/bin/todo-mvc.jsexe && nix-shell -p python --run 'python -m SimpleHTTPServer'
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

## Pinning nixpkgs

By default `miso` uses a known-to-work, pinned version of [`nixpkgs`](https://github.com/dmjio/miso/blob/master/nix/nixpkgs.json) known as `pkgs`.
We also maintain a legacy version of nixpkgs known as `legacyPkgs` so we can use tools like `nixops` and build `miso` with the original `GHCJS 8.6` backend.

## Binary cache

`nix` users on a Linux or OSX distro can take advantage of a [binary cache](https://haskell-miso-cachix.cachix.org) for faster builds. To use the binary cache follow the instructions on [cachix](https://haskell-miso-cachix.cachix.org/). We highly recommend using [cachix]() when using `miso` with a `nix` workflow.

```bash
cachix use haskell-miso-cachix
```

## Benchmarks

[According to benchmarks](https://krausest.github.io/js-framework-benchmark/current.html), `miso` is among the fastest functional programming web frameworks, second only to [Elm](http://elm-lang.org).

<a target="_blank" href="https://krausest.github.io/js-framework-benchmark/current.html"><img src="https://cdn-images-1.medium.com/max/1600/1*6EjJTf1mhlTxd4QWsygCwA.png" width="500" height="600" /></a>

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
