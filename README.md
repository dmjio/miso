<h1 align="center">miso</h1>
<p align="center">

<a href="https://haskell-miso.org">
  <img width=10% src="https://em-content.zobj.net/thumbs/240/apple/325/steaming-bowl_1f35c.png">
   </a>
<p align="center">A <i>tasty</i> <a href="https://www.haskell.org/"><strong>Haskell</strong></a> front-end framework</p>
</p>

<p align="center">
  <a href="https://join.slack.com/t/haskell-miso/shared_invite/zt-1w80x404h-AF2Be_BDQixNMadzadnUNg">
<img src="https://img.shields.io/badge/slack-miso-E01563.svg?style=flat-square" alt="Miso Slack">
  </a>
  <a href="https://haskell.org">
	<img src="https://img.shields.io/badge/language-Haskell-orange.svg?style=flat-square" alt="Haskell">
  </a>
  <a href="https://miso-haskell.cachix.org">
	<img src="https://img.shields.io/badge/build-cachix-yellow.svg?style=flat-square" alt="Cachix">
  </a>
  <a href="https://github.com/dmjio/miso/actions">
    <img src="https://github.com/dmjio/miso/workflows/test%20matrix/badge.svg" alt="GitHub Actions">
  </a>
  <a href="http://hackage.haskell.org/package/miso">
	<img src="https://img.shields.io/hackage/v/miso.svg?style=flat-square" alt="Hackage">
  </a>
  <a href="https://www.irccloud.com/invite?channel=%23haskell-miso&amp;hostname=irc.libera.chat&amp;port=6697&amp;ssl=1">
	<img src="https://img.shields.io/badge/irc-%23haskell--miso-1e72ff.svg?style=flat-square" alt="IRC #haskell-miso">
  </a>
  <a href="https://github.com/dmjio/miso/blob/master/LICENSE">
	<img src="http://img.shields.io/badge/license-BSD3-blueviolet.svg?style=flat-square" alt="LICENSE">
  </a>
</p>

**Miso** is a small, production-ready, "[isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/)" [Haskell](https://www.haskell.org/) front-end framework for quickly building highly interactive single-page web applications. It features a virtual-dom, recursive diffing / patching algorithm, attribute and property normalization, event delegation, event batching, SVG, Server-sent events, Websockets, type-safe [servant](https://haskell-servant.github.io/)-style routing and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril). **Miso** is pure by default, but side effects (like `XHR`) can be introduced into the system via the `Effect` data type. **Miso** makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies. **Miso** can be considered a shallow [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language) for modern web programming.

## Table of Contents
- [Quick Start](#quick-start)
  - [Begin](#begin)
  - [Nix](#nix)
  - [Architecture](#architecture)
- [Examples](#examples)
  - [TodoMVC](#todomvc)
  - [Flatris](#flatris)
  - [2048](#2048)
  - [Snake](#snake)
  - [Mario](#mario)
  - [Miso Plane (Flappy Birds)](#miso-plane-flappy-birds)
  - [Websocket](#websocket)
  - [SSE](#sse)
  - [XHR](#xhr)
  - [Router](#router)
  - [SVG](#svg)
  - [Canvas 2D](#canvas-2d)
  - [ThreeJS](#threejs)
  - [Simple](#simple)
  - [File Reader](#file-reader)
  - [WebVR](#webvr)
  - [Pixel Card Wars](#pixel-card-wars)
  - [Currency Converter](#currency-converter)
- [Haddocks](#haddocks)
  - [GHC](#ghc)
  - [GHCJS](#ghcjs)
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
To get started quickly building applications, we recommend using the [`nix`](https://nixos.org/nix) package manager with miso's binary cache provided by [`cachix`](https://miso-haskell.cachix.org/). It is possible to use [`stack`](https://docs.haskellstack.org/en/stable/README/) to build GHCJS projects, but support for procuring `GHCJS` has been removed [as of stack 2.0](https://github.com/commercialhaskell/stack/issues/4086). `nix` is used to procure a working version of `GHCJS`. If you're using `cabal` we assume you have [obtained `GHCJS`](https://github.com/ghcjs/ghcjs#installation) by other means. All source code depicted below for the quick start app is available [here](https://github.com/dmjio/miso/tree/master/sample-app).

### Begin
To build the sample-app with `nix`, execute the commands below:

```bash
# optional use of cache
nix-env -iA cachix -f https://cachix.org/api/v1/install
# optional use of cache
cachix use miso-haskell
git clone https://github.com/dmjio/miso
cd miso/sample-app
nix-build
open ./result/bin/app.jsexe/index.html
```

The above commands will add miso's binary cache to your nix installation (support for both Linux and OSX).
`nix-build` will fetch the dependencies from miso's cache and build the sample application.

### Nix
`Nix` is a more powerful option for building web applications with `miso` since it encompasses development workflow, configuration management, and deployment. The source code for [`haskell-miso.org`](https://github.com/dmjio/miso/tree/master/examples/haskell-miso.org) is an example of this.

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
For constructing client and server applications, we recommend using one `cabal` file with two executable sections, where the `buildable` attribute set is contingent on the compiler. An example of this layout is [here](https://github.com/dmjio/miso/blob/master/examples/haskell-miso.org/haskell-miso.cabal#L16-L60). For more info on how to use `stack` with a `client`/`server` setup, see this [link](https://docs.haskellstack.org/en/stable/ghcjs/#project-with-both-client-and-server). For more information on how to use `nix` with a `client`/`server` setup, see the [nix scripts](https://github.com/dmjio/miso/blob/master/examples/haskell-miso.org/default.nix) for [https://haskell-miso.org](https://haskell-miso.org).

## Examples

### TodoMVC
  - [Link](https://todo-mvc.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs)

### Flatris
  - [Link](https://flatris.haskell-miso.org/) / [Source](https://github.com/ptigwe/hs-flatris/)

### 2048
  - [Link](https://2048.haskell-miso.org/) / [Source](https://github.com/ptigwe/hs2048/)

### Snake
  - [Link](https://snake.haskell-miso.org/) / [Source](https://github.com/lbonn/miso-snake)

### Mario
  - [Link](https://mario.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/mario/Main.hs)

### Miso Plane (Flappy Birds)
  - [Link](http://miso-plane.haskell-miso.org) / [Source](https://github.com/Lermex/miso-plane)

### Websocket
  - [Link](https://websocket.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/websocket/Main.hs)

### SSE
  - [Link](http://sse.haskell-miso.org/) / [Client](https://github.com/dmjio/miso/blob/master/examples/sse/client/Main.hs) / [Server](https://github.com/dmjio/miso/blob/master/examples/sse/server/Main.hs)

### XHR
  - [Link](https://xhr.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/xhr/Main.hs)

### Router
  - [Link](https://router.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/router/Main.hs)

### SVG
  - [Link](https://svg.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/svg/Main.hs)

### Canvas 2D
  - [Link](https://canvas.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/canvas2d/Main.hs)

### ThreeJS
  - [Link](https://threejs.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/three/Main.hs)

### Simple
  - [Link](https://simple.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/simple/Main.hs)

### File Reader
  - [Link](https://file-reader.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/file-reader/Main.hs)

### WebVR
  - [Link](http://fizruk.github.io/fpconf-2017-talk/miso-aframe-demo/dist/demo.jsexe/index.html) / [Source](https://github.com/fizruk/miso-aframe)

### Pixel Card Wars
  - [Link](https://www.schplaf.org/hgames/darkcraw/) / [Source](https://github.com/smelc/miso-darkcraw)

### Currency Converter
  - [Link](https://functora.github.io/apps/currency-converter) / [Source](https://github.com/functora/functora.github.io/tree/master/ghcjs/currency-converter)

## Haddocks

### GHCJS
  - [Link](https://haddocks.haskell-miso.org/)

### GHC
  - [Link](http://hackage.haskell.org/package/miso)

## Sample application
```haskell
-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only applies to `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel action m =
  case action of
    AddOne
      -> noEff (m + 1)
    SubtractOne
      -> noEff (m - 1)
    NoOp
      -> noEff m
    SayHelloWorld
      -> m <# do consoleLog "Hello World" >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
```

## Transition application

An alternative, more powerful interface for constructing `miso` applications is using the `Transition` interface.
`Transition` is based on the `StateT` monad transformer, and can be used to construct components. It also works
very nicely with lenses based on `MonadState` (i.e. `(.=)`, `(%=)`,`(+=)`,`(-=)`).


```haskell
-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

-- | Lens import
import Control.Lens

-- | Type synonym for an application model
data Model
  = Model
  { _counter :: Int
  } deriving (Show, Eq)

counter :: Lens' Model Int
counter = lens _counter $ \record field -> record { _counter = field }

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = Model 0              -- initial model
    update = fromTransition . updateModel -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only applies to `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Action Model ()
updateModel action =
  case action of
    AddOne
      -> counter += 1
    SubtractOne
      -> counter -= 1
    NoOp
      -> pure ()
    SayHelloWorld
      -> scheduleIO_ (consoleLog "Hello World")

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text . ms $ x^.counter
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
```

## Live reload with JSaddle

It is possible to build `miso` applications with `ghcid`, `jsaddle` that allow live reloading of your application in reponse to changes in application code. See the [README](https://github.com/dmjio/miso/blob/master/sample-app-jsaddle/README.md) in the `sample-app-jsaddle` folder for more information.

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
|-- compose-update.jsexe
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
`-- xhr.jsexe
```

To see examples, we recommend hosting them with a webserver

```
cd result/bin/todo-mvc.jsexe && nix-shell -p python --run 'python -m SimpleHTTPServer'
Serving HTTP on 0.0.0.0 port 8000 ...
```

## Coverage

The core algorithmic component of miso is [diff.js](https://github.com/dmjio/miso/blob/master/jsbits/diff.js). It is responsible for all DOM manipulation that occurs in a miso application and has [100% code coverage](http://coverage.haskell-miso.org). Tests and coverage made possible using [jsdom](https://github.com/jsdom/jsdom) and [jest](https://github.com/facebook/jest).

To run the tests and build the coverage report:

```bash
cd miso/tests
npm i
npm run test
## Or by using `yarn` instead of `npm`:
# yarn
# yarn test
```

## Isomorphic

[Isomorphic javascript](https://en.wikipedia.org/wiki/Isomorphic_JavaScript) is a technique for increased SEO, code-sharing and perceived page load times. It works in two parts. First, the server sends a pre-rendered HTML body to the client's browser. Second, after the client javascript application loads, the pointers of the pre-rendered DOM are copied into the virtual DOM, and the application proceeds as normal. All subsequent page navigation is handled locally by the client, avoiding full-page postbacks as necessary.

The `miso` function is used to perform the pointer-copying behavior client-side.

For more information on how `miso` handles isomorphic javascript, we recommend [this tutorial](https://github.com/FPtje/miso-isomorphic-example).

## Pinning nixpkgs

By default `miso` uses a known-to-work, pinned version of [`nixpkgs`](https://github.com/dmjio/miso/blob/master/nixpkgs.json).

## Binary cache

`nix` users on a Linux or OSX distro can take advantage of a [binary cache](https://miso-haskell.cachix.org) for faster builds. To use the binary cache follow the instructions on [cachix](https://miso-haskell.cachix.org/).

```bash
cachix use miso-haskell
```

## Benchmarks

[According to benchmarks](https://krausest.github.io/js-framework-benchmark/current.html), `miso` is among the fastest functional programming web frameworks, second only to [Elm](http://elm-lang.org).

<a target="_blank" href="https://krausest.github.io/js-framework-benchmark/current.html"><img src="https://cdn-images-1.medium.com/max/1600/1*6EjJTf1mhlTxd4QWsygCwA.png" width="500" height="600" /></a>

## Maintainers

[@dmjio](https://github.com/dmjio)

## Commercial Users
  - [Polimorphic](https://www.polimorphic.com)
  - [LumiGuide](https://lumi.guide/en/)
  - [Clovyr](https://clovyr.io)

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
