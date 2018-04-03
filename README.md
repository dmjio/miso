<h1 align="center">miso</h1>
<p align="center">
<a href="https://haskell-miso.org">
  <img width=10% src="https://emojipedia-us.s3.amazonaws.com/thumbs/240/apple/96/steaming-bowl_1f35c.png">
   </a>
<p align="center">A <i>tasty</i> <a href="https://www.haskell.org/"><strong>Haskell</strong></a> front-end framework</p>
</p>

<p align="center">
  <a href="https://haskell-miso-slack.herokuapp.com">
	<img src="https://img.shields.io/badge/slack-miso-E01563.svg?style=flat-square" alt="Miso Slack">
  </a>
  <a href="http://hackage.haskell.org/package/miso">
	<img src="https://img.shields.io/hackage/v/miso.svg?style=flat-square" alt="Hackage">
  </a>
  <a href="https://haskell.org">
	<img src="https://img.shields.io/badge/language-Haskell-green.svg?style=flat-square" alt="Haskell">
  </a>
  <a href="https://github.com/dmjio/miso/blob/master/LICENSE">
	<img src="http://img.shields.io/badge/license-BSD3-brightgreen.svg?style=flat-square" alt="LICENSE">
  </a>
  <a href="https://hydra.dmj.io">
	<img src="https://img.shields.io/badge/build-Hydra-00BDFD.svg?style=flat-square" alt="Miso Hydra">
  </a>
  <a href="https://www.irccloud.com/invite?channel=%23haskell-miso&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1">
	<img src="https://img.shields.io/badge/irc-%23haskell--miso-1e72ff.svg?style=flat-square" alt="IRC #haskell-miso">
  </a>
</p>

<p align="center">
  <a href="https://saucelabs.com/u/dmjio">
    <img src="https://saucelabs.com/browser-matrix/dmjio.svg" alt="Sauce Test Status"/>
  </a>
</p>

**Miso** is a small "[isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/)" [Haskell](https://www.haskell.org/) front-end framework for quickly building highly interactive single-page web applications. It features a virtual-dom, diffing / patching algorithm, attribute and property normalization, event delegation, event batching, SVG, Server-sent events, Websockets, type-safe [servant](https://haskell-servant.github.io/)-style routing and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril). **Miso** is pure by default, but side effects (like `XHR`) can be introduced into the system via the `Effect` data type. **Miso** makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies. **Miso** can be considered a shallow [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language) for modern web programming.

## Table of Contents
- [Quick Start](#quick-start)
  - [Begin](#begin)
  - [Stack](#stack)
  - [Nix](#nix)
  - [Cabal](#cabal)
  - [GHCJSi Caveats](#ghcjsi-caveats)
  - [Architecture](#architecture)
- [Examples](#examples)
  - [TodoMVC](#todomvc)
  - [Flatris](#flatris)
  - [2048](#2048)
  - [Snake](#snake)
  - [Mario](#mario)
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
- [Haddocks](#haddocks)
  - [GHC](#ghc)
  - [GHCJS](#ghcjs)
- [Sample Application](#sample-application)
- [Building examples](#building-examples)
- [Coverage](#coverage)
- [Isomorphic](#isomorphic)
- [Pinning nixpkgs](#pinning-nixpkgs)
- [Binary cache](#binary-cache)
- [Benchmarks](#benchmarks)
- [Maintainers](#maintainers)
- [Contributing](#contributing)
- [License](#license)

## Quick start
To get started quickly building applications, we recommend using the [`stack`](https://docs.haskellstack.org/en/stable/README/) or [`nix`](https://nixos.org/nix) package managers. Obtaining [`GHCJS`](https://github.com/ghcjs/ghcjs) is required as a prerequisite. `stack` and `nix` make this process easy, if you're using `cabal` we assume you have [obtained `GHCJS`](https://github.com/ghcjs/ghcjs#installation) by other means.

All source code depicted below for the quick start app is available [here](https://github.com/dmjio/miso/tree/master/sample-app).

### Begin
We recommend using `nix` when working with `miso`, but it is just as fine to use `stack`.
To build the sample-app with `nix`, execute the command below:

```bash
git clone https://github.com/dmjio/miso && cd miso/sample-app && nix-build
```

To develop with `nix` run the below command (this will put you into a shell where you can iteratively develop the project with `cabal build`).

```bash
git clone https://github.com/dmjio/miso && cd miso/sample-app && nix-shell -A env
```

For more information on using `nix` w/ `miso`, see the [`nix` section below](#nix)

To build the sample-app with `stack`, execute the command below:
```bash
git clone https://github.com/dmjio/miso && cd miso/sample-app && stack setup && stack build
```

Note: It's important to ensure that you don't have a global `cabal-install` present on your system. This could cause build problems. If you see an error like this (below), try deleting your global `cabal-install`.

```bash
exit status: 1
stderr: solver must be one of: modular
CallStack (from HasCallStack):
  error, called at libraries/Cabal/Cabal/Distribution/ReadE.hs:46:24 in Cabal-2.0.1.0:Distribution.ReadE
```


For more information on using `stack` w/ `miso`, see the [`stack` section below](#stack)


### Stack
In the `miso` repository there is a [folder named `stack`](https://github.com/dmjio/miso/tree/master/stack) with "known to work" configurations for `GHCJS`. One stack file exists for both the `7.10.3` and `8.0.1` versions of `GHCJS`. In general, we recommend developing with the `7.10.3` version since it currently supports `GHCJSi` (a REPL that connects to the browser by way of a [`nodejs`](https://nodejs.org/en/) web server using [`socket.io`](https://socket.io/)) and building with the `8.0.1` version (if possible). For more information on using `stack` with `GHCJS`, please consult the [GHCJS section of the `stack` docs](https://docs.haskellstack.org/en/stable/ghcjs/).

To begin, create the following directory layout
```bash
➜  mkdir app && touch app/{Main.hs,app.cabal,stack.yaml} && tree app
app
|-- Main.hs
|-- app.cabal
`-- stack.yaml
```

Add a `stack.yaml` file that uses a recent version of `miso`.
```bash
➜  cat app/stack.yaml
resolver: lts-6.30
compiler: ghcjs-0.2.0.9006030_ghc-7.10.3
compiler-check: match-exact

packages:
 - '.'
extra-deps:
 - miso-0.12.0.0

setup-info:
  ghcjs:
	source:
	  ghcjs-0.2.0.9006030_ghc-7.10.3:
		 url: http://ghcjs.tolysz.org/lts-6.30-9006030.tar.gz
		 sha1: 2371e2ffe9e8781808b7a04313e6a0065b64ee51
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
  build-depends:       base, miso
  default-language:    Haskell2010
```

Add the source from [Sample Application](#sample-application) to `app/Main.hs`

Run `stack setup`. This might take a long time, since it will have to build `GHCJS`.
```
stack setup
```

Run `stack build` to get the static assets
```
stack build
```

See the result
```
open $(stack path --local-install-root)/bin/app.jsexe/index.html
```

Using GHCJSi
```
stack ghci
```

If that warns with `socket.io not found, browser session not available`, you'll need to install `socket.io`
```
npm install socket.io
```

and update your `NODE_PATH`
```
export NODE_PATH=$(pwd)/node_modules
```

Now you should be connected, and the app viewable in `GHCJSi` (open http://localhost:6400).
```bash
➜  stack ghci
app-0.1.0.0: initial-build-steps (exe)
Configuring GHCi with the following packages: app
GHCJSi, version 0.2.0.9006020-7.10.3: http://www.github.com/ghcjs/ghcjs/  :? for help
[1 of 1] Compiling Main             ( /Users/david/Desktop/miso/sample-app/Main.hs, interpreted )
socket.io found, browser session available at http://localhost:6400
Ok, modules loaded: Main.
*Main> main
browser connected, code runs in browser from now on
```

### Nix
`Nix` is a more powerful option for building web applications with `miso` since it encompasses development workflow, configuration management, and deployment. The source code for [`haskell-miso.org`](https://github.com/dmjio/miso/tree/master/examples/haskell-miso.org) is an example of this.

If unfamiliar with `nix`, we recommend [@Gabriel439](https://github.com/Gabriel439)'s ["Nix and Haskell in production"](https://github.com/Gabriel439/haskell-nix) guide.

To get started, we will use the [`cabal2nix`](https://github.com/NixOS/cabal2nix) tool to convert our `Cabal` file into a `nix` derivation (named `app.nix`). We'll then write a file named `default.nix`, which is used for building our project (via `nix-build`) and development (via `nix-shell`).

To begin, make the following directory layout:
```bash
➜  mkdir app && touch app/{Main.hs,app.cabal,default.nix,app.nix} && tree app
app
|-- Main.hs
|-- app.cabal
|-- default.nix
`-- app.nix
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
  build-depends:       base, miso
  default-language:    Haskell2010
```

Use [`cabal2nix`](https://github.com/NixOS/cabal2nix) to generate a file named `app.nix`
that looks like below.
```bash
➜  cabal2nix . --compiler ghcjs > app.nix
➜  cat app.nix
```

```nix
{ mkDerivation, base, miso, stdenv }:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso ];
  description = "First miso app";
  license = stdenv.lib.licenses.bsd3;
}
```

Write a `default.nix` (which calls `app.nix`), this fetches a recent version of `miso`.
```nix
{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
	owner = "NixOS";
	repo = "nixpkgs";
	rev = "a0aeb23";
	sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  result = import (pkgs.fetchFromGitHub {
	owner = "dmjio";
	repo = "miso";
	sha256 = "1l1gwzzqlvvcmg70jjrwc5ijv1vb6y5ljqkh7rxxq7hkyxpjyx9q";
	rev = "95f6bc9b1ae6230b110358a82b6a573806f272c2";
  }) {};
in pkgs.haskell.packages.ghcjs.callPackage ./app.nix {
  miso = result.miso-ghcjs;
}
```

Build the project
```
nix-build
```

Open the result
```
open ./result/bin/app.jsexe/index.html
```

For development with `nix`, it's important to have `cabal` present for building. This command will make it available in your `PATH`.
```
nix-env -iA cabal-install -f '<nixpkgs>'
```

To be put into a shell w/ `GHCJS` and all the dependencies for this project present, use `nix-shell`.
```
nix-shell -A env
```

To open `GHCJSi` (`NODE_PATH` should already be set properly)
```
$ cabal configure --ghcjs
$ cabal repl
Package has never been configured. Configuring with default flags. If this
fails, please run configure manually.
Resolving dependencies...
Configuring app-0.1.0.0...
Preprocessing executable 'app' for app-0.1.0.0...
GHCJSi, version 0.2.0-7.10.3: http://www.github.com/ghcjs/ghcjs/  :? for help
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main>
browser connected, code runs in browser from now on
```

### Cabal
The latest stable version of `miso` will be available on Hackage.
To build with cabal, we assume `ghcjs` is in your `PATH` and `ghcjs-base` is present in your `ghcjs-pkg` list.
```bash
cabal sandbox init
cabal install --ghcjs
cabal build
open dist/build/app/app.jsexe/index.html
```

### GHCJSi Caveats
If you run `main` in `GHCJSi`, interrupt it and then run it again, you
will end up with two copies of your app displayed above each other. As
a workaround, you can use `clearBody >> main` which will completely
clear the document body before rendering your application.

### Architecture
For constructing client and server applications, we recommend using one `cabal` file with two executable sections, where the `buildable` attribute set is contingent on the compiler. An example of this layout is [here](https://github.com/dmjio/miso/blob/master/examples/haskell-miso.org/haskell-miso.cabal#L16-L60). For more info on how to use `stack` with a `client`/`server` setup, see this [link](https://docs.haskellstack.org/en/stable/ghcjs/#project-with-both-client-and-server). For more information on how to use `nix` with a `client`/`server` setup, see the [nix scripts](https://github.com/dmjio/miso/blob/master/examples/haskell-miso.org/default.nix) for [https://haskell-miso.org](https://haskell-miso.org).

## Examples

### TodoMVC
  - [Link](https://todo-mvc.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs)

### Flatris
  - [Link](https://flatris.haskell-miso.org/) / [Source](https://github.com/ptigwe/hs-flatris/)

### 2048
  - [Link](http://2048.haskell-miso.org/) / [Source](https://github.com/ptigwe/hs2048/)

### Snake
  - [Link](http://snake.haskell-miso.org/) / [Source](https://github.com/lbonn/miso-snake)

### Mario
  - [Link](https://mario.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/mario/Main.hs)

### Websocket
  - [Link](https://websocket.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/websocket/Main.hs)

### SSE
  - [Link](https://sse.haskell-miso.org/) / [Client](https://github.com/dmjio/miso/blob/master/examples/sse/client/Main.hs) / [Server](https://github.com/dmjio/miso/blob/master/examples/sse/server/Main.hs)

### XHR
  - [Link](https://xhr.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/xhr/Main.hs)

### Router
  - [Link](https://router.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/router/Main.hs)

### SVG
  - [Link](https://svg.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/svg/Main.hs)

### Canvas 2D
  - [Link](http://canvas.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/canvas2d/Main.hs)

### ThreeJS
  - [Link](http://threejs.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/three/Main.hs)

### Simple
  - [Link](https://simple.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/exe/Main.hs)

### File Reader
  - [Link](https://file-reader.haskell-miso.org/) / [Source](https://github.com/dmjio/miso/blob/master/examples/file-reader/Main.hs)

### WebVR
  - [Link](https://fizruk.github.io/fpconf-2017-talk/miso-aframe-demo/dist/demo.jsexe/index.html) / [Source](https://github.com/fizruk/miso-aframe)

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
	model         = 0             -- initial model
	update        = updateModel   -- update function
	view          = viewModel     -- view function
	events        = defaultEvents -- default delegated events
	subs          = []            -- empty subscription list
	mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  putStrLn "Hello World" >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
```

## Building examples

The easiest way to build the examples is with the [`nix`](https://nixos.org/nix/) package manager
```
git clone https://github.com/dmjio/miso && cd miso && nix-build
```

This will build all examples and documentation into a folder named `result`
```
➜  miso git:(master) ✗ tree result -d
result
|-- doc
|   |-- x86_64-osx-ghc-8.0.2
|   |   `-- miso-0.2.0.0
|   |       `-- html
|   |           `-- src
|   `-- x86_64-osx-ghcjs-0.2.0-ghc7_10_3
|       `-- miso-0.2.0.0
|           `-- html
|               `-- src
|-- examples
|   |-- mario.jsexe
|   |   `-- imgs
|   |       |-- jump
|   |       |-- stand
|   |       `-- walk
|   |-- router.jsexe
|   |-- simple.jsexe
|   |-- tests.jsexe
|   |-- todo-mvc.jsexe
|   `-- websocket.jsexe
```

To see examples, we recommend hosting them with a webserver

```
cd result/examples/todo-mvc.jsexe && python -m SimpleHTTPServer
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

Isomorphic javascript is a technique for increased SEO, code-sharing and perceived page load times. It works in two parts. First, the server sends a pre-rendered HTML body to the client's browser. Second, after the client javascript application loads, the pointers of the pre-rendered DOM are copied into the virtual DOM, and the application proceeds as normal. All subsequent page navigation is handled locally by the client, avoiding full-page postbacks as necessary.

The `miso` function is used to perform the pointer-copying behavior client-side.

For more information on how `miso` handles isomorphic javascript, we recommend [this tutorial](https://github.com/FPtje/miso-isomorphic-example).

## Pinning nixpkgs

By default `miso` uses a known-to-work, pinned version of [`nixpkgs`](https://github.com/dmjio/miso/blob/master/default.nix#L1-L6).
To override this to your system's version of `nixpkgs` write:

```
nix-build --arg nixpkgs 'import <nixpkgs> {}'
```

## Binary cache

`nix` users on a Linux distro can take advantage of a [binary cache](https://hydra.dmj.io/nix-cache-info) for faster builds. To use the binary cache simply append `https://hydra.dmj.io/nix-cache-info` during all `nix-shell` and/or `nix-build` invocations.

```bash
nix-build --option extra-binary-caches https://hydra.dmj.io
```

Alternatively, add `https://hydra.dmj.io` to your list of local binary caches in `nix.conf` (usually found in `/etc/nix/nix.conf`), and it will automatically be used on all invocations of `nix-build` and/or `nix-shell`.

```
binary-caches = https://hydra.dmj.io/ https://cache.nixos.org/
```

## Benchmarks

[According to benchmarks](https://medium.com/@saurabhnanda/benchmarks-fp-languages-libraries-for-front-end-development-a11af0542f7e), `miso` is among the fastest functional programming web frameworks, second only to [Elm](http://elm-lang.org).

<img src="https://cdn-images-1.medium.com/max/1600/1*6EjJTf1mhlTxd4QWsygCwA.png" width="500" height="600" />

## Maintainers

[@dmjio](https://github.com/dmjio)

## Contributing

Feel free to dive in! [Open an issue](https://github.com/dmjio/miso/issues/new) or submit [PRs](https://github.com/dmjio/miso/pulls).

See [CONTRIBUTING](https://github.com/dmjio/miso/blob/master/CONTRIBUTING.md) for more info.

## License

[BSD3](LICENSE) © David Johnson
