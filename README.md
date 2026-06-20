<h1 align="center">miso</h1>

<p align="center">

<a href="https://haskell-miso.org">
  <img width=10% src="https://em-content.zobj.net/thumbs/240/apple/325/steaming-bowl_1f35c.png">

   </a>
<p align="center">A <a href="https://www.haskell.org/"><strong>Haskell</strong></a> library for building web and <a href="https://github.com/haskell-miso/miso-lynx">mobile</a> applications</p>
</p>

<p align="center"> 
  <a href="https://matrix.to/#/#haskell-miso:matrix.org">
    <img src="https://img.shields.io/badge/matrix.org-miso-FF4B33.svg?style=for-the-badge" alt="Matrix #haskell-miso:matrix.org">
  </a>
  <a href="https://www.npmjs.com/package/haskell-miso">  
    <img src="https://img.shields.io/npm/v/haskell-miso?style=for-the-badge" />
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

**miso** is a [Haskell](https://www.haskell.org/) library for building web and [mobile](https://github.com/dmjio/miso-native) applications. It is inspired by both [Elm](http://elm-lang.org/) and [React](http://react.dev/). Try it [live](https://try.haskell-miso.org). See the [org](https://github.com/haskell-miso). Read the [docs](https://haddocks.haskell-miso.org/miso/Miso.html).

## Key features

- [Virtual DOM](https://en.wikipedia.org/wiki/Virtual_DOM) with recursive diffing and patching algorithm
- Attribute and property normalization, event delegation, and event batching
- Model-View-Update paradigm
- Pure by default 
- SVG, 2D Canvas, and WebGL (via [three.js](https://threejs.org))
- [Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API), [Server-Sent Events](https://github.com/haskell-miso/miso-sse), and [WebSocket](https://github.com/haskell-miso/miso-websocket) support
- Type-safe client-side routing
- An extensible subscription system for long-running effects and third-party library integration
- Lifecycle hooks (`onCreated`, `onDestroyed`, `mount`, `unmount`)
- [Reactive](https://github.com/haskell-miso/miso-reactive) extensions for fine-grained reactivity
- [Component](https://react.dev/reference/react/Component), [Fragment](https://react.dev/reference/react/Fragment) and [Props](https://react.dev/learn/passing-props-to-a-component) features.

It makes heavy use of the [GHC JavaScript FFI](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#javascript-ffi-in-the-wasm-backend) and maintains minimal dependencies. It can be considered a shallow [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language) for modern web programming. Compilation targets include [JavaScript](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html) and [WebAssembly](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html) via [GHC](https://www.haskell.org/ghc/). Hot reload is provided through [WASM browser mode](https://www.tweag.io/blog/2025-04-17-wasm-ghci-browser/) integrated with [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch).

> [!TIP]
> See the [Haskell miso organization](https://github.com/haskell-miso) on GitHub for the full ecosystem of packages and examples 🍜

## Table of Contents
- [Playground](#playground-)
- [Quick Start (Nix)](#quick-start-nix-)
- [Manual Setup (GHCup / Cabal)](#manual-setup-ghcup--cabal)
  - [cabal.project](#cabalproject)
  - [app.cabal](#appcabal)
  - [Main.hs](#mainhs)
- [Hot Reload](#hot-reload-)
- [Installation](#installation)
- [Haddocks](#haddocks)
- [Wiki](#wiki)
- [Architecture](#architecture)
- [Examples](#examples)
- [HTTP](#interacting-with-http-apis-)
- [Testing](#testing-)
- [Native](#native-)
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
- [History](#history-)
- [License](#license)

## Playground 🛝

An interactive playground is available at [try.haskell-miso.org](https://try.haskell-miso.org). It allows editing and running applications directly in the browser without any local toolchain setup, and is useful for experimentation and sharing minimal reproducible examples.

## Quick Start (Nix) ⚡

> [!TIP]
> The [miso-sampler](https://github.com/haskell-miso/miso-sampler) template repository includes a counter application with build scripts for WebAssembly, JavaScript, and native GHC targets.

The following requires [Nix Flakes](https://wiki.nixos.org/wiki/Flakes). See also [Binary cache](#binary-cache) to avoid rebuilding dependencies.

```bash
# Install nix 
curl -L https://nixos.org/nix/install | sh

# Enable flakes
echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf

# Clone, build and serve
git clone https://github.com/haskell-miso/miso-sampler && cd miso-sampler
nix develop .#wasm --command bash -c 'make && make serve'
```

## Manual Setup (GHCup / Cabal)

To develop applications without Nix, acquire [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/) via [GHCup](https://www.haskell.org/ghcup/).

> [!TIP]
> For users new to Haskell tooling, [GHCup](https://www.haskell.org/ghcup/) is the recommended way to install both [GHC](https://www.haskell.org/ghc/) and [cabal](https://www.haskell.org/cabal/).

A minimal application requires three files:

  - `cabal.project`
  - `app.cabal`
  - `Main.hs`

### `cabal.project`

```cabal
packages:
  .

source-repository-package
  type: git
  location: https://github.com/dmjio/miso
  branch: master
```

> [!NOTE]
> Pinning to a specific `tag:` or `commit:` rather than `branch: master` is recommended for reproducible builds.

### `app.cabal`

Using `cabal-version: 2.2` or later enables [common stanzas](https://vrom911.github.io/blog/common-stanzas), which allow a single `.cabal` file to target both the WASM and JS backends.

```cabal
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

A counter application demonstrating the Model-View-Update pattern:

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
import           Miso.Lens
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
main = startApp defaultEvents app
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | `vcomp` takes as arguments the initial model, update function, view function
app :: App Int Action
app = vcomp 0 updateModel viewModel
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent props Int Action
updateModel = \case
  AddOne -> this += 1
  SubtractOne -> this -= 1
  SayHelloWorld -> io_ $ do
    alert "Hello World"
    consoleLog "Hello World"
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: () -> Int -> View Int Action
viewModel _props x = vfrag
    [ H.button_ [ H.onClick AddOne ] [ text "+" ]
    , text (ms x)
    , H.button_ [ H.onClick SubtractOne ] [ text "-" ]
    , H.br_ []
    , H.button_ [ H.onClick SayHelloWorld ] [ text "Alert Hello World!" ]
    ]
----------------------------------------------------------------------------
```

## Hot Reload 🔥

Hot reload is supported via [WASM browser mode](https://www.tweag.io/blog/2025-04-17-wasm-ghci-browser/) and [ghciwatch](https://github.com/MercuryTechnologies/ghciwatch). This provides incremental recompilation with automatic browser refresh on file changes. See the [miso-sampler browser mode documentation](https://github.com/haskell-miso/miso-sampler/blob/main/README.md#browser-mode-) for setup instructions.

## Installation

See [Installation](docs/Install.md) for platform-specific installation instructions.

## Haddocks

Official API reference. See also the [Miso](https://haddocks.haskell-miso.org/miso/Miso.html) module for a guided entry point into the library.

| Platform | URL |
|------|-------------|
| GHCJS | [Link](https://haddocks.haskell-miso.org/) |
| GHC | [Link](http://hackage.haskell.org/package/miso) |

## Wiki

See the [DeepWiki](https://deepwiki.com/dmjio/miso) entry for an AI-assisted exploration of the source code.

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/dmjio/miso)

## Architecture

**miso** follows the [Model-View-Update](https://guide.elm-lang.org/architecture/) (MVU) pattern. A `Component` is parameterized by a `model` type and an `action` type. The `update` function maps actions to `Effect` values — a monad over the Reader/Writer/State stack — which can both modify the model and schedule `IO` operations. Long-running effects are expressed as `Sub`scriptions that push actions into the component via a `Sink`.

For (client/server) applications, the recommended layout is a single `.cabal` file with separate executable stanzas conditioned on the compiler target. An example of this structure is the [haskell-miso.org source](https://github.com/haskell-miso/haskell-miso.org/blob/master/haskell-miso.cabal).

> [!TIP]
> For a worked example of a Nix-based client/server deployment, see the [nix scripts](https://github.com/haskell-miso/haskell-miso.org/blob/master/default.nix) for [haskell-miso.org](https://haskell-miso.org).

## Examples

Examples are hosted under the [haskell-miso](https://github.com/haskell-miso) GitHub organization. Each repository contains its own build instructions. The recommended approach is to build via [`nix`](https://nixos.org/nix/).

> [!TIP]
> Use [cachix](https://cachix.org) to avoid rebuilding shared dependencies: `cachix use haskell-miso-cachix`

| Name                  | Description                               | Source                                                        | Demo                                                      | Author                                            |
|-----------------------|-------------------------------------------|---------------------------------------------------------------|-----------------------------------------------------------|---------------------------------------------------|
| **TodoMVC**           | TodoMVC reference implementation          | [Source](https://github.com/haskell-miso/miso-todomvc)        | [Demo](https://todomvc.haskell-miso.org)       | [@dmjio](https://github.com/dmjio)                |
| **2048**              | Clone of the 2048 sliding-tile game       | [Source](https://github.com/haskell-miso/miso-2048)           | [Demo](https://2048.haskell-miso.org/)         | [@ptigwe](https://github.com/ptigwe)              |
| **Flatris**           | Tetris variant                            | [Source](https://github.com/haskell-miso/miso-flatris)        | [Demo](https://flatris.haskell-miso.org/)      | [@ptigwe](https://github.com/ptigwe)              |
| **Plane**             | Flappy-bird-style game                    | [Source](https://github.com/haskell-miso/miso-plane)          | [Demo](https://plane.haskell-miso.org/)        | [@Lermex](https://github.com/Lermex)              |
| **Snake**             | Classic Snake game                        | [Source](https://github.com/haskell-miso/miso-snake)          | [Demo](https://snake.haskell-miso.org/)        | [@lbonn](https://github.com/lbonn)                |
| **SVG**               | SVG rendering                             | [Source](https://github.com/haskell-miso/miso-svg)            | [Demo](https://svg.haskell-miso.org/)          | [@dmjio](https://github.com/dmjio)                |
| **Fetch**             | HTTP API interaction via Fetch            | [Source](https://github.com/haskell-miso/miso-fetch)          | [Demo](https://fetch.haskell-miso.org)         | [@dmjio](https://github.com/dmjio)                |
| **File Reader**       | FileReader API                            | [Source](https://github.com/haskell-miso/miso-filereader)     | [Demo](https://file-reader.haskell-miso.org/)  | [@dmjio](https://github.com/dmjio)                |
| **Mario**             | Physics-based platformer                  | [Source](https://github.com/haskell-miso/miso-mario)          | [Demo](https://mario.haskell-miso.org)         | [@dmjio](https://github.com/dmjio)                |
| **WebSocket**         | WebSocket communication                   | [Source](https://github.com/haskell-miso/miso-websocket)      | [Demo](https://websocket.haskell-miso.org)     | [@dmjio](https://github.com/dmjio)                |
| **Router**            | Client-side routing                       | [Source](https://github.com/haskell-miso/miso-router)         | [Demo](https://router.haskell-miso.org)        | [@dmjio](https://github.com/dmjio)                |
| **Canvas 2D**         | 2D Canvas rendering                       | [Source](https://github.com/haskell-miso/miso-canvas2d)       | [Demo](https://canvas.haskell-miso.org)        | [@dmjio](https://github.com/dmjio)                |
| **MathML**            | MathML rendering                          | [Source](https://github.com/haskell-miso/miso-mathml)         | [Demo](https://mathml.haskell-miso.org)        | [@dmjio](https://github.com/dmjio)                |
| **Simple**            | Counter (minimal example)                 | [Source](https://github.com/haskell-miso/miso-sampler)        | [Demo](https://counter.haskell-miso.org)       | [@dmjio](https://github.com/dmjio)                |
| **SSE**               | Server-Sent Events                        | [Source](https://github.com/haskell-miso/miso-sse)            | [Demo](https://sse.haskell-miso.org)           | [@dmjio](https://github.com/dmjio)                |
| **Three.js**          | 3D rendering via Three.js                 | [Source](https://github.com/haskell-miso/three-miso)          | [Demo](https://threejs.haskell-miso.org/)      | [@juliendehos](https://github.com/juliendehos)    |
| **Space Invaders**    | Space Invaders clone                      | [Source](https://github.com/haskell-miso/miso-invaders)       | [Demo](https://space-invaders.haskell-miso.org/) | [@juliendehos](https://github.com/juliendehos)    |
| **Audio**             | Audio playback                            | [Source](https://github.com/haskell-miso/miso-audio)          | [Demo](https://audio.haskell-miso.org/)        | [@juliendehos](https://github.com/juliendehos)    |
| **Video**             | Video playback                            | [Source](https://github.com/haskell-miso/miso-video)          | [Demo](https://video.haskell-miso.org/)        | [@juliendehos](https://github.com/juliendehos)    |
| **WebVR**             | WebVR via A-Frame                         | [Source](https://github.com/haskell-miso/miso-aframe)         | [Demo](https://aframe.haskell-miso.org/)       | [@dmjio](https://github.com/dmjio)                |
| **Reactivity**        | Fine-grained reactive updates             | [Source](https://github.com/haskell-miso/miso-reactive)       | [Demo](https://reactive.haskell-miso.org/)     | [@dmjio](https://github.com/dmjio)                |
| **Chess**             | Chess game                                | [Source](https://github.com/haskell-miso/chess)               | [Demo](https://chess.haskell-miso.org)         | [@dmjio](https://github.com/dmjio)                |

## Interacting with HTTP APIs 🔌

Two approaches are supported:

  1. For simple JSON-based APIs, use the [Fetch](https://haddocks.haskell-miso.org/miso/Miso-Fetch.html) module directly.

  2. For more complex cases, define a [Servant](https://www.servant.dev/) API and derive client functions via [servant-miso-client](https://github.com/haskell-miso/servant-miso-client).

     The [Fetch example](https://github.com/haskell-miso/miso-fetch) ([Demo](https://fetch.haskell-miso.org/)) demonstrates the required setup. Add the following to `cabal.project` to use `servant-miso-client`:

     ```
     source-repository-package
       type: git
       location: https://github.com/haskell-miso/servant-miso-client
       tag: master
     ```

## Testing ✅

The test suite spans three layers:

- **Unit tests** — the TypeScript runtime (virtual DOM, diffing, event delegation) is tested with [bun](https://github.com/oven-sh/bun), covering the core `diff` engine and supporting utilities.
- **Integration tests** — Haskell internals are exercised via a WASM test suite that runs the runtime in a headless browser environment, verifying component lifecycle, subscriptions, and state transitions.
- **End-to-end tests** — selected applications such as [TodoMVC](https://github.com/haskell-miso/miso-todomvc) are tested end-to-end against a live browser to validate full-stack rendering and event handling.

A full coverage report for the TypeScript layer is available at [coverage.haskell-miso.org](http://coverage.haskell-miso.org).

> [!NOTE]
> To run the TypeScript tests, install [bun](https://github.com/oven-sh/bun) first.

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

## Native 📱

iOS and Android applications are supported via [LynxJS](https://lynxjs.org). See the [miso-lynx](https://github.com/haskell-miso/miso-lynx) repository for details.

## Benchmarks 🏎️

[According to benchmarks](https://krausest.github.io/js-framework-benchmark/current.html), `miso` performs competitively relative to other frameworks.

## Nix <img src="https://raw.githubusercontent.com/NixOS/nixos-artwork/refs/heads/master/logo/nix-snowflake-colours.svg" alt="nixos-snowflake" width="25"/>

`Nix` provides a reproducible environment for building, configuring, and deploying applications. The [haskell-miso.org](https://github.com/dmjio/miso/tree/master/haskell-miso.org) source serves as a reference for this workflow.

### Pinning nixpkgs 📌

By default, `miso` uses a pinned version of [`nixpkgs`](https://github.com/dmjio/miso/blob/master/nix/nixpkgs.json) known as `pkgs`.

> [!NOTE]
> `miso` also maintains a `legacyPkgs` nixpkgs pin for tools such as `nixops` and for builds using the original `GHCJS 8.6` backend.

### Binary cache

Linux and macOS users can use a [binary cache](https://haskell-miso-cachix.cachix.org) to avoid rebuilding dependencies. Follow the setup instructions on [cachix](https://haskell-miso-cachix.cachix.org/).

```bash
$ cachix use haskell-miso-cachix
```

For CI pipelines using GitHub Actions:

```yaml
- name: Install cachix
  uses: cachix/cachix-action@v16
  with:
    name: haskell-miso-cachix
```

## Community :octocat:

- [Github](https://github.com/haskell-miso)
- [Matrix](https://matrix.to/#/#haskell-miso:matrix.org)
- [Discord](https://discord.gg/QVDtfYNSxq)

## Maintainers

[@dmjio](https://github.com/dmjio)

## Commercial 🚀

Since its launch, `miso` has been deployed across a range of domains, including quantitative finance, network security, defense research, academia, SaaS, the public sector, and non-profit organizations. The largest known deployment consisted of approximately 200,000 LOC serving over 10,000 users.

## Contributing

Contributions are welcome. [Open an issue](https://github.com/dmjio/miso/issues/new) or submit a [pull request](https://github.com/dmjio/miso/pulls).

See [CONTRIBUTING](https://github.com/dmjio/miso/blob/master/CONTRIBUTING.md) for guidelines.

## Contributors 🦾

> [!NOTE]
> This project exists thanks to all the people who [contribute](CONTRIBUTING.md).

<a href="https://github.com/dmjio/miso/graphs/contributors"><img src="https://opencollective.com/miso/contributors.svg?width=890&button=false" /></a>

## Partnerships 🤝

For inquiries regarding feature sponsorship or corporate partnerships, contact <a href="mailto:support@haskell-miso.org">support@haskell-miso.org</a>.

## Backers

Become a [financial contributor](https://opencollective.com/miso/contribute) to help sustain the project.

<a href="https://opencollective.com/miso"><img src="https://opencollective.com/miso/individuals.svg?width=890"></a>

## organizations

[Support this project](https://opencollective.com/miso/contribute) with your organization. Your logo will appear here with a link to your website.

<a target="_blank" href="https://opencollective.com/miso/organization/0/website"><img src="https://opencollective.com/miso/organization/0/avatar.svg"></a>

## History 📜

> **miso** is a portmanteau of ***micro*** and ***isomorphic***.

[miso](https://haskell-miso.org) was initiated in 2016 as a research project exploring two directions:

- Expressing the [Elm architecture](https://elm-lang.org) in [GHCJS](https://github.com/ghcjs/ghcjs) as an [embedded domain-specific language](https://wiki.haskell.org/Embedded_domain_specific_language)
- Implementing reconciliation and isomorphic rendering techniques from the JavaScript ecosystem ([Reconciliation](https://legacy.reactjs.org/docs/reconciliation.html#the-diffing-algorithm), [isomorphic JavaScript](https://en.wikipedia.org/wiki/Isomorphic_JavaScript)) within a purely functional setting

The project addresses the [JavaScript problem](https://wiki.haskell.org/The_JavaScript_Problem) in Haskell by providing component abstractions and rendering primitives familiar to practitioners of frameworks such as [React](https://reactjs.org) and [Vue.js](https://vuejs.org). The library has since expanded to include multiple rendering backends and native mobile support for [iOS](https://www.apple.com/ios/), [Android](https://www.android.com/), and [HarmonyOS](https://device.harmonyos.com/en/) via [LynxJS](https://lynxjs.org).

## License

[BSD3](LICENSE) © dmjio
