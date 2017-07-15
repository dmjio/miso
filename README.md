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
  <a href="https://img.shields.io/hackage-deps/v/miso.svg">
    <img src="https://img.shields.io/hackage-deps/v/miso.svg?style=flat-square" alt="Hackage">
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

**Miso** is a small "[isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/)" [Haskell](https://www.haskell.org/) front-end framework featuring a virtual-dom, diffing / patching algorithm, event delegation, event batching, SVG, Server-sent events, Websockets, type-safe [servant](https://haskell-servant.github.io/)-style routing and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril). **Miso** is pure by default, but side effects (like `XHR`) can be introduced into the system via the `Effect` data type. **Miso** makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies.

## Table of Contents
- [Examples](#examples)
- [Haddocks](#haddocks)
- [Sample Application](#sample-application)
- [Building examples](#building-examples)

## Examples
  - TodoMVC
    - [Link](https://todo-mvc.haskell-miso.org/)
    - [Source](https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs)
  - Mario
    - [Link](https://mario.haskell-miso.org/)
    - [Source](https://github.com/dmjio/miso/blob/master/examples/mario/Main.hs)
 - Websocket
    - [Link](https://websocket.haskell-miso.org/)
    - [Source](https://github.com/dmjio/miso/blob/master/examples/websocket/Main.hs)
 - Router
    - [Link](https://router.haskell-miso.org/)
    - [Source](https://github.com/dmjio/miso/blob/master/examples/router/Main.hs)
 - SVG
    - [Link](https://svg.haskell-miso.org/)
 - Simple
    - [Link](https://simple.haskell-miso.org/)
    - [Source](https://github.com/dmjio/miso/blob/master/exe/Main.hs)

## Haddocks
  - [GHCJS](https://haddocks.haskell-miso.org/)
  - [GHC](http://hackage.haskell.org/package/miso)

## Sample application
```haskell
-- | Haskell language pragma
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    model  = 0   		-- initial model
    update = updateModel	-- update function
    view   = viewModel		-- view function
    events = defaultEvents	-- default delegated events
    subs   = []			-- empty subscription list

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Model Action
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (show x)
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
