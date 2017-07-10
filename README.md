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

**Miso** is a small "[isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/)" [Haskell](https://www.haskell.org/) front-end framework featuring a virtual-dom, diffing / patching algorithm, event delegation, event batching, SVG, Server-sent events, Websockets, and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril). `IO` and other effects (like `XHR`) can be introduced into the system via the `Effect` data type. Miso makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies.

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
 - SVG
    - [Link](https://svg.haskell-miso.org/)
 - Simple
    - [Link](https://simple.haskell-miso.org/)
    - [Source](https://github.com/dmjio/miso/blob/master/exe/Main.hs)

## Haddocks
  - [GHCJS](https://haddocks.haskell-miso.org/)
  - [GHC](http://hackage.haskell.org/package/miso)

## Getting Started
```haskell
{-# LANGUAGE RecordWildCards #-}

module Main where

import Miso

type Model = Int

main :: IO ()
main = startApp App {..}
  where
    model  = 0
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []

updateModel :: Action -> Model -> Effect Model Action
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)

data Action
  = AddOne
  | SubtractOne
  deriving (Show, Eq)

viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (show x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
 ```
