:ramen: miso
======================
[![Hackage](https://img.shields.io/hackage/v/miso.svg?style=flat-square)](http://hackage.haskell.org/package/miso)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-green.svg?style=flat-square)](https://haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg?style=flat-square)](https://github.com/dmjio/miso/blob/master/LICENSE)
[![IRC #haskell-miso](https://img.shields.io/badge/IRC-%23haskell--miso-1e72ff.svg?style=flat-square)](https://www.irccloud.com/invite?channel=%23haskell-miso&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1)
[![Slack Status](https://img.shields.io/badge/Slack-miso-E01563.svg?style=flat-square)](https://haskell-miso-slack.herokuapp.com)
[![Hydra CI](https://img.shields.io/badge/Hydra-CI-00BDFD.svg?style=flat-square)](https://hydra.dmj.io)

**Miso** is a small "[isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/)" [Haskell](https://www.haskell.org/) front-end framework featuring a virtual-dom, diffing / patching algorithm, event delegation, event batching, SVG, Server-sent events, Websockets, and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril). `IO` and other effects (like `XHR`) can be introduced into the system via the `Effect` data type. Miso makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies.

## Examples
  - TodoMVC
    - [Link](https://d3u8rq3uy5wnb9.cloudfront.net/)
    - [Source](https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs)
  - Mario
    - [Link](https://dfhxhtlu1tq0x.cloudfront.net/)
    - [Source](https://github.com/dmjio/miso/blob/master/examples/mario/Main.hs)
 - SVG
    - [Link](https://d2dwfl7f3j7of0.cloudfront.net/)
 - Simple
    - [Link](https://dco9lhtzw9c6i.cloudfront.net)
    - [Source](https://github.com/dmjio/miso/blob/master/exe/Main.hs)

## Documentation
  - [GHCJS](https://d10z4r8eai3cm9.cloudfront.net/)
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
