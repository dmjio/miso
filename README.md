:ramen: <center>miso</center>
======================
![Hackage](https://img.shields.io/hackage/v/miso.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-green.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
<a href="https://www.irccloud.com/invite?channel=%23haskell-miso&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1" target="_blank"><img src="https://img.shields.io/badge/IRC-%23haskell--miso-1e72ff.svg?style=flat"  height="20"></a>
[![Slack Status](https://haskell-miso-slack.herokuapp.com/badge.svg)](https://haskell-miso-slack.herokuapp.com)

**Miso** is a small [isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/) [Haskell](https://www.haskell.org/) front-end framework featuring a virtual-dom, diffing / patching algorithm, event delegation, event batching, SVG support, and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril). `IO` and other effects (like `XHR`) can be introduced into the system via the `Effect` data type. *Miso* makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies.

# Contents
Motivation:
Installation:
Getting Started:
Examples:
Documentation:
Features:
  - Subscriptions
    - WebSocket
    - Window
    - Mouse
    - History
    - Server-sent events
    - Keys
  - Router
  - Event delegation
  - Event batching
  - Diffing / patching
  - SVG
Limitations:
Deployment:

## Motivation
**Miso** is meant to be a pragmatic yet understandable framework, demonstrating exactly how a virtual-dom diffing / patching algorithm works.

## Installation
  - Cabal
  - Stack
  - Nix

## Getting Started
```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
updateModel SubtractOne m = noEff (m + 1)

data Action
  = AddOne
  | SubtractOne
  deriving (Show, Eq)

viewModel :: Int -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (show x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
 ```

## Examples
  - TodoMVC
    - [http://todomvc.haskell-miso.com](Link)
    - [https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs](Source)
  - Mario
    - [http://mario.haskell-miso.com](Link)
    - [https://github.com/dmjio/miso/blob/master/examples/mario/Main.hs](Source)

## Documentation
  - [http://haddocks-ghcjs.haskell-miso.com](GHCJS)
  - [http://haddocks-ghc.haskell-miso.com](GHC)

## Limitations
  - Canvas (2D or 3D) is not yet supported
  - When using keys for faster rendering of large child lists, keys must be unique. Failure to provide unique keys is undefined behavior.
  - If using isomorphic functionality, your templates may not have sibling text nodes. It's undefined behavior to do so.
    - This is due to the browser not being able to distinguish between multiple text nodes during parsing.
  

