:ramen: <center>miso</center>
======================
![Hackage](https://img.shields.io/hackage/v/miso.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-green.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
<a href="https://www.irccloud.com/invite?channel=%23haskell-miso&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1" target="_blank"><img src="https://img.shields.io/badge/IRC-%23haskell--miso-1e72ff.svg?style=flat"  height="20"></a>
[![Slack Status](https://haskell-miso-slack.herokuapp.com/badge.svg)](https://haskell-miso-slack.herokuapp.com)

**Miso** is a small [isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/) [Haskell](https://www.haskell.org/) front-end framework featuring a virtual-dom, fast hand-rolled javascript diffing / patching algorithm, event delegation, event batching, SVG support, and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril), Miso currently supports WebSocket, Window, Mouse, History and KeysDown subscriptions. `IO` and other effects (such as `XHR`) can be introduced into the system via the `Effect action model` data type. *Miso* makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies.

## Setting up
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
main = startApp model update view subs defaultEvents
  where
    model = 0
    subs = []

update :: Action -> Model -> Effect Model Action
update AddOne m = noEff (m + 1)
update SubtractOne m = noEff (m + 1)

data Action
  = AddOne
  | SubtractOne
  deriving (Show, Eq)

view :: Int -> View Action
view x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (show x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
 ```
## Examples
  - TodoMVC
    - Link: <link goes here>
	- Source: <link goes here>

## Hackage Docs
  - Link: <link goes here>

## Why isomorphic?
https://strongloop.com/strongblog/node-js-react-isomorphic-javascript-why-it-matters/

## FAQ


