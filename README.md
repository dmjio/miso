:ramen: <center>miso</center>
======================
![Hackage](https://img.shields.io/hackage/v/miso.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-green.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
<a href="https://www.irccloud.com/invite?channel=%23haskell-miso&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1" target="_blank"><img src="https://img.shields.io/badge/IRC-%23haskell--miso-1e72ff.svg?style=flat"  height="20"></a>
[![Slack Status](https://haskell-miso-slack.herokuapp.com/badge.svg)](https://haskell-miso-slack.herokuapp.com)

**Miso** (micro-isomorphic), is a small [isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/) [Haskell](https://www.haskell.org/) front-end framework featuring a virtual-dom, fast hand-rolled javascript diffing / patching algorithm, event delegation, event batching, event handler DSL, SVG support, and an extensible Signal-based [FRP](http://hackage.haskell.org/package/elerea) system. Inspired by [elm](http://elm-lang.org/) and [redux](http://redux.js.org/), miso currently supports WebSocket, Window, Mouse, History and KeysDown signals. `IO` and other effects (such as `XHR`) can be introduced into the system via the `Effect` data type inside the `update` function. Isomorphic routing is is made possible with [servant](http://haskell-servant.readthedocs.io/en/stable/). *Miso* makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies.

## Setting up
 - Cabal
 - Stack
 - Nix

## Getting Started
```haskell
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Miso

default (MisoString)

data Action = Add | Sub

main :: IO ()
main = startApp 0 view update defaultSettings
  where
    update Add m = noEff ( m + 1 )
    update Sub m = noEff ( m - 1 )

view :: Int -> View Action
view x = div_ [] [
   button_ [ onClick_ Add ] [ text_ "+" ]
 , text_ $ show x
 , button_ [ onClick_ Sub ] [ text_ "-" ]
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


