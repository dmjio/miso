:ramen: miso
======================
![Hackage](https://img.shields.io/hackage/v/miso.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-green.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
<a href="https://www.irccloud.com/invite?channel=%23haskell-miso&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1" target="_blank"><img src="https://img.shields.io/badge/IRC-%23haskell--miso-1e72ff.svg?style=flat"  height="20"></a>
[![Slack Status](https://haskell-miso-slack.herokuapp.com/badge.svg)](https://haskell-miso-slack.herokuapp.com)
![Build Status](https://api.travis-ci.org/dmjio/miso.svg?branch=master)

**Miso** is a small [isomorphic](http://nerds.airbnb.com/isomorphic-javascript-future-web-apps/) [Haskell](https://www.haskell.org/) front-end framework featuring a virtual-dom, diffing / patching algorithm, event delegation, event batching, SVG, Server-sent events, Websockets, and an extensible Subscription-based subsystem. Inspired by [Elm](http://elm-lang.org/), [Redux](http://redux.js.org/) and [Bobril](http://github.com/bobris/bobril). `IO` and other effects (like `XHR`) can be introduced into the system via the `Effect` data type. Miso makes heavy use of the [GHCJS](https://github.com/ghcjs/ghcjs) FFI and therefore has minimal dependencies.

## Examples
  - TodoMVC
    - [Link](http://miso-todomvc.bitballoon.com/)
    - [Source](https://github.com/dmjio/miso/blob/master/examples/todo-mvc/Main.hs)
  - Mario
    - [Link](https://s3.amazonaws.com/aws-website-mario-5u38b/index.html)
    - [Source](https://github.com/dmjio/miso/blob/master/examples/mario/Main.hs)

## Documentation
  - [GHCJS](https://d10z4r8eai3cm9.cloudfront.net/)
  - [GHC](https://d1f745wtmyhj66.cloudfront.net/)

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
