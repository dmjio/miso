----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
-- | Miso framework import
import Miso
import Miso.String
----------------------------------------------------------------------------
-- | Type synonym for an application model
type Model = Int
----------------------------------------------------------------------------
-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Required when using the WASM backnend
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run (startApp app)
----------------------------------------------------------------------------
app :: App Model Action
app = defaultApp emptyModel updateModel viewModel SayHelloWorld
----------------------------------------------------------------------------
-- | Empty model
emptyModel :: Model
emptyModel = 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m          = noEff m
updateModel AddOne m        = noEff (m + 1)
updateModel SubtractOne m   = noEff (m - 1)
updateModel SayHelloWorld m = m <# NoOp <$ consoleLog "Hello World"
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ []
  [ button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  ]
----------------------------------------------------------------------------
