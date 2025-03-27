{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import Control.Monad.State

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne PointerEvent
  | SubtractOne PointerEvent
  | SayHelloWorld
  deriving (Show, Eq)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for a miso application
main :: IO ()
main = run $ startApp app
  { events = pointerEvents
  }

-- | Application definition (uses 'defaultApp' smart constructor)
app :: App Effect Model Action ()
app = defaultApp 0 updateModel viewModel

-- | UpdateModels model, optionally introduces side effects
updateModel :: Action -> Effect Model Action ()
updateModel (AddOne event) = do
  modify (+1)
  io $ consoleLog (ms (show event))
updateModel (SubtractOne event) = do
  modify (subtract 1)
  io $ consoleLog (ms (show event))
updateModel SayHelloWorld =
  io (consoleLog "Hello World!")

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
    div_
        []
        [ button_ [onPointerDown AddOne] [text "+"]
        , text (ms x)
        , button_ [onPointerDown SubtractOne] [text "-"]
        ]
