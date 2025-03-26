{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import Control.Lens

import Control.Monad.IO.Class

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
    = AddOne PointerEvent
    | SubtractOne PointerEvent
    | NoOp
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
app :: App Transition Model Action
app = defaultTransitionApp 0 updateModel viewModel

-- | UpdateModels model, optionally introduces side effects
updateModel :: Action -> Model -> Transition Action Model
updateModel (AddOne event) _ = do
  id += 1
  scheduleIO_ $ consoleLog (ms (show event))
updateModel (SubtractOne event) m = do
  id -= 1
  scheduleIO_ $ consoleLog (ms (show event))
updateModel NoOp m = undefined
  
updateModel SayHelloWorld m =
  scheduleIO_ (consoleLog "Hello World")

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
    div_
        []
        [ button_ [onPointerDown AddOne] [text "+"]
        , text (ms x)
        , button_ [onPointerDown SubtractOne] [text "-"]
        ]
