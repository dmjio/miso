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
    | NoOp
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
app :: App Effect Model Action
app = defaultApp 0 updateModel viewModel

-- | UpdateModels model, optionally introduces side effects
updateModel :: Action -> Effect Action Model ()
updateModel NoOp = noEff
updateModel (AddOne event) = do
  m <- get
  m + 1 <# do
    consoleLog (ms (show event))
    pure NoOp
updateModel (SubtractOne event) = do
  m <- get
  m - 1 <# do
    consoleLog (ms (show event))
    pure NoOp
updateModel SayHelloWorld = do
  scheduleIO_ (consoleLog "Hello World!")

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
    div_
        []
        [ button_ [onPointerDown AddOne] [text "+"]
        , text (ms x)
        , button_ [onPointerDown SubtractOne] [text "-"]
        ]
