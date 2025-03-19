{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

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

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for a miso application
main :: IO ()
main = run $ startApp app
  { events = pointerEvents
  }

-- | Application definition (uses 'defaultApp' smart constructor)
app :: App Model Action
app = defaultApp 0 updateModel viewModel

-- | UpdateModels model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (AddOne event) m = m + 1 <#
  NoOp <$ consoleLog (ms (show event))
updateModel (SubtractOne event) m = m - 1 <#
  NoOp <$ consoleLog (ms (show event))
updateModel NoOp m = noEff m
updateModel SayHelloWorld m =
  m <# liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
    div_
        []
        [ button_ [onPointerDown AddOne] [text "+"]
        , text (ms x)
        , button_ [onPointerDown SubtractOne] [text "-"]
        ]
