{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Miso

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for a miso application
main :: IO ()
main = run $ startApp (defaultApp Main.Empty updateModel viewModel)

data Model
  = Empty
  deriving (Eq)

data Action
  = NoOp
  deriving (Show, Eq)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp = noEff

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel _ =
    nodeMathml
        "math"
        []
        [ nodeMathml
            "msup"
            []
            [ nodeMathml "mi" [] [text "x"]
            , nodeMathml "mn" [] [text "2"]
            ]
        ]
