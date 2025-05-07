{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Miso

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for a miso application
main :: IO ()
main = run $ startApp (defaultApp Main.Empty updateModel viewModel)

data Model = Empty
  deriving (Eq)

updateModel :: Applicative f => p -> f ()
updateModel _ = pure ()

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View ()
viewModel _ =
    math_
        []
        [ nodeMathml
            "msup"
            []
            [ nodeMathml "mi" [] [text "x"]
            , nodeMathml "mn" [] [text "2"]
            ]
        ]
