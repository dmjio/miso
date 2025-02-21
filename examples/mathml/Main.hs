-- | Haskell language pragma
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.Mathml

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for a miso application
main :: IO ()
main = run $ startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = Main.Empty           -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = "body"          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off

data Model = Empty deriving Eq

data Action
  =  NoOp
  deriving (Show, Eq)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp = noEff

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel _ = nodeMathml "math" [] [
    nodeMathml "msup" [] [
        nodeMathml "mi" [] [text "x"]
        , nodeMathml "mn" [] [text "2"]
    ]
 ]
