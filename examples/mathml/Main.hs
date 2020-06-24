
-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import Miso.Mathml

data Model = Empty deriving Eq

data Action
  =  NoOp
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = Empty                -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp = noEff

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = nodeMathml "math" [] [
    nodeMathml "msup" [] [
        nodeMathml "mi" [] [text "x"]
        , nodeMathml "mn" [] [text "2"]
    ]
 ]
