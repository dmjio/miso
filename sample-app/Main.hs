----------------------------------------------------------------------------
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StaticPointers     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE CPP                #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import           Miso.Lens
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _counter :: Int
  } deriving stock (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving stock (Show, Eq)
----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else
main = startApp defaultEvents app
#endif
----------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
----------------------------------------------------------------------------
-- | `component` takes as arguments the initial model, update function, view function
app :: App Model Action
app = component emptyModel updateModel viewModel
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  AddOne        -> counter += 1
  SubtractOne   -> counter -= 1
  SayHelloWorld -> io_ (consoleLog "Hello world")
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ (Model x) =
  vfrag
  [ H.button_ [ H.onClick AddOne ] [ "+" ]
  , text (ms x)
  , H.button_ [ H.onClick SubtractOne ] [ "-" ]
  ]
----------------------------------------------------------------------------
