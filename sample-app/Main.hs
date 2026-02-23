----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import           Miso.Lens
----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (forever)
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _counter :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)
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
app = (component emptyModel updateModel viewModel)
  { subs = [ \sink -> forever $ do
               threadDelay 100000
               sink AddOne
           ]
  }
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent Model Action
updateModel = \case
  AddOne        -> counter += 1
  SubtractOne   -> counter -= 1
  SayHelloWorld -> io_ (consoleLog "Hello world")
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel x =
  H.div_
    [ P.className "counter"
    ]
    [ H.button_ [ H.onClick AddOne ] [ text "+" ]
    , text $ ms (x ^. counter)
    , H.button_ [ H.onClick SubtractOne ] [ text "-" ]
    , H.br_ []
    , H.button_ [ H.onClick SayHelloWorld ] [ text "Alert Hello World!" ]
    ]
----------------------------------------------------------------------------
