----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import Miso
import Miso.String
----------------------------------------------------------------------------
-- | Application model state
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
main = run (startApp app)
----------------------------------------------------------------------------
-- | `defaultApp` takes as arguments the initial model, update function, view function
app :: App Effect Model Action ()
app = defaultApp emptyModel updateModel viewModel
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect Model Action ()
updateModel = \case
  AddOne        -> counter += 1
  SubtractOne   -> counter -= 1
  SayHelloWorld -> io $ do
    alert "Hello World"
    consoleLog "Hello World"
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ []
  [ button_ [ onClick AddOne ] [ text "+" ]
  , text . ms $ x^.counter
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , button_ [ onClick SayHelloWorld ] [ text "Alert Hello World!" ]
  ]
----------------------------------------------------------------------------
