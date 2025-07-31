----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import Miso
import Miso.String
import Miso.Lens
----------------------------------------------------------------------------
-- | Component model state
newtype Model
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
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
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
updateModel :: Action -> Transition Model Action
updateModel = \case
  AddOne -> do
    counter += 1
    io_ (consoleLog "I clicked something")
  SubtractOne -> do
    counter -= 1
    io_ (consoleLog "I clicked decrement")
  SayHelloWorld -> io_ $ do
    alert "Hello World"
    consoleLog "Hello World"
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel (Model _) = div_ []
  [ button_
    [ onClick AddOne ]
    [ text "+" ]
  , div_
    [ key_ @MisoString "component-1" ] +> childComponent
  , button_
    [ onClick SubtractOne ]
    [ text "-" ]
  , br_ []
  , button_
    [ onClick SayHelloWorld ]
    [ text "Alert Hello World!" ]
  ]
----------------------------------------------------------------------------
class ChildParams component where
  getCounter :: component -> Int
----------------------------------------------------------------------------
-- | Component used for distribution
childComponent
  :: ChildParams props
  => Component props ChildModel action
childComponent = (component (ChildModel 0) noop view_)
  { props =
    [ prop getCounter childModel
    ]
  } where
      view_ (ChildModel x) = text (ms x)
----------------------------------------------------------------------------
instance ChildParams Model where
  getCounter (Model x) = x
----------------------------------------------------------------------------
newtype ChildModel = ChildModel { _childModel :: Int }
  deriving (Eq, Show)
----------------------------------------------------------------------------
childModel :: Lens ChildModel Int
childModel = lens _childModel $ \r x -> r { _childModel = x }
----------------------------------------------------------------------------
