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
data Model
  = Model
  { _counter :: Int
  , _condition :: Bool
  } deriving (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
condition :: Lens Model Bool
condition = lens _condition $ \record field -> record { _condition = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  | Toggle
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
emptyModel = Model 0 False
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
  AddOne -> counter += 1
  SubtractOne -> counter -= 1
  SayHelloWorld -> io_ $ do
    alert "Hello World"
    consoleLog "Hello World"
  Toggle ->
    condition %= not
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel (Model x condition_) = div_ []
  [ button_
    [ onClick AddOne ]
    [ text "+" ]
  , button_
    [ onClick Toggle ]
    [ text "Toggle components" ]
  , if condition_
      then div_  [ key_ @MisoString "component-1" ] +> childComponent
      else "foo"
  , button_
    [ onClick SubtractOne ]
    [ text "-" ]
  , br_ []
  , button_
    [ onClick SayHelloWorld ]
    [ text "Alert Hello World!" ]
  , text (ms x)
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
      view_ (ChildModel x) =
        div_
        []
        [ text (ms x)
        , div_ [ key_ @MisoString "foobah" ] +> childComponent2
        ]
----------------------------------------------------------------------------
childComponent2 :: Component ChildModel Int action
childComponent2 = (component 0 noop view_)
  { props =
    [ getCounter --> _id
    ]
  } where
      view_ x =
        div_
        []
        [ text (ms x)
        ]
----------------------------------------------------------------------------
instance ChildParams Model where
  getCounter (Model x _) = x
----------------------------------------------------------------------------
instance ChildParams ChildModel where
  getCounter (ChildModel x) = x
----------------------------------------------------------------------------
newtype ChildModel = ChildModel { _childModel :: Int }
  deriving (Eq, Show)
----------------------------------------------------------------------------
childModel :: Lens ChildModel Int
childModel = lens _childModel $ \r x -> r { _childModel = x }
----------------------------------------------------------------------------
