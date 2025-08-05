----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import Miso hiding (model)
import Miso.String (MisoString, ms)
import Miso.Lens
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _counter :: Int
  , _childCounter :: Int
  } deriving (Show, Eq)
----------------------------------------------------------------------------
counter :: Lens Model Int
counter = lens _counter $ \record field -> record { _counter = field }
----------------------------------------------------------------------------
childCounter :: Lens Model Int
childCounter = lens _childCounter $ \record field -> record { _childCounter = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  deriving (Show, Eq)
----------------------------------------------------------------------------
data ChildModel = ChildModel { _x :: Int }
  deriving (Eq, Show)
----------------------------------------------------------------------------
x :: Lens ChildModel Int
x = lens _x $ \record field -> record { _x = field }
----------------------------------------------------------------------------
-- | Sum type for App events
data ChildAction
  = ChildAddOne
  | ChildSubtractOne
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
app :: Component ROOT Model Action
app = component emptyModel updateModel viewModel
----------------------------------------------------------------------------
-- | Empty application state
emptyModel :: Model
emptyModel = Model 0 0
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Model Action
updateModel = \case
  AddOne ->
    counter += 1
  SubtractOne ->
    counter -= 1
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel (Model parentState _) = div_ []
  [ button_
    [ onClick AddOne ]
    [ text "+" ]
  , div_
    [ id_ "child components"
    ]
    [ div_  [ key_ @MisoString "component-1" ] +> childComponent "one"
    , div_  [ key_ @MisoString "component-2" ] +> childComponent "two"
    ]
  , button_
    [ onClick SubtractOne ]
    [ text "-" ]
  , text ("Parent state: " <> ms parentState)
  ]
----------------------------------------------------------------------------
-- | Component used for distribution
childComponent :: MisoString -> Component Model ChildModel ChildAction
childComponent name = (component (ChildModel 0) updateChildModel childView_)
  { bindings =
      [ childCounter <--> x
      ]
  } where
      childView_ :: ChildModel -> View ChildModel ChildAction
      childView_ (ChildModel x_) =
        div_
        []
        [ text ("Hi I'm: " <> name <> ms x_)
        , button_ [ onClick ChildAddOne ] [ "child +" ]
        , button_ [ onClick ChildSubtractOne ] [ "child -" ]
        ]
----------------------------------------------------------------------------
-- | Updates model, optionally introduces side effects
updateChildModel :: ChildAction -> Effect Model ChildModel ChildAction
updateChildModel = \case
  ChildAddOne ->
    x += 1
  ChildSubtractOne ->
    x -= 1
----------------------------------------------------------------------------
