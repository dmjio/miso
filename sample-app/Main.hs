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
import Miso.String
import Miso.Lens
import Miso.Lens.TH
----------------------------------------------------------------------------
-- | Component model state
data Model
  = Model
  { _counter :: Int
  , _condition :: Bool
  } deriving (Show, Eq)
----------------------------------------------------------------------------
data ChildModel = ChildModel { _x :: Int }
  deriving (Eq, Show)
----------------------------------------------------------------------------
$(makeClassy ''Model)
$(makeClassy ''ChildModel)
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
  AddOne ->
    counter += 1
  SubtractOne ->
    counter -= 1
  SayHelloWorld -> io_ $ do
    alert "Hello World"
    consoleLog "Hello World"
  Toggle ->
    condition %= not
----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
viewModel (Model x_ condition_) = div_ []
  [ button_
    [ onClick AddOne ]
    [ text "+" ]
  , button_
    [ onClick Toggle ]
    [ text "Toggle components" ]
  , if condition_
      then div_ []
           [ div_  [ key_ @MisoString "component-1" ] +> childComponent True
           , div_  [ key_ @MisoString "component-2" ] +> childComponent False
           ]
      else "foo"
  , button_
    [ onClick SubtractOne ]
    [ text "-" ]
  , br_ []
  , button_
    [ onClick SayHelloWorld ]
    [ text "Alert Hello World!" ]
  , text (ms x_)
  ]
----------------------------------------------------------------------------
-- | Component used for distribution
childComponent :: HasModel props => Bool -> Component props ChildModel action
childComponent useBinding = (component (ChildModel 0) noop view_)
  { bindings =
    [ counter --> x
    | useBinding
    ]
  } where
      view_ (ChildModel x_) =
        div_
        []
        [ text (ms x_)
        , div_ [] +> childComponent2
        ]
----------------------------------------------------------------------------
childComponent2 :: HasChildModel parent => Component parent Int action
childComponent2 = (component 0 noop view_)
  { bindings =
    [ x --> this
    ]
  } where
      view_ x_ =
        div_
        []
        [ text (ms x_)
        ]
----------------------------------------------------------------------------
