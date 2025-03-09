{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Monad.IO.Class

import Miso
import Miso.String

type Model = Int

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  | ToggleAction
  | UnMount
  | Mount
  deriving (Show, Eq)

data MainAction
  = MainNoOp
  | Toggle
  | MountMain
  | UnMountMain

type MainModel = Bool

main :: IO ()
main = run (startComponent mainComponent)

mainComponent :: Component "main" MainModel MainAction
mainComponent = component app

app :: App MainModel MainAction
app = defaultApp True updateModel1 viewModel1 MainNoOp

component2 :: Component "component-2" Model Action
component2 = component counterApp2

component3 :: Component "component-3" (Bool, Model) Action
component3 = component counterApp3

component4 :: Component "component-4" Model Action
component4 = component counterApp4

-- | Constructs a virtual DOM from a model
viewModel1 :: MainModel -> View MainAction
viewModel1 x = div_ [ id_ "Main application" ]
  [ "Component 1 - Three sub components nested recursively below me"
  , "The +/- for Components 3 and 4 will affect the state of Component 2"
  , "This is an example of component communication using the 'mail' / 'notify' functions"
  , button_ [ onClick Toggle ] [ text "Toggle Component 2" ]
  , if x
    then embedWith component2 componentOptions
         { onMounted = Just MountMain
         , onUnmounted = Just UnMountMain
         }
    else div_ [ id_ "other test" ] [ "Main application content" ]
  ]
-- | Updates model, optionally introduces side effects
updateModel1 :: MainAction -> MainModel -> Effect MainAction MainModel
updateModel1 MainNoOp m = noEff m
updateModel1 Toggle m = noEff (not m)
updateModel1 UnMountMain m = do
  m <# do
    consoleLog "Component 2 was unmounted!"
    pure MainNoOp
updateModel1 MountMain m = do
  m <# do
    consoleLog "Component 2 was mounted!"
    pure MainNoOp

counterApp2 :: App Model Action
counterApp2 = defaultApp 0 updateModel2 viewModel2 SayHelloWorld

-- | Updates model, optionally introduces side effects
updateModel2 :: Action -> Model -> Effect Action Model
updateModel2 AddOne m = do
  noEff (m + 1)
updateModel2 SubtractOne m   = do
  noEff (m - 1)
updateModel2 NoOp m          = noEff m
updateModel2 SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World from Component 2") >> pure NoOp
updateModel2 UnMount m = do
  m <# do consoleLog "Component 3 was unmounted!"
          pure NoOp
updateModel2 Mount m = do
  m <# do
    consoleLog "Component 3 was mounted!"
    pure NoOp
updateModel2 _ m = noEff m

-- | Constructs a virtual DOM from a model
viewModel2 :: Model -> View Action
viewModel2 x = div_ []
  [ "This is the view for Component 2"
  , button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , embed component3
  ]

counterApp3 :: App (Bool, Model) Action
counterApp3 = defaultApp (True, 0) updateModel3 viewModel3 SayHelloWorld

-- | Updates model, optionally introduces side effects
updateModel3 :: Action -> (Bool, Model) -> Effect Action (Bool, Model)
updateModel3 AddOne (t,n) =
  (t, n + 1) <# do
    mail component2 AddOne
    pure NoOp
updateModel3 SubtractOne (t,n)   = do
  (t, n - 1) <# do
    mail component2 SubtractOne
    pure NoOp
updateModel3 NoOp m          = noEff m
updateModel3 SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World from Component 3") >> pure NoOp
updateModel3 ToggleAction (t,n) = noEff (not t, n)
updateModel3 UnMount m =
  m <# do
    consoleLog "Component 4 was unmounted!"
    pure NoOp
updateModel3 Mount m =
  m <# do
    consoleLog "component 4 was mounted!"
    pure NoOp

-- | Constructs a virtual DOM from a model
viewModel3 :: (Bool, Model) -> View Action
viewModel3 (toggle, x) = div_ [] $
  [ "This is the view for Component 3"
  , button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , button_ [ onClick ToggleAction ] [ text "Toggle Component 4" ]
  ] ++
  [ embed component4
  | toggle
  ]

counterApp4 :: App Model Action
counterApp4 = defaultApp 0 updateModel4 viewModel4 SayHelloWorld

-- | Updates model, optionally introduces side effects
updateModel4 :: Action -> Model -> Effect Action Model
updateModel4 AddOne m =
  (m + 1) <# do
    mail component2 AddOne
    pure NoOp
updateModel4 SubtractOne m = do
  (m - 1) <# do
    mail component2 SubtractOne
    pure NoOp
updateModel4 SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World from Component 4") >> pure NoOp
updateModel4 _ m          = noEff m

-- | Constructs a virtual DOM from a model
viewModel4 :: Model -> View Action
viewModel4 x = div_ []
  [ "This is the view for Component 4"
  , button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  ]
