{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)

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
    | Sample
    deriving (Show, Eq)

data MainAction
    = MainNoOp
    | Toggle
    | MountMain
    | UnMountMain
    | SampleChild

type MainModel = Bool

main :: IO ()
main = run $ startApp app
  { logLevel = DebugPrerender
  , subs = [loggerSub "main-app"]
  }

secs :: Int -> Int
secs = (* 1000000)

loggerSub :: MisoString -> Sub action
loggerSub msg = \_ ->
    forever $ do
        liftIO $ threadDelay (secs 10)
        consoleLog msg

app :: App MainModel MainAction
app = defaultApp False updateModel1 viewModel1

component2 :: Component Model Action
component2 =
    component "component-2"
        counterApp2
            { subs = [loggerSub "component-2 sub"]
            }

component3 :: Component (Bool, Model) Action
component3 =
    component "component-3"
        counterApp3
            { subs = [loggerSub "component-3 sub"]
            }

component4 :: Component Model Action
component4 =
    component "component-4"
        counterApp4
            { subs = [loggerSub "component-4 sub"]
            }

-- | Constructs a virtual DOM from a model
viewModel1 :: MainModel -> View MainAction
viewModel1 x =
    div_
        []
        [ "Component 1 - Three sub components nested recursively below me"
        , br_ []
        , "The +/- for Components 3 and 4 will affect the state of Component 2"
        , br_ []
        , "This is an example of component communication using the 'mail' / 'notify' functions"
        , br_ []
        , button_ [onClick Toggle] [text "Toggle Component 2"]
        , button_ [onClick SampleChild] [text "Sample Child (unsafe)"]
        , if x
            then
                embed component2
                  [ onCreated MountMain
                  , onBeforeDestroyed UnMountMain
                  ] 
            else div_ [id_ "other test"] ["Main application content"]
        ]

-- | Updates model, optionally introduces side effects
updateModel1 :: MainAction -> MainModel -> Effect MainAction MainModel
updateModel1 MainNoOp m = noEff m
updateModel1 Toggle m = noEff (not m)
updateModel1 UnMountMain m =
    m <# do
        consoleLog "Component 2 was unmounted!"
        pure MainNoOp
updateModel1 MountMain m =
    m <# do
        consoleLog "Component 2 was mounted!"
        pure MainNoOp
updateModel1 SampleChild m =
    m <# do
      componentTwoModel <- sample component2
      consoleLog $
        "Sampling child component 2 from parent component main (unsafe)" <>
          ms (show componentTwoModel)
      pure MainNoOp

counterApp2 :: App Model Action
counterApp2 = defaultApp 0 updateModel2 viewModel2

-- | Updates model, optionally introduces side effects
updateModel2 :: Action -> Model -> Effect Action Model
updateModel2 AddOne m =
    noEff (m + 1)
updateModel2 SubtractOne m =
    noEff (m - 1)
updateModel2 NoOp m = noEff m
updateModel2 SayHelloWorld m = noEff m
updateModel2 UnMount m =
    m <# do
        consoleLog "Component 3 was unmounted!"
        pure NoOp
updateModel2 Mount m =
    m <# do
        consoleLog "Component 3 was mounted!"
        pure NoOp
updateModel2 _ m = noEff m

-- | Constructs a virtual DOM from a model
viewModel2 :: Model -> View Action
viewModel2 x =
    div_
        []
        [ "This is the view for Component 2"
        , button_ [onClick AddOne] [text "+"]
        , text (ms x)
        , button_ [onClick SubtractOne] [text "-"]
        , embed component3
          [ onCreated Mount
          , onBeforeDestroyed UnMount
          ] 
        ]

counterApp3 :: App (Bool, Model) Action
counterApp3 = defaultApp (True, 0) updateModel3 viewModel3

-- | Updates model, optionally introduces side effects
updateModel3 :: Action -> (Bool, Model) -> Effect Action (Bool, Model)
updateModel3 AddOne (t, n) =
    (t, n + 1) <# do
        mail component2 AddOne
        pure NoOp
updateModel3 SubtractOne (t, n) =
    (t, n - 1) <# do
        mail component2 SubtractOne
        pure NoOp
updateModel3 SayHelloWorld m = noEff m
updateModel3 ToggleAction (t, n) = noEff (not t, n)
updateModel3 UnMount m =
    m <# do
        consoleLog "Component 4 was unmounted!"
        pure NoOp
updateModel3 Mount m =
    m <# do
        consoleLog "Component 4 was mounted!"
        pure NoOp
updateModel3 NoOp m = noEff m
updateModel3 Sample m = noEff m

-- | Constructs a virtual DOM from a model
viewModel3 :: (Bool, Model) -> View Action
viewModel3 (toggle, x) =
    div_ [] $
        [ "This is the view for Component 3"
        , button_ [onClick AddOne] [text "+"]
        , text (ms x)
        , button_ [onClick SubtractOne] [text "-"]
        , button_ [onClick ToggleAction] [text "Toggle Component 4"]
        ]
            ++ [ embed component4
                   [ onMounted Mount
                   , onUnmounted UnMount
                   ]
               | toggle
               ]

counterApp4 :: App Model Action
counterApp4 = defaultApp 0 updateModel4 viewModel4

-- | Updates model, optionally introduces side effects
updateModel4 :: Action -> Model -> Effect Action Model
updateModel4 AddOne m =
    (m + 1) <# do
        mail component2 AddOne
        pure NoOp
updateModel4 SubtractOne m =
    (m - 1) <# do
        mail component2 SubtractOne
        pure NoOp
updateModel4 Sample m =
    m <# do
      componentTwoModel <- sample component2
      consoleLog $
          "Sampling parent component 2 from child component 4: " <>
            ms (show componentTwoModel)
      pure NoOp
updateModel4 SayHelloWorld m =
    m <# liftIO (putStrLn "Hello World from Component 4") >> pure NoOp
updateModel4 _ m = noEff m

-- | Constructs a virtual DOM from a model
viewModel4 :: Model -> View Action
viewModel4 x =
    div_
        []
        [ "This is the view for Component 4"
        , button_ [onClick AddOne] [text "+"]
        , text (ms x)
        , button_ [onClick SubtractOne] [text "-"]
        , button_ [onClick Sample] [text "Sample Component 2 state"]
        ]
