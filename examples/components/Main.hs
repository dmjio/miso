{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)

import Miso
import Miso.String

type Model = Int

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

data Action
    = AddOne
    | SubtractOne
    | SayHelloWorld
    | ToggleAction
    | UnMount
    | Mount
    | Sample
    deriving (Show, Eq)

data MainAction
    = Toggle
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
        , "This is an example of component communication using the 'notify' / 'notify' functions"
        , br_ []
        , button_ [onClick Toggle] [text "Toggle Component 2"]
        , button_ [onClick SampleChild] [text "Sample Child (unsafe)"]
        , if x
            then
                embed component2
                  [ onMounted MountMain
                  , onUnmounted UnMountMain
                  ] 
            else div_ [id_ "other test"] ["Main application content"]
        ]

-- | Updates model, optionally introduces side effects
updateModel1 :: MainAction -> Effect MainModel MainAction
updateModel1 Toggle = modify not
updateModel1 UnMountMain =
  io (consoleLog "Component 2 was unmounted!")
updateModel1 MountMain =
  io (consoleLog "Component 2 was mounted!")
updateModel1 SampleChild = do
  io $ do
    componentTwoModel <- sample component2
    consoleLog $
        "Sampling child component 2 from parent component main (unsafe): " <>
          ms (show componentTwoModel)

counterApp2 :: App Model Action
counterApp2 = defaultApp 0 updateModel2 viewModel2

-- | Updates model, optionally introduces side effects
updateModel2 :: Action -> Effect Model Action
updateModel2 AddOne = modify (+1)
updateModel2 SubtractOne = modify (subtract 1)
updateModel2 UnMount =
  io (consoleLog "Component 3 was unmounted!")
updateModel2 Mount =
  io (consoleLog "Component 3 was mounted!")
updateModel2 SayHelloWorld = do
  io (consoleLog "Hello World from Component 2")
updateModel2 _ = pure ()

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
          [ onMounted Mount
          , onUnmounted UnMount
          ] 
        ]

counterApp3 :: App (Bool, Model) Action
counterApp3 = defaultApp (True, 0) updateModel3 viewModel3

-- | Updates model, optionally introduces side effects
updateModel3 :: Action -> Effect (Bool, Model) Action
updateModel3 AddOne = do
  modify (fmap (+1))
  io (notify component2 AddOne)
updateModel3 SubtractOne = do
  modify (fmap (subtract 1))
  io (notify component2 SubtractOne)
updateModel3 ToggleAction =
  modify $ \(x,y) -> (not x, y)
updateModel3 UnMount =
  io (consoleLog "Component 4 was unmounted!")
updateModel3 Mount =
  io (consoleLog "Component 4 was mounted!")
updateModel3 SayHelloWorld = do
  io (consoleLog "Hello World from Component 3")
updateModel3 _ = pure ()

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
updateModel4 :: Action -> Effect Model Action
updateModel4 AddOne = do
  modify (+1)
  io (notify component2 AddOne)
updateModel4 SubtractOne = do
  modify (subtract 1)
  io (notify component2 SubtractOne)
updateModel4 Sample =
  io $ do
    componentTwoModel <- sample component2
    consoleLog $
      "Sampling parent component 2 from child component 4: " <>
         ms (show componentTwoModel)
updateModel4 SayHelloWorld = do
  io (consoleLog "Hello World from Component 4")
updateModel4 _ = pure ()

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
