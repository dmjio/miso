{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)

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
    | UnMount MisoString
    | Mount MisoString
    | Sample
    deriving (Show, Eq)

data MainAction
    = Toggle
    | MountMain
    | UnMountMain
    | SampleChild
    | StartLogger
    | StopLogger

type MainModel = Bool

main :: IO ()
main = run $ startApp app
  { logLevel = DebugPrerender
  , subs = []
  }

secs :: Int -> Int
secs = (* 1000000)

loggerSub :: MisoString -> Sub action
loggerSub msg = \_ ->
  forever $ do
    liftIO $ threadDelay (secs 1)
    consoleLog msg

app :: App "app" MainModel MainAction
app = defaultApp False updateModel1 viewModel1

counterApp2 :: App "app-2" Model Action
counterApp2 = (defaultApp 0 updateModel2 viewModel2)
  { subs = [loggerSub "component-2 sub"]
  }

counterApp3 :: App "app-3" (Bool, Model) Action
counterApp3 = (defaultApp (True, 0) updateModel3 viewModel3)
  { subs = [ loggerSub "component-3 sub"]
  }

counterApp4 :: App "app-4" Model Action
counterApp4 = (defaultApp 0 updateModel4 viewModel4)
  { subs = [loggerSub "component-4 sub"]
  }

-- | Constructs a virtual DOM from a model
viewModel1 :: MainModel -> View MainAction
viewModel1 x =
    div_
        []
        [ "Component 1 - Three sub components nested recursively below me"
        , br_ []
        , "The +/- for Components 3 and 4 will affect their local state"
        , br_ []
        , "The +/- for Components 3 and 4 will affect their local state"
        , br_ []
        , "Toggle component button will oscillate between component 4 and 5"
        , br_ []
        , "Components 4 will update its own state and the state of component 3"
        , br_ []
        , "Components 5 will update its own state and the state of component 2"
        , br_ []
        , "Sample state will read from component 2 or 3's state depending on the toggle"
        , br_ []
        , "Example of using 'start' and 'stop' subscriptions dynamically"
        , div_ []
          [ button_ [ onClick StartLogger ] [ "start logger" ]
          , button_ [ onClick StopLogger ] [ "stop logger" ]
          ]
        , button_ [onClick Toggle] [text "Toggle Component 2"]
        , button_ [onClick SampleChild] [text "Sample Child (unsafe)"]
        , if x
            then
                componentWith counterApp2 Nothing
                  [ onMounted MountMain
                  , onUnmounted UnMountMain
                  ]
            else div_ [id_ "other test"] ["Main application content"]
        ]

-- | Updates model, optionally introduces side effects
updateModel1 :: MainAction -> Effect MainModel MainAction
updateModel1 StartLogger = startSub "logger" (loggerSub "main-app")
updateModel1 StopLogger  = stopSub "logger"
updateModel1 Toggle = modify not
updateModel1 UnMountMain =
  io (consoleLog "Component 2 was unmounted!")
updateModel1 MountMain =
  io (consoleLog "Component 2 was mounted!")
updateModel1 SampleChild = do
  io $ do
    componentTwoModel <- sample counterApp2
    consoleLog $
        "Sampling child component 2 from parent component main (unsafe): " <>
          ms (show componentTwoModel)

-- | Updates model, optionally introduces side effects
updateModel2 :: Action -> Effect Model Action
updateModel2 AddOne = modify (+1)
updateModel2 SubtractOne = modify (subtract 1)
updateModel2 u@UnMount{} = unmountAction u
updateModel2 m@Mount{} = mountAction m
updateModel2 SayHelloWorld = do
  io (consoleLog "Hello World from Component 2")
updateModel2 _ = pure ()

unmountAction :: Action -> Effect model action
unmountAction (UnMount name) = io $ consoleLog ("Component " <> name <> " was unmounted!")
unmountAction _ = pure ()

mountAction :: Action -> Effect model action
mountAction (Mount name) = io $ consoleLog ("Component " <> name <> " was mounted!")
mountAction _ = pure ()

-- | Constructs a virtual DOM from a model
viewModel2 :: Model -> View Action
viewModel2 x =
    div_
        []
        [ "This is the view for Component 2"
        , button_ [onClick AddOne] [text "+"]
        , text (ms x)
        , button_ [onClick SubtractOne] [text "-"]
        , componentWith counterApp3 Nothing
          [ onMounted (Mount "3")
          , onUnmounted (UnMount "3")
          ]
        ]

-- | Updates model, optionally introduces side effects
updateModel3 :: Action -> Effect (Bool, Model) Action
updateModel3 AddOne = do
  modify (fmap (+1))
updateModel3 SubtractOne = do
  modify (fmap (subtract 1))
updateModel3 ToggleAction =
  modify $ \(x,y) -> (not x, y)
updateModel3 x@UnMount{} = unmountAction x
updateModel3 x@Mount{} = mountAction x
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
        , button_ [onClick ToggleAction]
          [ text $ "Toggle Component " <> if toggle then "4" else "5"
          ]
        ] ++ [ if toggle
               -- dmj: N.B. if you are replacing a component w/ another component
               -- when using the 'component_' function you *must* use 'embedKeyed'
               -- , think of it like a StableName. Failure to do so is undefined behavior
               -- but it will probably result in the component not actually getting replaced.
               --
               -- If you are replacing an unnamed component (using 'component_') with anything else (e.g. 'vtext', 'vnode',
               -- 'vcomp', null), then you don't need to worry about this.
               then
                 componentWith counterApp4 Nothing
                   [ onMounted (Mount "4")
                   , onUnmounted (UnMount "4")
                   ]
               else
                 componentWith counterApp5 Nothing
                   [ onMounted (Mount "5")
                   , onUnmounted (UnMount "5")
                   ]
             ]

-- | Updates model, optionally introduces side effects
updateModel4 :: Action -> Effect Model Action
updateModel4 AddOne = do
  modify (+1)
  io (notify counterApp3 AddOne)
updateModel4 SubtractOne = do
  modify (subtract 1)
  io (notify counterApp3 SubtractOne)
updateModel4 Sample =
  io $ do
    componentTwoModel <- sample counterApp3
    consoleLog $
      "Sampling parent component 3 from child component 4: " <>
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
        , button_ [onClick Sample] [text "Sample Component 3 state"]
        ]

app5 :: App "app-5" Model Action
app5 =
        counterApp5
            { subs = [loggerSub "component-5 sub"]
            }

counterApp5 :: App "app-5" Model Action
counterApp5 = defaultApp 0 updateModel5 viewModel5

-- | Updates model, optionally introduces side effects
updateModel5 :: Action -> Effect Model Action
updateModel5 AddOne = do
  modify (+1)
  io (notify counterApp2 AddOne)
updateModel5 SubtractOne = do
  modify (subtract 1)
  io (notify counterApp2 SubtractOne)
updateModel5 Sample =
  io $ do
    componentTwoModel <- sample counterApp2
    consoleLog $
      "Sampling parent component 2 from child component 5: " <>
         ms (show componentTwoModel)
updateModel5 SayHelloWorld = do
  io (consoleLog "Hello World from Component 5")
updateModel5 _ = pure ()

-- | Constructs a virtual DOM from a model
viewModel5 :: Model -> View Action
viewModel5 x =
    div_
        []
        [ "This is the view for Component 5"
        , button_ [onClick AddOne] [text "+"]
        , text (ms x)
        , button_ [onClick SubtractOne] [text "-"]
        , button_ [onClick Sample] [text "Sample Component 2 state"]
        ]
