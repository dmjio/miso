{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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
    | UnMount MisoString
    | Mount MisoString
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
            { subs = [ loggerSub "component-3 sub"]
            }

component4 :: Component Model Action
component4 =
    component_
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
        , embed component3
          [ onMounted (Mount "3")
          , onUnmounted (UnMount "3")
          ]
        ]

counterApp3 :: App (Bool, Model) Action
counterApp3 = defaultApp (True, 0) updateModel3 viewModel3

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
               then
                 embedKeyed component4
                   [ onMounted (Mount "4")
                   , onUnmounted (UnMount "4")
                   ] "key-4"
               else
                 embedKeyed component5
                   [ onMounted (Mount "5")
                   , onUnmounted (UnMount "5")
                   ] "key-5"
             ]

counterApp4 :: App Model Action
counterApp4 = defaultApp 0 updateModel4 viewModel4

-- | Updates model, optionally introduces side effects
updateModel4 :: Action -> Effect Model Action
updateModel4 AddOne = do
  modify (+1)
  io (notify component3 AddOne)
updateModel4 SubtractOne = do
  modify (subtract 1)
  io (notify component3 SubtractOne)
updateModel4 Sample =
  io $ do
    componentTwoModel <- sample component3
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

component5 :: Component Model Action
component5 =
    component_
        counterApp5
            { subs = [loggerSub "component-5 sub"]
            }

counterApp5 :: App Model Action
counterApp5 = defaultApp 0 updateModel5 viewModel5

-- | Updates model, optionally introduces side effects
updateModel5 :: Action -> Effect Model Action
updateModel5 AddOne = do
  modify (+1)
  io (notify component2 AddOne)
updateModel5 SubtractOne = do
  modify (subtract 1)
  io (notify component2 SubtractOne)
updateModel5 Sample =
  io $ do
    componentTwoModel <- sample component2
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
