{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Miso
import Miso.String
import Miso.Lens

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
main = run $ startComponent app

secs :: Int -> Int
secs = (* 1000000)

loggerSub :: MisoString -> Sub action
loggerSub msg = \_ ->
  forever $ do
    liftIO $ threadDelay (secs 1)
    consoleLog msg

app :: Component "app" MainModel MainAction
app = (defaultComponent False updateModel1 viewModel1)
  { logLevel = DebugHydrate
  , subs = []
  } 

counterComponent2 :: Component "app-2" Model Action
counterComponent2 = (defaultComponent 0 updateModel2 viewModel2)
  { subs = [loggerSub "component-2 sub"]
  }

counterComponent3 :: Component "app-3" (Bool, Model) Action
counterComponent3 = (defaultComponent (True, 0) updateModel3 viewModel3)
  { subs = [ loggerSub "component-3 sub"]
  }

counterComponent4 :: Component "app-4" Model Action
counterComponent4 = (defaultComponent 0 updateModel4 viewModel4)
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
                component_ counterComponent2
                  [ onMounted MountMain
                  , onUnmounted UnMountMain
                  ]
            else div_ [id_ "other test"] ["Main application content"]
        ]

data LoggerSub = LoggerSub
  deriving (Eq, Ord)

instance ToMisoString LoggerSub where
  toMisoString LoggerSub = "LoggerSub"

-- | Updates model, optionally introduces side effects
updateModel1 :: MainAction -> Effect MainModel MainAction
updateModel1 StartLogger = startSub LoggerSub (loggerSub "main-app")
updateModel1 StopLogger  = stopSub LoggerSub
updateModel1 Toggle = modify not
updateModel1 UnMountMain =
  io_ (consoleLog "Component 2 was unmounted!")
updateModel1 MountMain =
  io_ (consoleLog "Component 2 was mounted!")
updateModel1 SampleChild = do
  io_ $ do
    componentTwoModel <- sample counterComponent2
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
  io_ (consoleLog "Hello World from Component 2")
updateModel2 _ = pure ()

unmountAction :: Action -> Effect model action
unmountAction (UnMount name) = io_ $ consoleLog ("Component " <> name <> " was unmounted!")
unmountAction _ = pure ()

mountAction :: Action -> Effect model action
mountAction (Mount name) = io_ $ consoleLog ("Component " <> name <> " was mounted!")
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
        , component_ counterComponent3
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
  io_ (consoleLog "Hello World from Component 3")
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
                 component_ counterComponent4
                   [ onMounted (Mount "4")
                   , onUnmounted (UnMount "4")
                   ]
               else
                 component_ counterComponent5
                   [ onMounted (Mount "5")
                   , onUnmounted (UnMount "5")
                   ]
             ]

-- | Updates model, optionally introduces side effects
updateModel4 :: Action -> Effect Model Action
updateModel4 AddOne = do
  modify (+1)
  io_ (notify counterComponent3 AddOne)
updateModel4 SubtractOne = do
  modify (subtract 1)
  io_ (notify counterComponent3 SubtractOne)
updateModel4 Sample =
  io_ $ do
    componentTwoModel <- sample counterComponent3
    consoleLog $
      "Sampling parent component 3 from child component 4: " <>
         ms (show componentTwoModel)
updateModel4 SayHelloWorld = do
  io_ (consoleLog "Hello World from Component 4")
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

app5 :: Component "app-5" (Model, Maybe MisoString) Action
app5 =
        counterComponent5
            { subs = [loggerSub "component-5 sub"]
            }

counterComponent5 :: Component "app-5" (Model, Maybe MisoString) Action
counterComponent5 = defaultComponent (0, Nothing) updateModel5 viewModel5

-- | Updates model, optionally introduces side effects
updateModel5 :: Action -> Effect (Model, Maybe MisoString) Action
updateModel5 AddOne = do
  _1 += 1
  maybeChildId <- use _2
  forM_ maybeChildId $ \childId ->
    io_ (notify' childId counterComponent6 AddOne)
  io_ (notify counterComponent2 AddOne)
updateModel5 SubtractOne = do
  _1 -= 1
  io_ (notify counterComponent2 SubtractOne)
updateModel5 Sample = do
  io_ $ do
    componentTwoModel <- sample counterComponent2
    consoleLog $
      "Sampling parent component 2 from child component 5: " <>
         ms (show componentTwoModel)

  maybeChildId <- use _2
  io_ $ do
    forM_ maybeChildId $ \childId -> do
      componentTwoModel <- sample' childId counterComponent6
      consoleLog $
        "Sampling parent component 6 from child component 5: " <>
           ms (show componentTwoModel)

updateModel5 SayHelloWorld = do
  io_ (consoleLog "Hello World from Component 5")
updateModel5 (Mount childId) = do
  io_ $ consoleLog $ "In mount, setting childId: " <> childId
  _2 ?= childId
updateModel5 _ = pure ()

-- | Constructs a virtual DOM from a model
viewModel5 :: (Model, Maybe MisoString) -> View Action
viewModel5 (x, _) =
    div_
        []
        [ "This is the view for Component 5"
        , button_ [onClick AddOne] [text "+"]
        , text (ms x)
        , button_ [onClick SubtractOne] [text "-"]
        , button_ [onClick Sample] [text "Sample Component 2 state"]
        , "Here is dynamic component 6..."
        , br_ []
        , component_ counterComponent6
          [ onMountedWith Mount
          ]
        ]

-- | "" here means the component is given a dynamically generated name, can also leave generic
-- the component_ or componentWith_
counterComponent6 :: Component Dynamic Model Action
counterComponent6 = defaultComponent 0 updateModel6 viewModel6

-- | Updates model, optionally introduces side effects
updateModel6 :: Action -> Effect Model Action
updateModel6 AddOne = do
  modify (+1)
updateModel6 SubtractOne = do
  modify (subtract 1)
updateModel6 Sample =
  io_ $ do
    componentTwoModel <- sample counterComponent3
    consoleLog $
      "Sampling parent component 3 from child component 6: " <>
         ms (show componentTwoModel)
updateModel6 SayHelloWorld = do
  io_ (consoleLog "Hello World from Component 6")
updateModel6 _ = pure ()

-- | Constructs a virtual DOM from a model
viewModel6 :: Model -> View Action
viewModel6 x =
    div_
        []
        [ "This is the view for Component 6"
        , button_ [onClick AddOne] [text "+"]
        , text (ms x)
        , button_ [onClick SubtractOne] [text "-"]
        , button_ [onClick Sample] [text "Sample Component 3 state"]
        ]
