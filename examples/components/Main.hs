{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.IO.Class

import Miso
import Miso.String

type Model = Int

data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  | Toggle4
  | UnMount
  | Mount'
  deriving (Show, Eq)

data MainAction = MainNoOp | Toggle | Mount1 | UnMount1
type MainModel = Bool

main :: IO ()
main = run (startApp app)

app :: App MainModel MainAction
app = defaultApp True updateModel1 viewModel1 MainNoOp

-- | Constructs a virtual DOM from a model
viewModel1 :: MainModel -> View MainAction
viewModel1 x = div_ [ id_ "main div" ]
  [ "Main app - two sub components below me"
  , button_ [ onClick Toggle ] [ text "toggle component 2" ]
  , if x then componentMount counterApp2 mount else div_ [ id_ "other test" ] [ "foo bah" ]
  ] where
      mount
        = Mount
        { onMounted = Mount1
        , onUnmounted =  UnMount1
        }

-- | Updates model, optionally introduces side effects
updateModel1 :: MainAction -> MainModel -> Effect MainAction MainModel
updateModel1 MainNoOp m = noEff m
updateModel1 Toggle m = noEff (not m)
updateModel1 UnMount1 m = do
  m <# do consoleLog "component 2 was unmounted!"
          pure MainNoOp
updateModel1 Mount1 m = do
  m <# do
    consoleLog "component 2 was mounted!"
    pure MainNoOp

-- are you sure counter-app is on the DOM before you start the delegator?
counterApp2 :: App Model Action
counterApp2 = (defaultApp 0 updateModel2 viewModel2 SayHelloWorld)
  { mountPoint = "counter-app-2"
  }

-- | Updates model, optionally introduces side effects
updateModel2 :: Action -> Model -> Effect Action Model
updateModel2 AddOne m = do
  noEff (m + 1)
updateModel2 SubtractOne m   = do
  noEff (m - 1)
updateModel2 NoOp m          = noEff m
updateModel2 SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World2") >> pure NoOp
updateModel2 UnMount m = do
  m <# do consoleLog "component 3 was unmounted!"
          pure NoOp
updateModel2 Mount' m = do
  m <# do
    consoleLog "component 3 was mounted!"
    pure NoOp

-- | Constructs a virtual DOM from a model
viewModel2 :: Model -> View Action
viewModel2 x = div_ [ id_ "something here" ]
  [ "counter app 2"
  , button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , rawHtml "<div><p>hey expandable 2!</div></p>"
  , component counterApp3
  ]

counterApp3 :: App (Bool, Model) Action
counterApp3 = (defaultApp (True, 0) updateModel3 viewModel3 SayHelloWorld)
  { mountPoint = "counter-app-3"
  }

-- | Updates model, optionally introduces side effects
updateModel3 :: Action -> (Bool, Model) -> Effect Action (Bool, Model)
updateModel3 AddOne m@(t,n) = do
  notify m counterApp2 AddOne
  noEff (t, n + 1)
updateModel3 SubtractOne m@(t,n)   = do
  notify m counterApp2 SubtractOne
  noEff (t, n - 1)
updateModel3 NoOp m          = noEff m
updateModel3 SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World3") >> pure NoOp
updateModel3 Toggle4 (t,n) = pure (not t, n)
-- dmj: tests lifecycle hooks
updateModel3 UnMount m = do
  m <# do consoleLog "component 4 was unmounted!"
          pure NoOp
updateModel3 Mount' m = do
  m <# do
    consoleLog "component 4 was mounted!"
    pure NoOp

-- | Constructs a virtual DOM from a model
viewModel3 :: (Bool, Model) -> View Action
viewModel3 (toggle, x) = div_ [] $
  [ "counter app 3, this is the one that should show you the "
  , button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , button_ [ onClick Toggle4 ] [ text "toggle component 4" ]
  , rawHtml "<div><p>hey expandable 3!</div></p>"
  ] ++
  [ component counterApp4
  | toggle
  ]

counterApp4 :: App Model Action
counterApp4 = (defaultApp 0 updateModel4 viewModel4 SayHelloWorld)
  { mountPoint = "counter-app-4"
  }

-- | Updates model, optionally introduces side effects
updateModel4 :: Action -> Model -> Effect Action Model
updateModel4 AddOne m = do
  notify m counterApp2 AddOne
  noEff (m + 1)
updateModel4 SubtractOne m   = do
  notify m counterApp2 SubtractOne
  noEff (m - 1)
updateModel4 NoOp m          = noEff m
updateModel4 SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World4") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel4 :: Model -> View Action
viewModel4 x = div_ []
  [ "counter app 4"
  , button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , rawHtml "<div><p>hey expandable 4!</div></p>"
  ]
