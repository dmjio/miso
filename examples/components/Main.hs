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
  deriving (Show, Eq)

main :: IO ()
main = startApp app

app :: App Model Action
app = App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model         = 0             -- initial model
    update        = updateModel1  -- update function
    view          = viewModel1    -- view function
    events        = defaultEvents -- default delegated events
    subs          = []            -- empty subscription list
    mountPoint    = "body"        -- mount point for application (Nothing defaults to 'body')
    logLevel      = Off           -- Used to copy DOM into VDOM, applies only to `miso` function

-- | Constructs a virtual DOM from a model
viewModel1 :: Model -> View Action
viewModel1 x = div_ [ id_ "main div" ]
  [ "Main app - two sub components below me"
  , Component counterApp2
  , Component counterApp3
  ]

-- | Updates model, optionally introduces side effects
updateModel1 :: Action -> Model -> Effect Action Model
updateModel1 NoOp m          = noEff m
updateModel1 SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World1") >> pure NoOp

-- are you sure counter-app is on the DOM before you start the delegator?
counterApp2 :: App Model Action
counterApp2 = App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel2         -- update function
    view   = viewModel2           -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = "counter-app-2"    -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- Used to copy DOM into VDOM, applies only to `miso` function

-- | Updates model, optionally introduces side effects
updateModel2 :: Action -> Model -> Effect Action Model
updateModel2 AddOne m = do
  noEff (m + 1)
updateModel2 SubtractOne m   = do
  noEff (m - 1)
updateModel2 NoOp m          = noEff m
updateModel2 SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World2") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel2 :: Model -> View Action
viewModel2 x = div_ []
  [ "counter app 2"
  , button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , rawHtml "<div><p>hey expandable 2!</div></p>"
  ]

counterApp3 :: App (Bool, Model) Action
counterApp3 = App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = (True,0)                    -- initial model
    update = updateModel3         -- update function
    view   = viewModel3           -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = "counter-app-3"    -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- Used to copy DOM into VDOM, applies only to `miso` function

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
  -- ^ this tests mounting and unmounting a componente

-- | Constructs a virtual DOM from a model
viewModel3 :: (Bool, Model) -> View Action
viewModel3 (toggle, x) = div_ [] $
  [ "counter app 3"
  , button_ [ onClick AddOne ] [ text "+" ]
  , text (ms x)
  , button_ [ onClick SubtractOne ] [ text "-" ]
  , button_ [ onClick Toggle4 ] [ text "toggle component 4" ]
  , rawHtml "<div><p>hey expandable 3!</div></p>"
  ] ++ [ Component counterApp4 | toggle ]

counterApp4 :: App Model Action
counterApp4 = App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel4         -- update function
    view   = viewModel4           -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = "counter-app-4"    -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- Used to copy DOM into VDOM, applies only to `miso` function

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

