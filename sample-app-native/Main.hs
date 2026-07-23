-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE StaticPointers              #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE DerivingStrategies          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso hiding (text_)
import           Miso.Native
import           Miso.Native.Element.View.Event (onTap)
-----------------------------------------------------------------------------
import           Miso.JSON
import           Miso.Lens
import           Miso.String
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
import           Control.Concurrent
import           GHC.Generics
-----------------------------------------------------------------------------
-- | Application model
data Action
  = AddOne
  | SubtractOne
  | Ping
  | Pong
  | NoOp
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = native nativeEvents (static (mount_ counterComponent))
-----------------------------------------------------------------------------
counterComponent :: App Int Action
counterComponent = (component 0 updateInt viewInt)
  { mount = if mts then Just Ping else if bts then Just Pong else Nothing
  }
-----------------------------------------------------------------------------
updateInt
  :: Action
  -> Effect context props Int Action
updateInt = \case
  AddOne ->
    this += 1
  SubtractOne ->
    this -= 1
  Ping ->
    runOnBG $ \sink -> do
      threadDelay (secs 1)
      consoleLog "ping"
      sink Pong
  Pong ->
    runOnMain $ \sink -> do
      threadDelay (secs 1)
      consoleLog "pong"
      sink Ping
-----------------------------------------------------------------------------
secs :: Int -> Int
secs = (*1000000)
-----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewInt :: context -> props -> Int -> View context Action
viewInt _ _ m = view_
  [ CSS.style_
    [ CSS.height "200px"
    , CSS.display "flex"
    , CSS.alignItems "center"
    , CSS.justifyContent "center"
    ]
  ]
  [ view_
    [ onTap AddOne
    , CSS.style_
        [ CSS.backgroundColor CSS.yellow
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.margin "2px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ "🐈"
      ]
    ]
  , view_
    [ CSS.style_
        [ CSS.backgroundColor CSS.orange
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ text (ms m)
      ]
    ]
  , view_
    [ onTap SubtractOne
    , CSS.style_
        [ CSS.backgroundColor CSS.pink
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.margin "2px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ "🍜"
      ]
    ]
 ]
-----------------------------------------------------------------------------
