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
import           Miso.Native.Element.View.Event (onTapWith)
import           Miso.Native.MainThread (setStyleProperty)
import           Miso.Event (mainThread)
import           Miso.Effect (DOMRef)
-----------------------------------------------------------------------------
import           Miso.JSON
import           Miso.Lens
import           Miso.String
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
import           Control.Concurrent
import           GHC.Generics
-----------------------------------------------------------------------------
-- | Application actions
data Action
  = Toggle DOMRef
  -- ^ main-thread tap: scale the touched element up/down. The CSS `transition`
  -- on the element animates the change. Runs on the MTS with no diff/repaint.
  | NoOp
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
-----------------------------------------------------------------------------
-- | Entry point. The balloon's handler is marked 'mainThread' (per handler, not
-- per event), so its @tap@ is dispatched on the MTS with no background round-trip.
main :: IO ()
main = startApp nativeEvents (static (mount_ counterComponent))
-----------------------------------------------------------------------------
counterComponent :: App Int Action
counterComponent = component 0 updateInt viewInt
-----------------------------------------------------------------------------
updateInt :: Action -> Effect context props Int Action
updateInt = \case
  Toggle ref -> do
    -- Bump the (main-thread-local) model for toggle state, then imperatively
    -- write `transform`. The scheduler runs this IO on the MTS and skips the
    -- repaint; the CSS transition animates the element.
    this += 1
    n <- get
    io_ $ setStyleProperty ref "transform"
      (if odd n then "scale(1.6)" else "scale(1.0)")
  NoOp ->
    pure ()
-----------------------------------------------------------------------------
secs :: Int -> Int
secs = (*1000000)
-----------------------------------------------------------------------------
-- | A single balloon; tap it to grow/shrink on the main thread.
viewInt :: context -> props -> Int -> View context Action
viewInt _ _ _ = view_
  [ CSS.style_
    [ CSS.height "100%"
    , CSS.display "flex"
    , CSS.alignItems "center"
    , CSS.justifyContent "center"
    ]
  ]
  [ view_
    [ event (static (mainThread (onTapWith Toggle)))
    , CSS.style_
        [ CSS.backgroundColor CSS.blue
        , CSS.width "120px"
        , CSS.height "120px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        , CSS.transition "transform 0.25s ease-out"
        ]
    ]
    [ text_
      [ CSS.style_ [ CSS.fontSize "56px" ] ]
      [ "🎈" ]
    ]
  ]
-----------------------------------------------------------------------------
