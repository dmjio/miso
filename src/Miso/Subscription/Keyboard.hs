-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Keyboard
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription.Keyboard" provides global keyboard subscriptions that
-- track which keys are currently held down. All four subscriptions register
-- @keydown@, @keyup@, and @blur@ listeners on @window@; the @blur@ handler
-- clears the pressed-key set so keys cannot get stuck when the window loses
-- focus.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Subscription.Keyboard"
--
-- -- Fire action with arrow-key state on every key change
-- subs :: ['Miso.Effect.Sub' Action]
-- subs = [ 'arrowsSub' ArrowsChanged ]
--
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update (ArrowsChanged ('Arrows' x y)) = do
--   -- x ∈ {-1, 0, 1}, y ∈ {-1, 0, 1}
--   'Miso.Effect.io_' (move x y)
-- @
--
-- = Subscription variants
--
-- * 'keyboardSub' — delivers the raw @'Data.IntSet.IntSet'@ of all currently
--   pressed
--   <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode keyCodes>.
--   Use this when you need to handle arbitrary key combinations.
--
-- * 'arrowsSub' — maps the four arrow keys (37–40) to an 'Arrows' value
--   with @arrowX ∈ {-1, 0, 1}@ and @arrowY ∈ {-1, 0, 1}@.
--
-- * 'wasdSub' — same as 'arrowsSub' but for W\/A\/S\/D (keyCodes 87\/83\/65\/68).
--
-- * 'directionSub' — fully configurable: supply your own @(up, down, left, right)@
--   keyCode lists and get the same 'Arrows' mapping.
--
-- = See also
--
-- * "Miso.Subscription" — re-export hub
-- * "Miso.Event.Types" — 'Miso.Event.Types.KeyCode', 'Miso.Event.Types.KeyInfo'
-- * "Miso.Html.Event" — per-element 'Miso.Html.Event.onKeyDown' \/ 'Miso.Html.Event.onKeyUp'
----------------------------------------------------------------------------
module Miso.Subscription.Keyboard
  ( -- *** Types
    Arrows (..)
    -- *** Subscriptions
  , arrowsSub
  , directionSub
  , keyboardSub
  , wasdSub
  ) where
-----------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.IntSet
import qualified Data.IntSet as S
-----------------------------------------------------------------------------
import           Miso.DSL hiding (new)
import           Miso.Effect (Sub)
import           Miso.Subscription.Util (createSub)
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | Type for arrow keys currently pressed.
--
--  * 37 left arrow  ( x = -1 )
--  * 38 up arrow    ( y =  1 )
--  * 39 right arrow ( x =  1 )
--  * 40 down arrow  ( y = -1 )
data Arrows
 = Arrows
 { arrowX :: !Int
 -- ^ Horizontal direction: @-1@ (left), @0@ (neutral), @1@ (right)
 , arrowY :: !Int
 -- ^ Vertical direction: @-1@ (down), @0@ (neutral), @1@ (up)
 } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Helper function to convert keys currently pressed to @Arrows@, given a
-- mapping for keys representing up, down, left and right respectively.
toArrows :: ([Int], [Int], [Int], [Int]) -> IntSet -> Arrows
toArrows (up, down, left, right) set' = Arrows
  { arrowX =
      case (check left, check right) of
        (True, False) -> -1
        (False, True) -> 1
        (_,_) -> 0
  , arrowY =
      case (check down, check up) of
        (True, False) -> -1
        (False, True) -> 1
        (_,_) -> 0
  } where
      check = any (`S.member` set')
-----------------------------------------------------------------------------
-- | Maps t'Arrows' onto a Keyboard subscription.
arrowsSub :: (Arrows -> action) -> Sub action
arrowsSub = directionSub ([38], [40], [37], [39])
-----------------------------------------------------------------------------
-- | Maps t'Arrows' onto a Keyboard subscription for directions (W+A+S+D keys).
wasdSub :: (Arrows -> action) -> Sub action
wasdSub = directionSub ([87], [83], [65], [68])
-----------------------------------------------------------------------------
-- | Maps a specified list of keys to directions (up, down, left, right).
-- The Ints represent [keyCode](https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode)s for each direction.
directionSub
  :: ([Int], [Int], [Int], [Int])
  -- ^ @(up, down, left, right)@ keyCode lists for each direction
  -> (Arrows -> action)
  -- ^ Callback fired with the current 'Arrows' state on every key change
  -> Sub action
directionSub dirs = keyboardSub . (. toArrows dirs)
-----------------------------------------------------------------------------
-- | Returns 'Sub' for keyboard events.
-- The callback will be called with the Set of currently pressed @keyCode@s.
keyboardSub :: (IntSet -> action) -> Sub action
keyboardSub f sink = createSub acquire release sink
  where
    release (cb1, cb2, cb3) = do
      FFI.windowRemoveEventListener "keyup" cb1
      FFI.windowRemoveEventListener "keydown" cb2
      FFI.windowRemoveEventListener "blur" cb3
    acquire = do
      keySetRef <- liftIO (newIORef mempty)
      cb1 <- FFI.windowAddEventListener "keyup" (keyUpCallback keySetRef)
      cb2 <- FFI.windowAddEventListener "keydown" (keyDownCallback keySetRef)
      cb3 <- FFI.windowAddEventListener "blur" (blurCallback keySetRef)
      pure (cb1, cb2, cb3)
        where
          keyDownCallback keySetRef = \keyDownEvent -> do
              key <- fromJSValUnchecked =<< getProp "keyCode" (Object keyDownEvent)
              newKeys <- liftIO $ atomicModifyIORef' keySetRef $ \keys ->
                 let !new = S.insert key keys
                 in (new, new)
              sink (f newKeys)

          keyUpCallback keySetRef = \keyUpEvent -> do
              key <- fromJSValUnchecked =<< getProp "keyCode" (Object keyUpEvent)
              newKeys <- liftIO $ atomicModifyIORef' keySetRef $ \keys ->
                 let !new = S.delete key keys
                 in (new, new)
              sink (f newKeys)

          -- Assume keys are released the moment focus is lost. Otherwise going
          -- back and forth to the app can cause keys to get stuck.
          blurCallback keySetRef = \_ -> do
              newKeys <- liftIO $ atomicModifyIORef' keySetRef $ \_ ->
                let !new = S.empty
                in (new, new)
              sink (f newKeys)
-----------------------------------------------------------------------------
