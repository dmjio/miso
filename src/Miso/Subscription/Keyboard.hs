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
-- | Directional state derived from arrow (or WASD) key presses.
--
-- Each component is @-1@, @0@, or @1@:
--
--  * 37 left arrow  (@arrowX = -1@)
--  * 39 right arrow (@arrowX =  1@)
--  * 38 up arrow    (@arrowY =  1@)
--  * 40 down arrow  (@arrowY = -1@)
data Arrows
 = Arrows
 { arrowX :: !Int
 -- ^ Horizontal direction: @-1@ (left), @0@ (none), @1@ (right)
 , arrowY :: !Int
 -- ^ Vertical direction: @-1@ (down), @0@ (none), @1@ (up)
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
-- | Subscribes to arrow key events (37–40) and delivers an t'Arrows' value
-- indicating which directions are currently pressed.
--
-- @
-- app = (component m u v) { subs = [ arrowsSub ArrowsChanged ] }
-- data Action = ArrowsChanged Arrows
-- @
--
arrowsSub
  :: (Arrows -> action)
  -- ^ Callback invoked with the current directional state on every keydown/keyup
  -> Sub action
arrowsSub = directionSub ([38], [40], [37], [39])
-----------------------------------------------------------------------------
-- | Like 'arrowsSub' but maps the WASD keys (W=up, S=down, A=left, D=right)
-- to an t'Arrows' value.
--
-- @
-- app = (component m u v) { subs = [ wasdSub DirectionChanged ] }
-- @
--
wasdSub
  :: (Arrows -> action)
  -- ^ Callback invoked with the current directional state on every keydown/keyup
  -> Sub action
wasdSub = directionSub ([87], [83], [65], [68])
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode>
--
-- Subscribes to keyboard events and maps a custom set of keyCodes to
-- directional t'Arrows' values.
--
-- @
-- -- Map numpad keys to directions
-- numpadSub :: (Arrows -> action) -> Sub action
-- numpadSub = directionSub ([104], [98], [100], [102])
-- @
--
directionSub
  :: ([Int], [Int], [Int], [Int])
  -- ^ Tupled lists of keyCodes for @(up, down, left, right)@
  -> (Arrows -> action)
  -- ^ Callback invoked with the current directional state
  -> Sub action
directionSub dirs = keyboardSub . (. toArrows dirs)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode>
--
-- Subscribes to @keydown@, @keyup@, and @blur@ events on @window@, delivering
-- the full set of currently pressed keyCodes on every change.
-- Clears the set when the window loses focus, preventing stuck keys.
--
-- @
-- app = (component m u v) { subs = [ keyboardSub KeysChanged ] }
-- data Action = KeysChanged IntSet
-- @
--
keyboardSub
  :: (IntSet -> action)
  -- ^ Callback invoked with the current pressed-key set on every keydown/keyup/blur
  -> Sub action
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
