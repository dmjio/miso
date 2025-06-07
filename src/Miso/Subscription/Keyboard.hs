-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Keyboard
-- Copyright   :  (C) 2016-2025 David M. Johnson
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
import           Data.Set
import qualified Data.Set as S
import           Language.Javascript.JSaddle hiding (new)
-----------------------------------------------------------------------------
import           Miso.Effect (Sub)
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | type for arrow keys currently pressed
--
--  * 37 left arrow  ( x = -1 )
--  * 38 up arrow    ( y =  1 )
--  * 39 right arrow ( x =  1 )
--  * 40 down arrow  ( y = -1 )
data Arrows
 = Arrows
 { arrowX :: !Int
 , arrowY :: !Int
 } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Helper function to convert keys currently pressed to @Arrows@, given a
-- mapping for keys representing up, down, left and right respectively.
toArrows :: ([Int], [Int], [Int], [Int]) -> Set Int -> Arrows
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
-- | Maps @Arrows@ onto a Keyboard subscription
arrowsSub :: (Arrows -> action) -> Sub action
arrowsSub = directionSub ([38], [40], [37], [39])
-----------------------------------------------------------------------------
-- | Maps @Arrows@ onto a Keyboard subscription for directions (W+A+S+D keys)
wasdSub :: (Arrows -> action) -> Sub action
wasdSub = directionSub ([87], [83], [65], [68])
-----------------------------------------------------------------------------
-- | Maps a specified list of keys to directions (up, down, left, right)
directionSub
  :: ([Int], [Int], [Int], [Int])
  -> (Arrows -> action)
  -> Sub action
directionSub dirs = keyboardSub . (. toArrows dirs)
-----------------------------------------------------------------------------
-- | Returns subscription for Keyboard.
-- The callback will be called with the Set of currently pressed @keyCode@s.
keyboardSub :: (Set Int -> action) -> Sub action
keyboardSub f sink = do
  keySetRef <- liftIO (newIORef mempty)
  FFI.windowAddEventListener "keyup" $ keyUpCallback keySetRef
  FFI.windowAddEventListener "keydown" $ keyDownCallback keySetRef
  FFI.windowAddEventListener "blur" $ blurCallback keySetRef
    where
      keyDownCallback keySetRef = \keyDownEvent -> do
          Just key <- fromJSVal =<< getProp "keyCode" (Object keyDownEvent)
          newKeys <- liftIO $ atomicModifyIORef' keySetRef $ \keys ->
             let !new = S.insert key keys
             in (new, new)
          sink (f newKeys)

      keyUpCallback keySetRef = \keyUpEvent -> do
          Just key <- fromJSVal =<< getProp "keyCode" (Object keyUpEvent)
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
