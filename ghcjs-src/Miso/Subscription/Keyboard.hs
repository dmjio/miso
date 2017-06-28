{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Keyboard
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Keyboard
  ( -- * Types
    Arrows (..)
    -- * Subscriptions
  , arrowsSub
  , keyboardSub
  ) where

import           Data.IORef
import           Data.Set
import qualified Data.Set as S
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           JavaScript.Object
import           JavaScript.Object.Internal

import           Miso.FFI
import           Miso.Html.Internal ( Sub )

-- | type for arrow keys currently pressed
--  37 left arrow  ( x = -1 )
--  38 up arrow    ( y =  1 )
--  39 right arrow ( x =  1 )
--  40 down arrow  ( y = -1 )
data Arrows = Arrows {
   arrowX :: Int
 , arrowY :: Int
 } deriving (Show, Eq)

-- | Helper function to convert keys currently pressed to `Arrow`
toArrows :: Set Int -> Arrows
toArrows set =
  Arrows {
    arrowX =
      case (S.member 37 set, S.member 39 set) of
        (True, False) -> -1
        (False, True) -> 1
        (_,_) -> 0
 ,  arrowY =
      case (S.member 40 set, S.member 38 set) of
        (True, False) -> -1
        (False, True) -> 1
        (_,_) -> 0
 }

-- | Maps `Arrows` onto a Keyboard subscription
arrowsSub :: (Arrows -> action) -> Sub action model
arrowsSub = keyboardSub . (. toArrows)

-- | Returns subscription for Keyboard
keyboardSub :: (Set Int -> action) -> Sub action model
keyboardSub f _ sink = do
  keySetRef <- newIORef mempty
  windowAddEventListener "keyup" =<< keyUpCallback keySetRef
  windowAddEventListener "keydown" =<< keyDownCallback keySetRef
    where
      keyDownCallback keySetRef = do
        asyncCallback1 $ \keyDownEvent -> do
          Just key <- fromJSVal =<< getProp "keyCode" (Object keyDownEvent)
          newKeys <- atomicModifyIORef' keySetRef $ \keys ->
             let !new = S.insert key keys
             in (new, new)
          sink (f newKeys)

      keyUpCallback keySetRef = do
        asyncCallback1 $ \keyUpEvent -> do
          Just key <- fromJSVal =<< getProp "keyCode" (Object keyUpEvent)
          newKeys <- atomicModifyIORef' keySetRef $ \keys ->
             let !new = S.delete key keys
             in (new, new)
          sink (f newKeys)

