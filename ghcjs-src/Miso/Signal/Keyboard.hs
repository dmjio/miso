{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.Signal.Keyboard where

import           Control.Concurrent
import           Data.Set
import qualified Data.Set                   as S
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           JavaScript.Object
import           JavaScript.Object.Internal

import           Miso.FFI
import           Miso.Signal.Internal
import           Miso.Types

data Arrows = Arrows {
   arrowX :: Int
 , arrowY :: Int
 } deriving (Show, Eq)

data Keyboard = Keyboard {
    keysDown    :: Signal (Set Int)
  , arrows      :: Signal Arrows
  , stopKeysDown :: IO ()
  , startKeysDown :: IO ()
  }

-- | 37 left arrow  ( x = -1 )
--   38 up arrow    ( y =  1 )
--   39 right arrow ( x =  1 )
--   40 down arrow  ( y = -1 )
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

-- | Returns signals for Keyboard
keyboardSignal :: IO Keyboard
keyboardSignal = do
  keySetRef <- newMVar mempty
  (keysDown, sink) <- signal
  ucb <- upCB sink keySetRef
  dcb <- downCB sink keySetRef
  let stopKeysDown = do
        mapM_ releaseCallback [ucb, dcb]
        windowRemoveEventListener "keyup" ucb
        windowRemoveEventListener "keydown" dcb
      startKeysDown = do
        windowAddEventListener "keyup"  ucb
        windowAddEventListener "keydown" dcb
  let arrows = toArrows <$> keysDown
  startKeysDown
  pure Keyboard {..}
    where
     downCB sink keySetRef = do
       asyncCallback1 $ \keyDownEvent -> do
         Just key <- fromJSVal =<< getProp "keyCode" (Object keyDownEvent)
         modifyMVar_ keySetRef $ \keys -> do
           let newKeys = S.insert key keys
           newKeys <$ sink newKeys

     upCB sink keySetRef = do
       asyncCallback1 $ \keyUpEvent -> do
         Just key <- fromJSVal =<< getProp "keyCode" (Object keyUpEvent)
         modifyMVar_ keySetRef $ \keys -> do
           let newKeys = S.delete key keys
           newKeys <$ sink newKeys



