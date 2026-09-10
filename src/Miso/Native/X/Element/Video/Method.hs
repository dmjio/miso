-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Video.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Video.Method
  ( -- *** Methods
    play
  , pause
  , stop
  , seek
  ) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
-- | Params object for 'seek'.
newtype Seek = Seek Double
-----------------------------------------------------------------------------
instance ToJSVal Seek where
  toJSVal (Seek position) = do
    o <- create
    set "position" position o
    toJSVal o
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#play
--
-- Plays the video.
--
-- > play "#myVideo" Played PlayFailed
--
play
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
play selector action = invokeExec "play" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#pause
--
-- Pauses video playback.
--
-- > pause "#myVideo" Paused PauseFailed
--
pause
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
pause selector action = invokeExec "pause" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#stop
--
-- Stops video playback.
--
-- > stop "#myVideo" Stopped StopFailed
--
stop
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
stop selector action = invokeExec "stop" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#seek
--
-- Seeks to the target playback position, in seconds. Preserves the previous
-- playing or paused state. If the target position is out of range, the
-- error continuation is called.
--
-- > seek "#myVideo" 30.0 Sought SeekFailed
--
seek
  :: MisoString
  -> Double
  -- ^ Target playback position, in seconds
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
seek selector position action =
  invokeExec "seek" selector (Seek position) (\() -> action)
-----------------------------------------------------------------------------
