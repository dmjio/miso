-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Image.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Image.Method
  ( -- *** Methods
    startAnimation
  , pauseAnimation
  , stopAnimation
  , resumeAnimation
  ) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#startanimate>
--
-- Starts an animation at the ID selected
--
-- > startAnimation "someImageId" AnimationStarted AnimationError
--
startAnimation
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
startAnimation selector action =
  invokeExec "startAnimate" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#pauseanimation>
--
-- Pauses an animation at the ID selected
--
-- > pauseAnimation "someImageId" AnimationPauseed AnimationError
--
pauseAnimation
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
pauseAnimation selector action =
  invokeExec "pauseAnimation" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#resumeanimation>
--
-- Resumes an animation at the ID selected
--
-- > resumeAnimation "someImageId" AnimationResumeed AnimationError
--
resumeAnimation
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
resumeAnimation selector action =
  invokeExec "resumeAnimation" selector () (\() -> action)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#stopanimation>
--
-- Stops an animation at the ID selected
--
-- > stopAnimation "someImageId" AnimationStoped AnimationError
--
stopAnimation
  :: MisoString
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
stopAnimation selector action =
  invokeExec "stopAnimation" selector () (\() -> action)
-----------------------------------------------------------------------------
