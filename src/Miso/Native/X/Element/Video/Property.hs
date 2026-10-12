-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Video.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Video.Property
  ( -- *** Property
    src_
  , loop_
  , volume_
  , muted_
  , speed_
  , objectFit_
  , mode_
  , timeupdateInterval_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#src
--
-- Video source URL. Only online network URLs are supported. After @src@
-- changes, playback of the previous @src@ stops immediately and the player
-- tries to render the first frame of the new @src@.
--
-- > src_ "https://example.com/video.mp4"
--
src_ :: MisoString -> Attribute model action
src_ = textProp "src"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#loop
--
-- Whether to loop playback.
--
-- Default Value: @False@
--
loop_ :: Bool -> Attribute model action
loop_ = boolProp "loop"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#volume
--
-- Volume from @0@ to @1@.
--
-- Default Value: @1.0@
--
volume_ :: Double -> Attribute model action
volume_ = doubleProp "volume"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#muted
--
-- Whether the video is muted. This is independent from @volume_@; after
-- unmuting, the previous volume is restored.
--
-- Default Value: @False@
--
muted_ :: Bool -> Attribute model action
muted_ = boolProp "muted"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#speed
--
-- Playback speed, from @0.1@ to @2.0@.
--
-- Default Value: @1.0@
--
speed_ :: Double -> Attribute model action
speed_ = doubleProp "speed"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#object-fit
--
-- Video scaling strategy. Semantics are consistent with CSS. Supported
-- values are @\"contain\"@, @\"cover\"@, and @\"fill\"@.
--
-- > objectFit_ "cover"
--
-- Default Value: @\"contain\"@
--
objectFit_ :: MisoString -> Attribute model action
objectFit_ = textProp "object-fit"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#mode
--
-- UIMethod execution mode. Operations such as @play@ and @stop@ are
-- asynchronous. Supported values:
--
-- * @\"queue\"@: Execute operations in queue order, waiting for the
--   previous operation's callback before running the next.
-- * @\"direct\"@: Execute immediately without waiting for the previous
--   operation's callback.
-- * @\"latest\"@: If the previous operation's callback hasn't fired yet,
--   keep only the latest pending operation, overwriting earlier ones.
--
-- Default Value: @\"queue\"@
--
mode_ :: MisoString -> Attribute model action
mode_ = textProp "mode"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/next/api/elements/built-in/video.html#timeupdate-interval
--
-- Minimum dispatch interval for the @bindtimeupdate@ event, in seconds.
--
-- Default Value: @0.33@
--
timeupdateInterval_ :: Double -> Attribute model action
timeupdateInterval_ = doubleProp "timeupdate-interval"
-----------------------------------------------------------------------------
