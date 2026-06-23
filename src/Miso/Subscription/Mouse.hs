-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Mouse
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription.Mouse" provides a single global subscription,
-- 'mouseSub', that fires on every @pointermove@ event on @window@,
-- delivering a 'Miso.Event.Types.PointerEvent' carrying coordinates,
-- pressure, pointer type, and other pointer metadata.
--
-- It is a thin convenience wrapper over
-- @'Miso.Subscription.Window.windowSub' \"pointermove\" 'Miso.Event.Decoder.pointerDecoder'@.
-- Use 'Miso.Subscription.Window.windowPointerMoveSub' directly if you
-- need identical behaviour but prefer the window-level import.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Subscription.Mouse"
-- import "Miso.Event.Types" ('Miso.Event.Types.PointerEvent'(..))
--
-- data Action = MouseMoved 'Miso.Event.Types.PointerEvent'
--
-- subs :: ['Miso.Effect.Sub' Action]
-- subs = [ 'mouseSub' MouseMoved ]
--
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update (MouseMoved ev) = do
--   let (cx, cy) = client ev   -- (clientX, clientY)
--   ...
-- @
--
-- = See also
--
-- * "Miso.Subscription.Window" — 'Miso.Subscription.Window.windowPointerMoveSub',
--   'Miso.Subscription.Window.windowCoordsSub', 'Miso.Subscription.Window.windowSub'
-- * "Miso.Event.Types" — 'Miso.Event.Types.PointerEvent', 'Miso.Event.Types.PointerType'
-- * "Miso.Event.Decoder" — 'Miso.Event.Decoder.pointerDecoder'
----------------------------------------------------------------------------
module Miso.Subscription.Mouse
  ( -- *** Subscription
    mouseSub
  ) where
-----------------------------------------------------------------------------
import Miso.Event (pointerDecoder, PointerEvent)
import Miso.Subscription.Window (windowSub)
import Miso.Effect (Sub)
-----------------------------------------------------------------------------
-- | Captures mouse coordinates as they occur and writes them to
-- an event sink.
mouseSub
  :: (PointerEvent -> action)
  -- ^ Callback fired with the full 'PointerEvent' on every @pointermove@
  -> Sub action
mouseSub = windowSub "pointermove" pointerDecoder
-----------------------------------------------------------------------------
