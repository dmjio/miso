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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element/pointermove_event>
--
-- Subscribes to global @pointermove@ events, capturing the full t'PointerEvent'
-- payload on every pointer movement.
--
-- @
-- app :: Component ROOT () Model Action
-- app = (component initialModel update view)
--   { subs = [ mouseSub MouseMoved ] }
--
-- data Action = MouseMoved PointerEvent
-- @
--
mouseSub
  :: (PointerEvent -> action)
  -- ^ Callback invoked with the t'PointerEvent' on every pointer move
  -> Sub action
mouseSub = windowSub "pointermove" pointerDecoder
-----------------------------------------------------------------------------
