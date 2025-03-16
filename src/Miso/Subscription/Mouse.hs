-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Mouse
-- Copyright   :  (C) 2016-2025 David M. Johnson
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
-- | Captures mouse coordinates as they occur and writes them to
-- an event sink
mouseSub :: (PointerEvent -> action) -> Sub action
mouseSub = windowSub "pointermove" pointerDecoder
-----------------------------------------------------------------------------
