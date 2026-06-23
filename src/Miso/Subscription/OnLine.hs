-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.OnLine
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription.OnLine" provides 'onLineSub', a subscription that
-- tracks the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/onLine navigator.onLine>
-- connectivity status. It registers @online@ and @offline@ event listeners
-- on @window@ and fires an action with 'True' when the connection is
-- restored and 'False' when it is lost.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Subscription.OnLine"
--
-- data Action = OnLineChanged Bool
--
-- subs :: ['Miso.Effect.Sub' Action]
-- subs = [ 'onLineSub' OnLineChanged ]
--
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update (OnLineChanged isOnLine)
--   | isOnLine  = 'Miso.Effect.io_' (consoleLog \"Back online\")
--   | otherwise = 'Miso.Effect.io_' (consoleLog \"Offline\")
-- @
--
-- To read the current status imperatively without subscribing, use
-- 'Miso.Navigator.isOnLine' from "Miso.Navigator".
--
-- = See also
--
-- * "Miso.Navigator" — 'Miso.Navigator.isOnLine' for one-shot reads
-- * "Miso.Subscription" — re-export hub
-- * "Miso.Subscription.Util" — 'Miso.Subscription.Util.createSub' used internally
----------------------------------------------------------------------------
module Miso.Subscription.OnLine
  ( -- *** Subscriptions
    onLineSub
  ) where
-----------------------------------------------------------------------------
import           Miso.Effect (Sub)
import           Miso.Subscription.Util (createSub)
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | Returns 'Sub' for the navigator.onLine API.
-- Fires action with 'True' when the browser goes online, and 'False' when it goes offline.
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/onLine>
--
onLineSub
  :: (Bool -> action)
  -- ^ Callback: 'True' when going online, 'False' when going offline
  -> Sub action
onLineSub f sink = createSub acquire release sink
  where
    release (cb1, cb2) = do
      FFI.windowRemoveEventListener "online" cb1
      FFI.windowRemoveEventListener "offline" cb2
    acquire = do
      cb1 <- FFI.windowAddEventListener "online" (\_ -> sink (f True))
      cb2 <- FFI.windowAddEventListener "offline" (\_ -> sink (f False))
      pure (cb1, cb2)
-----------------------------------------------------------------------------
