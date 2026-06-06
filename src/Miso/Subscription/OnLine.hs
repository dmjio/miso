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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Navigator/onLine>
--
-- Subscribes to browser online/offline status changes.
-- Fires the action with 'True' when the browser goes online and 'False' when it goes offline.
--
-- @
-- app :: Component ROOT () Model Action
-- app = (component initialModel update view)
--   { subs = [ onLineSub NetworkStatus ] }
--
-- data Action = NetworkStatus Bool
-- @
--
onLineSub
  :: (Bool -> action)
  -- ^ Callback invoked with 'True' on @online@ and 'False' on @offline@ events
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
