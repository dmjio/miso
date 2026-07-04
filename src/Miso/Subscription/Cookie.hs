-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Cookie
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription.Cookie" provides 'cookieChangeSub', an optional
-- subscription that delivers
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/change_event cookieStore change>
-- events into the update loop. Each event carries the list of cookies that
-- were added\/updated and the list that were deleted since the last event.
--
-- The subscription is optional — applications that only need one-shot reads
-- and writes can use 'Miso.Cookie.cookieGet', 'Miso.Cookie.cookieSet', and
-- 'Miso.Cookie.cookieDelete' directly without registering a subscription.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Subscription.Cookie"
-- import "Miso.Cookie" ('CookieChangeEvent' (..))
--
-- data Action = CookiesChanged 'CookieChangeEvent'
--
-- subs :: ['Miso.Effect.Sub' Action]
-- subs = [ 'cookieChangeSub' CookiesChanged ]
--
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update (CookiesChanged ev) =
--   'Miso.Effect.io_' (consoleLog (ms (show ('cookiesChanged' ev))))
-- @
--
-- = Availability
--
-- The CookieStore API requires a
-- <https://developer.mozilla.org/en-US/docs/Web/Security/Secure_Contexts secure context>
-- (HTTPS or @localhost@) and is not yet supported in all browsers. The
-- subscription silently does nothing when @cookieStore@ is unavailable.
--
-- = See also
--
-- * "Miso.Cookie" — 'Miso.Cookie.cookieGet', 'Miso.Cookie.cookieSet',
--   'Miso.Cookie.cookieDelete', 'Miso.Cookie.cookieGetAll'
-- * "Miso.Subscription.Util" — 'Miso.Subscription.Util.createSub' used internally
-- * "Miso.Subscription" — re-export hub
-----------------------------------------------------------------------------
module Miso.Subscription.Cookie
  ( -- ** Subscriptions
    cookieChangeSub
  ) where
-----------------------------------------------------------------------------
import           Miso.Cookie (CookieChangeEvent)
import           Miso.DSL (fromJSVal)
import           Miso.Effect (Sub)
import           Miso.Subscription.Util (createSub)
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | Returns a 'Sub' that fires an action on every
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/change_event cookieStore change>
-- event, carrying the decoded 'CookieChangeEvent' (changed and deleted
-- cookie lists).
--
-- The subscription is self-contained: it registers the listener on mount
-- and removes it on unmount via 'Miso.Subscription.Util.createSub'.
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CookieStore/change_event>
--
cookieChangeSub
  :: (CookieChangeEvent -> action)
  -- ^ Callback: receives the change event on every cookie modification
  -> Sub action
cookieChangeSub f sink = createSub acquire release sink
  where
    acquire = FFI.cookieStoreAddEventListener $ \ev ->
      fromJSVal ev >>= mapM_ (sink . f)
    release = FFI.cookieStoreRemoveEventListener
-----------------------------------------------------------------------------
