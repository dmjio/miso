-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.History
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription.History" wraps the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/History History API>
-- and
-- <https://developer.mozilla.org/en-US/docs/Web/API/PopStateEvent popstate>
-- event, providing both a reactive subscription and imperative navigation
-- helpers.
--
-- = Subscriptions
--
-- 'uriSub' fires whenever the URL changes — through browser back\/forward
-- buttons or any of the imperative helpers below:
--
-- @
-- subs :: ['Miso.Effect.Sub' Action]
-- subs = [ 'uriSub' UrlChanged ]
-- @
--
-- 'routerSub' is a convenience wrapper that decodes the 'URI' via a
-- 'Miso.Router.Router' instance before delivering it as an action:
--
-- @
-- subs = [ 'routerSub' (RouteChanged . 'Data.Either.fromRight' NotFound) ]
-- @
--
-- = Imperative navigation
--
-- These functions push or replace entries on the browser history stack and
-- simultaneously fire a synthetic @popstate@ event so that 'uriSub' and
-- 'routerSub' are notified automatically:
--
-- @
-- update GoHome     = 'Miso.Effect.io_' ('pushURI' ('Miso.Router.toURI' Home))
-- update GoProfile  = 'Miso.Effect.io_' ('pushRoute' (User (Capture 42)))
-- update ReplaceUrl = 'Miso.Effect.io_' ('replaceURI' newUri)
-- update GoBack     = 'Miso.Effect.io_' 'back'
-- update GoForward  = 'Miso.Effect.io_' 'forward'
-- update (Jump n)   = 'Miso.Effect.io_' ('go' n)
-- @
--
-- 'getURI' reads the current URL from @window.location@ without subscribing:
--
-- @
-- update Init = 'Miso.Effect.io' (GotURI \<$\> 'getURI')
-- @
--
-- = See also
--
-- * "Miso.Router" — 'Miso.Router.Router', 'Miso.Router.URI', 'Miso.Router.toURI', 'Miso.Router.prettyURI'
-- * "Miso.Subscription" — re-export hub
-- * "Miso.Subscription.Util" — 'Miso.Subscription.Util.createSub' used internally
----------------------------------------------------------------------------
module Miso.Subscription.History
  ( -- *** Subscription
    uriSub
  , routerSub
    -- *** Functions
  , getURI
  , pushURI
  , pushRoute
  , replaceURI
  , back
  , forward
  , go
   -- *** Types
  , URI (..)
  ) where
-----------------------------------------------------------------------------
import           Control.Monad
-----------------------------------------------------------------------------
import           Miso.DSL
import qualified Miso.FFI.Internal as FFI
import           Miso.String
import           Miso.Router
import           Miso.Effect (Sub)
import           Miso.Subscription.Util
-----------------------------------------------------------------------------
-- | Pushes a new URI onto the History stack. Also raises a `popstate` event.
pushURI
  :: URI
  -- ^ The URI to push onto the history stack
  -> IO ()
pushURI uri = do
  pushState (prettyURI uri)
  raisePopState
-----------------------------------------------------------------------------
-- | Pushes a new 'Route' onto the History stack. Also raises a `popstate` event.
--
-- Converts the t'Route' to a t'URI' internally.
--
pushRoute
  :: Router route
  => route
  -- ^ The route to push onto the history stack (converted to a URI internally)
  -> IO ()
pushRoute = pushURI . toURI
-----------------------------------------------------------------------------
-- | Replaces current URI on stack. Also raises a `popstate` event.
replaceURI
  :: URI
  -- ^ The URI to replace the current history entry with
  -> IO ()
replaceURI uri = do
  replaceState (prettyURI uri)
  raisePopState
-----------------------------------------------------------------------------
raisePopState :: IO ()
raisePopState = do
  event <- new (jsg "PopStateEvent") ["popstate" :: MisoString]
  window <- jsg "window"
  void $ window # "dispatchEvent" $ [event]
-----------------------------------------------------------------------------
-- | Navigates backwards.
back :: IO ()
back = void $ getHistory # "back" $ ()
-----------------------------------------------------------------------------
-- | Navigates forwards.
forward :: IO ()
forward = void $ getHistory # "forward" $ ()
-----------------------------------------------------------------------------
-- | Jumps to a specific position in history.
go
  :: Int
  -- ^ Number of steps to jump; positive = forward, negative = backward
  -> IO ()
go n = void $ getHistory # "go" $ [n]
-----------------------------------------------------------------------------
-- | Subscription for t'URI' changes, uses the History API.
--
-- This returns a new t'URI' whenever `go`, `back`, `forward`, `pushState`
-- or `replaceState` have been called.
--
uriSub
  :: (URI -> action)
  -- ^ Callback fired with the new 'URI' on every URL change
  -> Sub action
uriSub f sink = createSub acquire release sink
  where
    release = FFI.windowRemoveEventListener "popstate"
    acquire = FFI.windowAddEventListener "popstate" $ \_ ->
      sink . f =<< getURI
-----------------------------------------------------------------------------
-- | Subscription for @popstate@ events, from the History API, mapped
-- to a user-defined 'Router'.
routerSub
  :: Router route
  => (Either RoutingError route -> action)
  -- ^ Callback fired with the decoded route (or 'RoutingError') on every URL change
  -> Sub action
routerSub f = uriSub $ \uri -> f (route uri)
-----------------------------------------------------------------------------
-- | Retrieves the current relative URI by inspecting @pathname@, @search@
-- and @hash@.
getURI :: IO URI
getURI = do
  location <- jsg "window" ! "location"
  pathname <- fromJSValUnchecked =<< location ! "pathname"
  search <- fromJSValUnchecked =<< location ! "search"
  hash <- fromJSValUnchecked =<< location ! "hash"
  let uriText =
        mconcat
        [ pathname
        , search
        , hash
        ]
  case parseURI uriText of
    Left err -> do
      FFI.consoleError ("Couldn't parse URI: " <> err)
      pure emptyURI
    Right uri -> do
      pure uri
-----------------------------------------------------------------------------
getHistory :: IO JSVal
getHistory = jsg "window" ! "history"
-----------------------------------------------------------------------------
pushState :: MisoString -> IO ()
pushState url = void $ getHistory # "pushState" $ (jsNull, jsNull, url)
-----------------------------------------------------------------------------
replaceState :: MisoString -> IO ()
replaceState url = void $ getHistory # "replaceState" $ (jsNull, jsNull, url)
-----------------------------------------------------------------------------
