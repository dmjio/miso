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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/pushState>
--
-- Pushes a new t'URI' onto the browser History stack and dispatches a @popstate@
-- event so that 'uriSub' / 'routerSub' subscribers are notified.
pushURI
  :: URI
  -- ^ New URI to push onto the history stack
  -> IO ()
pushURI uri = do
  pushState (prettyURI uri)
  raisePopState
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/pushState>
--
-- Like 'pushURI' but accepts a typed route value from a 'Router' instance.
-- Converts the route to a t'URI' before pushing.
--
-- @
-- update = \case
--   GoToProfile uid -> io_ (pushRoute (ProfileRoute uid))
-- @
--
pushRoute
  :: Router route
  => route
  -- ^ Typed route to convert to a t'URI' and push onto the history stack
  -> IO ()
pushRoute = pushURI . toURI
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/replaceState>
--
-- Replaces the current entry in the browser History stack with the given t'URI'
-- and dispatches a @popstate@ event so subscribers are notified.
replaceURI
  :: URI
  -- ^ Replacement URI for the current history entry
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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/go>
--
-- Jumps to a specific relative position in the session history.
-- Positive values move forward, negative values move backward.
go
  :: Int
  -- ^ Relative position in history (@1@ = forward, @-1@ = backward, @0@ = reload)
  -> IO ()
go n = void $ getHistory # "go" $ [n]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event>
--
-- Subscribes to @popstate@ events via the History API, delivering the current
-- t'URI' whenever navigation occurs (@go@, @back@, @forward@, @pushState@,
-- or @replaceState@).
--
-- @
-- app = (component m u v) { subs = [ uriSub URIChanged ] }
-- data Action = URIChanged URI
-- @
--
uriSub
  :: (URI -> action)
  -- ^ Callback invoked with the new t'URI' on every navigation event
  -> Sub action
uriSub f sink = createSub acquire release sink
  where
    release = FFI.windowRemoveEventListener "popstate"
    acquire = FFI.windowAddEventListener "popstate" $ \_ ->
      (sink =<< f <$> getURI)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window/popstate_event>
--
-- Like 'uriSub' but parses the t'URI' into a typed route using the supplied
-- 'Router' instance. The callback receives a 'Left' t'RoutingError' if parsing
-- fails, or a 'Right' route value on success.
--
-- @
-- app = (component m u v) { subs = [ routerSub RouteChanged ] }
-- data Action = RouteChanged (Either RoutingError MyRoute)
-- @
--
routerSub
  :: Router route
  => (Either RoutingError route -> action)
  -- ^ Callback invoked with the parsed route (or error) on every navigation event
  -> Sub action
routerSub f = uriSub $ \uri -> f (route uri)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Location>
--
-- Reads the current page URI from @window.location@ by combining
-- @pathname@, @search@, and @hash@.
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
