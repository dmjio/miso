-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.History
-- Copyright   :  (C) 2016-2025 David M. Johnson
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
pushURI :: URI -> IO ()
pushURI uri = do
  pushState (prettyURI uri)
  raisePopState
-----------------------------------------------------------------------------
-- | Replaces current URI on stack. Also raises a `popstate` event.
replaceURI :: URI -> IO ()
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
go :: Int -> IO ()
go n = void $ getHistory # "go" $ [n]
-----------------------------------------------------------------------------
-- | Subscription for t'URI' changes, uses the History API.
--
-- This returns a new t'URI' whenever `go`, `back`, `forward`, `pushState`
-- or `replaceState` have been called.
--
uriSub :: (URI -> action) -> Sub action
uriSub f sink = createSub acquire release sink
  where
    release = FFI.windowRemoveEventListener "popstate"
    acquire = FFI.windowAddEventListener "popstate" $ \_ ->
      (sink =<< f <$> getURI)
-----------------------------------------------------------------------------
-- | Subscription for @popstate@ events, from the History API, mapped
-- to a user-defined 'Router'.
routerSub :: Router route => (Either RoutingError route -> action) -> Sub action
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
