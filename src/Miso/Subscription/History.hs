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
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           System.IO.Unsafe
-----------------------------------------------------------------------------
import           Miso.Concurrent
import           Miso.DSL
import qualified Miso.FFI.Internal as FFI
import           Miso.String
import           Miso.Router
import           Miso.Effect (Sub)
import           Miso.Subscription.Util
-----------------------------------------------------------------------------
-- | Pushes a new URI onto the History stack.
pushURI :: URI -> IO ()
pushURI uri = do
  pushState (prettyURI uri)
  liftIO (notify chan)
-----------------------------------------------------------------------------
-- | Replaces current URI on stack.
replaceURI :: URI -> IO ()
replaceURI uri = do
  replaceState (prettyURI uri)
  liftIO (notify chan)
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
chan :: Waiter
{-# NOINLINE chan #-}
chan = unsafePerformIO waiter
-----------------------------------------------------------------------------
-- | Subscription for @popstate@ events, from the History API.
uriSub :: (URI -> action) -> Sub action
uriSub f sink = createSub acquire release sink
  where
    release (tid, cb) = do
      FFI.windowRemoveEventListener "popstate" cb
      killThread tid
    acquire = do
      tid <- forkIO . forever $ do
        liftIO (wait chan)
        sink . f =<< getURI
      cb <- FFI.windowAddEventListener "popstate" $ \_ ->
        sink . f =<< getURI
      pure (tid, cb)
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
