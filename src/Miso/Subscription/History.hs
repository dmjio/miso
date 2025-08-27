-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards   #-}
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
import           Control.Monad.IO.Class
import           Language.Javascript.JSaddle
import           System.IO.Unsafe
-----------------------------------------------------------------------------
import           Miso.Concurrent
import qualified Miso.FFI.Internal as FFI
import           Miso.String
import           Miso.Router
import           Miso.Effect (Sub)
-----------------------------------------------------------------------------
-- | Pushes a new URI onto the History stack
pushURI :: URI -> JSM ()
pushURI uri = do
  pushState (prettyURI uri)
  liftIO (serve chan)
-----------------------------------------------------------------------------
-- | Replaces current URI on stack
replaceURI :: URI -> JSM ()
replaceURI uri = do
  replaceState (prettyURI uri)
  liftIO (serve chan)
-----------------------------------------------------------------------------
-- | Navigates backwards
back :: JSM ()
back = void $ getHistory # "back" $ ()
-----------------------------------------------------------------------------
-- | Navigates forwards
forward :: JSM ()
forward = void $ getHistory # "forward" $ ()
-----------------------------------------------------------------------------
-- | Jumps to a specific position in history
go :: Int -> JSM ()
go n = void $ getHistory # "go" $ [n]
-----------------------------------------------------------------------------
chan :: Waiter
{-# NOINLINE chan #-}
chan = unsafePerformIO waiter
-----------------------------------------------------------------------------
-- | Subscription for @popstate@ events, from the History API
uriSub :: (URI -> action) -> Sub action
uriSub = \f sink -> do
  void . FFI.forkJSM . forever $ do
    liftIO (wait chan)
    sink . f =<< getURI
  void $ FFI.windowAddEventListener (ms "popstate") $ \_ ->
    sink . f =<< getURI
-----------------------------------------------------------------------------
-- | Subscription for @popstate@ events, from the History API, mapped
-- to a user-defined @Router@
routerSub :: Router route => (Either RoutingError route -> action) -> Sub action
routerSub f = uriSub $ \uri -> f (route uri)
-----------------------------------------------------------------------------
-- | Retrieves the current relative URI by inspecting `pathname`, `search`
-- and `hash`.
getURI :: JSM URI
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
      FFI.consoleError (ms "Couldn't parse URI: " <> err)
      pure emptyURI
    Right uri -> do
      pure uri
-----------------------------------------------------------------------------
getHistory :: JSM JSVal
getHistory = jsg "window" ! "history"
-----------------------------------------------------------------------------
pushState :: MisoString -> JSM ()
pushState url = void $ getHistory # "pushState" $ (jsNull, jsNull, url)
-----------------------------------------------------------------------------
replaceState :: MisoString -> JSM ()
replaceState url = void $ getHistory # "replaceState" $ (jsNull, jsNull, url)
-----------------------------------------------------------------------------
