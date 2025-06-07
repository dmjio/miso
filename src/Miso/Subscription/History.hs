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
import           Network.URI hiding (path)
import           System.IO.Unsafe
-----------------------------------------------------------------------------
import           Miso.Concurrent
import qualified Miso.FFI.Internal as FFI
import           Miso.String
import           Miso.Effect (Sub)
-----------------------------------------------------------------------------
-- | Retrieves current URI of page
getURI :: JSM URI
{-# INLINE getURI #-}
getURI = do
  href <- fromMisoString <$> getWindowLocationHref
  case parseURI href of
    Nothing  -> fail $ "Could not parse URI from window.location: " ++ href
    Just uri ->
      pure (dropPrefix uri)
  where
    dropPrefix u@URI{..}
      | '/' : xs <- uriPath = u { uriPath = xs }
      | otherwise = u
-----------------------------------------------------------------------------
-- | Pushes a new URI onto the History stack
pushURI :: URI -> JSM ()
{-# INLINE pushURI #-}
pushURI uri = pushStateNoModel uri { uriPath = toPath uri }
-----------------------------------------------------------------------------
-- | Prepend '/' if necessary
toPath :: URI -> String
toPath uri =
  case uriPath uri of
    "" -> "/"
    "/" -> "/"
    xs@('/' : _) -> xs
    xs -> '/' : xs
-----------------------------------------------------------------------------
-- | Replaces current URI on stack
replaceURI :: URI -> JSM ()
{-# INLINE replaceURI #-}
replaceURI uri = replaceTo' uri { uriPath = toPath uri }
-----------------------------------------------------------------------------
-- | Navigates backwards
back :: JSM ()
{-# INLINE back #-}
back = void $ getHistory # "back" $ ()
-----------------------------------------------------------------------------
-- | Navigates forwards
forward :: JSM ()
{-# INLINE forward #-}
forward = void $ getHistory # "forward" $ ()
-----------------------------------------------------------------------------
-- | Jumps to a specific position in history
go :: Int -> JSM ()
{-# INLINE go #-}
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
  FFI.windowAddEventListener (ms "popstate") $ \_ ->
    sink . f =<< getURI
-----------------------------------------------------------------------------
pushStateNoModel :: URI -> JSM ()
{-# INLINE pushStateNoModel #-}
pushStateNoModel u = do
  pushState . pack . show $ u
  liftIO (serve chan)
-----------------------------------------------------------------------------
replaceTo' :: URI -> JSM ()
{-# INLINE replaceTo' #-}
replaceTo' u = do
  replaceState . pack . show $ u
  liftIO (serve chan)
-----------------------------------------------------------------------------
getWindowLocationHref :: JSM MisoString
getWindowLocationHref = do
  href <- fromJSVal =<< jsg "window" ! "location" ! "href"
  case join href of
    Nothing -> pure mempty
    Just uri -> pure uri
-----------------------------------------------------------------------------
getHistory :: JSM JSVal
getHistory = jsg "window" ! "history"
-----------------------------------------------------------------------------
pushState :: MisoString -> JSM ()
pushState url = do
  _ <- getHistory # "pushState" $ (jsNull, jsNull, url)
  pure ()
-----------------------------------------------------------------------------
replaceState :: MisoString -> JSM ()
replaceState url = do
  _ <- getHistory # "replaceState" $ (jsNull, jsNull, url)
  pure ()
-----------------------------------------------------------------------------
