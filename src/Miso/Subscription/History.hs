{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
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
  ( getCurrentURI
  , pushURI
  , replaceURI
  , back
  , forward
  , go
  , uriSub
  , URI (..)
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Network.URI hiding (path)
import           System.IO.Unsafe

import           Miso.Concurrent
import qualified Miso.FFI as FFI
import           Miso.String
import           Miso.Types (Sub)

-- | Retrieves current URI of page
getCurrentURI :: IO URI
{-# INLINE getCurrentURI #-}
getCurrentURI = getURI

-- | Retrieves current URI of page
getURI :: IO URI
{-# INLINE getURI #-}
getURI = do
  href <- fromMisoString <$> FFI.getWindowLocationHref
  case parseURI href of
    Nothing  -> fail $ "Could not parse URI from window.location: " ++ href
    Just uri -> return uri

-- | Pushes a new URI onto the History stack
pushURI :: URI -> IO ()
{-# INLINE pushURI #-}
pushURI uri = pushStateNoModel uri { uriPath = toPath uri }

-- | Prepend '/' if necessary
toPath :: URI -> String
toPath uri =
  case uriPath uri of
    "" -> "/"
    "/" -> "/"
    xs@('/' : _) -> xs
    xs -> '/' : xs

-- | Replaces current URI on stack
replaceURI :: URI -> IO ()
{-# INLINE replaceURI #-}
replaceURI uri = replaceTo' uri { uriPath = toPath uri }

-- | Navigates backwards
back :: IO ()
{-# INLINE back #-}
back = FFI.back

-- | Navigates forwards
forward :: IO ()
{-# INLINE forward #-}
forward = FFI.forward

-- | Jumps to a specific position in history
go :: Int -> IO ()
{-# INLINE go #-}
go n = FFI.go n

chan :: Waiter
{-# NOINLINE chan #-}
chan = unsafePerformIO emptyWaiter

-- | Subscription for @popstate@ events, from the History API
uriSub :: (URI -> action) -> Sub action
uriSub = \f sink -> do
  void.forkIO.forever $ do
    liftIO (wait chan)
    liftIO . sink . f =<< getURI
  FFI.windowAddEventListener "popstate" =<< do
    FFI.asyncCallback (sink . f =<< getURI)

pushStateNoModel :: URI -> IO ()
{-# INLINE pushStateNoModel #-}
pushStateNoModel u = do
  FFI.pushState . ms . show $ u
  liftIO (serve chan)

replaceTo' :: URI -> IO ()
{-# INLINE replaceTo' #-}
replaceTo' u = do
  FFI.replaceState . ms . show $ u
  liftIO (serve chan)
