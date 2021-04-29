{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.History
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
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

import Control.Monad
import Control.Monad.IO.Class
import Miso.Concurrent
import Miso.Effect (Sub)
import Miso.FFI
import qualified Miso.FFI.History as FFI
import Miso.String
import Network.URI hiding (path)
import System.IO.Unsafe

-- | Retrieves current URI of page
getCurrentURI :: JSM URI
{-# INLINE getCurrentURI #-}
getCurrentURI = getURI

-- | Retrieves current URI of page
getURI :: JSM URI
{-# INLINE getURI #-}
getURI = do
  href <- fromMisoString <$> FFI.getWindowLocationHref
  case parseURI href of
    Nothing  -> fail $ "Could not parse URI from window.location: " ++ href
    Just uri -> return uri

-- | Pushes a new URI onto the History stack
pushURI :: URI -> JSM ()
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
replaceURI :: URI -> JSM ()
{-# INLINE replaceURI #-}
replaceURI uri = replaceTo' uri { uriPath = toPath uri }

-- | Navigates backwards
back :: JSM ()
{-# INLINE back #-}
back = FFI.back

-- | Navigates forwards
forward :: JSM ()
{-# INLINE forward #-}
forward = FFI.forward

-- | Jumps to a specific position in history
go :: Int -> JSM ()
{-# INLINE go #-}
go n = FFI.go n

chan :: Notify
{-# NOINLINE chan #-}
chan = unsafePerformIO newEmptyNotify

-- | Subscription for @popstate@ events, from the History API
uriSub :: (URI -> action) -> Sub action
uriSub = \f sink -> do
  void.forkJSM.forever $ do
    liftIO (wait chan)
    liftIO . sink . f =<< getURI
  windowAddEventListener "popstate" $ \_ ->
      liftIO . sink . f =<< getURI

pushStateNoModel :: URI -> JSM ()
{-# INLINE pushStateNoModel #-}
pushStateNoModel u = do
  FFI.pushState . pack . show $ u
  liftIO (notify chan)

replaceTo' :: URI -> JSM ()
{-# INLINE replaceTo' #-}
replaceTo' u = do
  FFI.replaceState . pack . show $ u
  liftIO (notify chan)
