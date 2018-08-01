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

import           Control.Concurrent
import           Control.Monad
import           GHCJS.Foreign.Callback
import           Miso.Concurrent
import           Miso.Html.Internal     (Sub)
import           Miso.String
import           Network.URI            hiding (path)
import           System.IO.Unsafe

-- | Retrieves current URI of page
getCurrentURI :: IO URI
{-# INLINE getCurrentURI #-}
getCurrentURI = getURI

-- | Retrieves current URI of page
getURI :: IO URI
{-# INLINE getURI #-}
getURI = do
  href <- fromMisoString <$> getWindowLocationHref
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
back = back'

-- | Navigates forwards
forward :: IO ()
{-# INLINE forward #-}
forward = forward'

-- | Jumps to a specific position in history
go :: Int -> IO ()
{-# INLINE go #-}
go n = go' n

chan :: Notify
{-# NOINLINE chan #-}
chan = unsafePerformIO newEmptyNotify

-- | Subscription for `popState` events, from the History API
uriSub :: (URI -> action) -> Sub action
uriSub = \f sink -> do
  void.forkIO.forever $ do
    wait chan >> do
      sink =<< f <$> getURI
  onPopState =<< do
     asyncCallback $ do
      sink =<< f <$> getURI

foreign import javascript safe "$r = window.location.href || '';"
  getWindowLocationHref :: IO MisoString

foreign import javascript unsafe "window.history.go($1);"
  go' :: Int -> IO ()

foreign import javascript unsafe "window.history.back();"
  back' :: IO ()

foreign import javascript unsafe "window.history.forward();"
  forward' :: IO ()

foreign import javascript unsafe "window.addEventListener('popstate', $1);"
  onPopState :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "window.history.pushState(null, null, $1);"
  pushStateNoModel' :: JSString -> IO ()

foreign import javascript unsafe "window.history.replaceState(null, null, $1);"
  replaceState' :: JSString -> IO ()

pushStateNoModel :: URI -> IO ()
{-# INLINE pushStateNoModel #-}
pushStateNoModel u = do
  pushStateNoModel' . pack . show $ u
  notify chan

replaceTo' :: URI -> IO ()
{-# INLINE replaceTo' #-}
replaceTo' u = do
  replaceState' . pack . show $ u
  notify chan
