{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.History
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.History
  ( getURI
  , pushURI
  , replaceURI
  , back
  , forward
  , go
  , uriSub
  ) where

import Miso.String
import GHCJS.Foreign.Callback
import Network.URI hiding (path)
import Miso.Html.Internal ( Sub )

-- | Retrieves current URI of page
getURI :: IO URI
{-# INLINE getURI #-}
getURI = do
  URI <$> pure mempty
      <*> pure Nothing
      <*> do unpack <$> getPathName
      <*> do unpack <$> getSearch
      <*> pure mempty

-- | Pushes a new URI onto the History stack
pushURI :: URI -> IO ()
{-# INLINE pushURI #-}
pushURI uri = pushStateNoModel uri { uriPath = path }
  where
    path | uriPath uri == mempty = "/"
         | otherwise = uriPath uri

-- | Replaces current URI on stack
replaceURI :: URI -> IO ()
{-# INLINE replaceURI #-}
replaceURI uri = replaceTo' uri { uriPath = path }
  where
    path | uriPath uri == mempty = "/"
         | otherwise = uriPath uri

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
go = go'

-- | Subscription for `popState` events, from the History API
uriSub :: (URI -> action) -> Sub action model
uriSub = \f _ sink ->
  onPopState =<< do
     ps <- f <$> getURI
     asyncCallback $ sink ps

foreign import javascript unsafe "window.history.go($1);"
  go' :: Int -> IO ()

foreign import javascript unsafe "window.history.back();"
  back' :: IO ()

foreign import javascript unsafe "window.history.forward();"
  forward' :: IO ()

foreign import javascript unsafe "$r = window.location.pathname;"
  getPathName :: IO JSString

foreign import javascript unsafe "$r = window.location.search;"
  getSearch :: IO JSString

foreign import javascript unsafe "window.addEventListener('popstate', $1);"
  onPopState :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "window.history.pushState(null, null, $1);"
  pushStateNoModel' :: JSString -> IO ()

foreign import javascript unsafe "window.history.replaceState(null, null, $1);"
  replaceState' :: JSString -> IO ()

pushStateNoModel :: URI -> IO ()
{-# INLINE pushStateNoModel #-}
pushStateNoModel = pushStateNoModel' . pack . show

replaceTo' :: URI -> IO ()
{-# INLINE replaceTo' #-}
replaceTo' = replaceState' . pack . show
