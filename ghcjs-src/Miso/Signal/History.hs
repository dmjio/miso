{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Miso.Signal.History where

import Data.JSString
import GHCJS.Foreign.Callback
import Network.URI

import Miso.Signal
import Miso.Types

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

foreign import javascript unsafe "window.onpopstate = $1;"
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

getURI :: IO URI
{-# INLINE getURI #-}
getURI = do
  URI <$> pure mempty
      <*> pure Nothing
      <*> do unpack <$> getPathName
      <*> do unpack <$> getSearch
      <*> pure mempty

newtype URL = URL String
  deriving (Show, Eq)

newtype PopStateEvent = PopStateEvent { unPopStateEvent :: URI }
  deriving (Show, Eq)

data History = History {
    pathSignal :: Signal PopStateEvent
  , goTo :: URI -> IO ()
  , replaceTo :: URI -> IO ()
  , back :: IO ()
  , forward :: IO ()
  , go :: Int -> IO ()
  , initialPath :: URI
  }

historySignal :: IO History
historySignal = do
  (pathSignal, sink) <- signal
  onPopState =<< do
    asyncCallback $ sink =<< PopStateEvent <$> getURI
  initialPath <- getURI
  pure History {
    goTo = \uri -> do
      let newUri = if uriPath uri == mempty
                     then uri { uriPath = "/" }
                     else uri
      pushStateNoModel newUri
  , replaceTo = \uri -> do
      let newUri = if uriPath uri == mempty
                     then uri { uriPath = "/" }
                     else uri
      replaceTo' newUri
  , back = back'
  , forward = forward'
  , go = \n -> go' n
  , pathSignal = pathSignal
  , initialPath = initialPath
  }


