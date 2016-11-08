{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Miso.History where

import Data.JSString
import GHCJS.Foreign.Callback
import GHCJS.Types
import Network.URI

import Miso.Signal
import Miso.Types

foreign import javascript unsafe "$r = window.location.path;"
  currentPath' :: IO JSString

foreign import javascript unsafe "$r = window.history.length;"
  historyLength :: IO Int

foreign import javascript unsafe "$r = window.location.pathname;"
  getPathName :: IO JSString

foreign import javascript unsafe "$r = window.location.search;"
  getSearch :: IO JSString

foreign import javascript unsafe "window.onpopstate = $1;"
  onPopState :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "window.history.pushState(null, null, $1);"
  pushStateNoModel' :: JSString -> IO ()

foreign import javascript unsafe "window.history.pushState($1, $2, $3);"
  pushState' :: JSVal -> JSString -> JSString -> IO ()

foreign import javascript unsafe "window.history.replaceState($1,$2);"
  replaceState :: JSString -> JSString -> IO ()

pushStateNoModel :: URI -> IO ()
{-# INLINE pushStateNoModel #-}
pushStateNoModel = pushStateNoModel' . pack . show

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

data History = History {
    pathSignal :: Signal URI
  , goTo       :: URI -> IO ()
  , initialPath :: URI
  }

foreign import javascript unsafe "$r = $1.target.location.href;"
  eHref :: JSVal -> IO JSVal

historySignal :: IO History
historySignal = do
  (pathSignal, sink) <- signal
  onPopState =<< do asyncCallback $ getURI >>= sink
  initialPath <- getURI
  pure History {
    goTo = \uri -> do
      pushStateNoModel $
        if uriPath uri == mempty
          then uri { uriPath = "/" }
          else uri
      pushStateNoModel uri
  , pathSignal = pathSignal
  , initialPath = initialPath
  }


