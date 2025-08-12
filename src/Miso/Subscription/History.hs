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
import           Language.Javascript.JSaddle
import           Network.URI hiding (path)
-----------------------------------------------------------------------------
import           Miso.Subscription.Util
import qualified Miso.FFI.Internal as FFI
import           Miso.String (MisoString, ms, fromMisoString)
import           Miso.Effect (Sub)
-----------------------------------------------------------------------------
-- | Subscription for @popstate@ events, from the History API
uriSub :: (URI -> action) -> Sub action
uriSub withURI sink = createSub acquire release sink
  where
    release callbacks = forM_ callbacks $ \(eventName, callback) ->
      FFI.windowRemoveEventListener (ms eventName) callback
    acquire = do
      let eventNames =
            [ "miso-pushState"
            , "miso-replaceState"
            , "popstate"
            ]
      cbs <- forM eventNames $ \eventName ->
        FFI.windowAddEventListener (ms eventName) $ \_ ->
          sink . withURI =<< getURI
      pure (Prelude.zip eventNames cbs)
-----------------------------------------------------------------------------
-- | Retrieves current URI of page
getURI :: JSM URI
getURI = do
  href <- fromMisoString <$> getWindowLocationHref
  case parseURI href of
    Nothing  -> fail $ "Could not parse URI from window.location: " ++ href
    Just uri -> pure (dropPrefix uri)
  where
    dropPrefix u@URI{..}
      | '/' : xs <- uriPath = u { uriPath = xs }
      | otherwise = u
-----------------------------------------------------------------------------
-- | Pushes a new URI onto the History stack
pushURI :: URI -> JSM ()
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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/replaceState>
replaceURI :: URI -> JSM ()
replaceURI uri = replaceState uri { uriPath = toPath uri }
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/back>
back :: JSM ()
back = void $ getHistory # "back" $ ()
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/forward>
forward :: JSM ()
forward = void $ getHistory # "forward" $ ()
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/go>
go :: Int -> JSM ()
go n = void $ getHistory # "go" $ [n]
-----------------------------------------------------------------------------
pushStateNoModel :: URI -> JSM ()
pushStateNoModel uri = do
  void $ getHistory # "pushState" $ (jsNull, jsNull, ms (show uri))
  FFI.customEvent (ms "miso-pushState") =<< jsg "window"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/History/replaceState>
replaceState :: URI -> JSM ()
replaceState u = do
  _ <- getHistory # "replaceState" $ (jsNull, jsNull, ms (show u))
  FFI.customEvent (ms "miso-replaceState") =<< jsg "window"
-----------------------------------------------------------------------------
getWindowLocationHref :: JSM MisoString
getWindowLocationHref = do
  href <- fromJSVal =<< jsg "window" ! "location" ! "href"
  case join href of
    Nothing -> pure mempty
    Just uri -> pure uri
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window/history>
getHistory :: JSM JSVal
getHistory = jsg "window" ! "history"
-----------------------------------------------------------------------------
