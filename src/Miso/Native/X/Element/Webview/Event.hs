-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Webview.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Webview.Event
  ( -- *** Events
    onError
  , onLoad
  , onLocationChange
  , onMessage
  , onOpenWindow
    -- *** Types
  , WebviewErrorEvent (..)
    -- *** Decoders
  , webviewErrorDecoder
  , urlDecoder
  , messageDecoder
    -- *** Event Map
  , webviewEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
-----------------------------------------------------------------------------
webviewEvents :: Events
webviewEvents
  = M.fromList
  [ ("error", BUBBLE)
  , ("load", BUBBLE)
  , ("locationchange", BUBBLE)
  , ("message", BUBBLE)
  , ("openwindow", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Payload of the @binderror@ event.
data WebviewErrorEvent
  = WebviewErrorEvent
  { errorCode :: Int
    -- ^ The error code
  , errorMsg :: MisoString
    -- ^ The error message
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
webviewErrorDecoder :: Decoder WebviewErrorEvent
webviewErrorDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      WebviewErrorEvent
        <$> o .: "errorCode"
        <*> o .: "errorMsg"
-----------------------------------------------------------------------------
-- | Decodes the @url@ field of @bindlocationchange@ and @bindopenwindow@.
urlDecoder :: Decoder MisoString
urlDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o -> o .: "url"
-----------------------------------------------------------------------------
-- | Decodes the @msg@ field of @bindmessage@.
messageDecoder :: Decoder MisoString
messageDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o -> o .: "msg"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#binderror
--
-- Triggered on a webview error.
--
onError :: (WebviewErrorEvent -> action) -> Attribute action
onError action = on "error" webviewErrorDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bindload
--
-- Triggered when the webview loads successfully.
--
onLoad :: action -> Attribute action
onLoad action = on "load" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bindlocationchange
--
-- *Desktop, Lynx 3.5+*. Triggered when the location changes.
--
onLocationChange :: (MisoString -> action) -> Attribute action
onLocationChange action = on "locationchange" urlDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bindmessage
--
-- Triggered when a message is posted from JavaScript.
--
onMessage :: (MisoString -> action) -> Attribute action
onMessage action = on "message" messageDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bindopenwindow
--
-- *Desktop, Lynx 3.5+*. Triggered on an open-window event.
--
onOpenWindow :: (MisoString -> action) -> Attribute action
onOpenWindow action = on "openwindow" urlDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
