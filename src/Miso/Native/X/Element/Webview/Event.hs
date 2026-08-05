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
  , onErrorWith
  , onLoad
  , onLoadWith
  , onLocationChange
  , onLocationChangeWith
  , onMessage
  , onMessageWith
  , onOpenWindow
  , onOpenWindowWith
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
import           Miso.Types (Attribute, DOMRef)
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
-- Triggered onMain a webview error.
--
onError :: (WebviewErrorEvent -> action) -> Attribute model action
onError action = on "error" webviewErrorDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bindload
--
-- Triggered when the webview loads successfully.
--
onLoad :: action -> Attribute model action
onLoad action = on "load" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bindlocationchange
--
-- *Desktop, Lynx 3.5+*. Triggered when the location changes.
--
onLocationChange :: (MisoString -> action) -> Attribute model action
onLocationChange action = on "locationchange" urlDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bindmessage
--
-- Triggered when a message is posted from JavaScript.
--
onMessage :: (MisoString -> action) -> Attribute model action
onMessage action = on "message" messageDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/webview.html#bindopenwindow
--
-- *Desktop, Lynx 3.5+*. Triggered on an open-window event.
--
onOpenWindow :: (MisoString -> action) -> Attribute model action
onOpenWindow action = on "openwindow" urlDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Like 'onError', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onErrorWith :: (WebviewErrorEvent -> DOMRef -> action) -> Attribute model action
onErrorWith action = on "error" webviewErrorDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onLoad', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLoadWith :: (DOMRef -> action) -> Attribute model action
onLoadWith action = on "load" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onLocationChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLocationChangeWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onLocationChangeWith action = on "locationchange" urlDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onMessage', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onMessageWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onMessageWith action = on "message" messageDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onOpenWindow', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onOpenWindowWith :: (MisoString -> DOMRef -> action) -> Attribute model action
onOpenWindowWith action = on "openwindow" urlDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
