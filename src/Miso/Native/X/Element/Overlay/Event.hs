-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Overlay.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Overlay.Event
  ( -- *** Events
    onDismissOverlay
  , onDismissOverlayWith
  , onError
  , onErrorWith
  , onOverlayTouch
  , onOverlayTouchWith
  , onRequestClose
  , onRequestCloseWith
  , onShowOverlay
  , onShowOverlayWith
    -- *** Types
  , OverlayErrorEvent (..)
  , OverlayTouchEvent (..)
    -- *** Decoders
  , overlayErrorDecoder
  , overlayTouchDecoder
    -- *** Event Map
  , overlayEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.String (MisoString)
import           Miso.Types (Attribute, DOMRef)
-----------------------------------------------------------------------------
overlayEvents :: Events
overlayEvents
  = M.fromList
  [ ("dismissoverlay", BUBBLE)
  , ("error", BUBBLE)
  , ("overlaytouch", BUBBLE)
  , ("requestclose", BUBBLE)
  , ("showoverlay", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Payload of the @binderror@ event.
data OverlayErrorEvent
  = OverlayErrorEvent
  { errorCode :: MisoString
    -- ^ The error code
  , errorMsg :: MisoString
    -- ^ The error message
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Payload of the @bindoverlaytouch@ event.
data OverlayTouchEvent
  = OverlayTouchEvent
  { touchState :: MisoString
    -- ^ The @OverlayTouchState@
  , touchX :: Double
    -- ^ The horizontal position of the touch
  , touchY :: Double
    -- ^ The vertical position of the touch
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
overlayErrorDecoder :: Decoder OverlayErrorEvent
overlayErrorDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      OverlayErrorEvent
        <$> o .: "errorCode"
        <*> o .: "errorMsg"
-----------------------------------------------------------------------------
overlayTouchDecoder :: Decoder OverlayTouchEvent
overlayTouchDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      OverlayTouchEvent
        <$> o .: "state"
        <*> o .: "x"
        <*> o .: "y"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#binddismissoverlay
--
-- Triggered when the overlay is hidden.
--
onDismissOverlay :: action -> Attribute model action
onDismissOverlay action = on "dismissoverlay" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#binderror
--
-- *Android 2.18+*. Triggered on an overlay error.
--
onError :: (OverlayErrorEvent -> action) -> Attribute model action
onError action = on "error" overlayErrorDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindoverlaytouch
--
-- Triggered when the overlay is touched.
--
onOverlayTouch :: (OverlayTouchEvent -> action) -> Attribute model action
onOverlayTouch action = on "overlaytouch" overlayTouchDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindrequestclose
--
-- Triggered when the back button is clicked.
--
onRequestClose :: action -> Attribute model action
onRequestClose action = on "requestclose" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindshowoverlay
--
-- Triggered when the overlay is displayed.
--
onShowOverlay :: action -> Attribute model action
onShowOverlay action = on "showoverlay" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onDismissOverlay', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onDismissOverlayWith :: (DOMRef -> action) -> Attribute model action
onDismissOverlayWith action = on "dismissoverlay" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onError', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onErrorWith :: (OverlayErrorEvent -> DOMRef -> action) -> Attribute model action
onErrorWith action = on "error" overlayErrorDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onOverlayTouch', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onOverlayTouchWith :: (OverlayTouchEvent -> DOMRef -> action) -> Attribute model action
onOverlayTouchWith action = on "overlaytouch" overlayTouchDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onRequestClose', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onRequestCloseWith :: (DOMRef -> action) -> Attribute model action
onRequestCloseWith action = on "requestclose" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onShowOverlay', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onShowOverlayWith :: (DOMRef -> action) -> Attribute model action
onShowOverlayWith action = on "showoverlay" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
