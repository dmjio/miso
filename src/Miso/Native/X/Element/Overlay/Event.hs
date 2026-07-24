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
  , onError
  , onOverlayTouch
  , onRequestClose
  , onShowOverlay
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
import           Miso.Types (Attribute)
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
onDismissOverlay :: action -> Attribute action
onDismissOverlay action = on "dismissoverlay" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#binderror
--
-- *Android 2.18+*. Triggered on an overlay error.
--
onError :: (OverlayErrorEvent -> action) -> Attribute action
onError action = on "error" overlayErrorDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindoverlaytouch
--
-- Triggered when the overlay is touched.
--
onOverlayTouch :: (OverlayTouchEvent -> action) -> Attribute action
onOverlayTouch action = on "overlaytouch" overlayTouchDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindrequestclose
--
-- Triggered when the back button is clicked.
--
onRequestClose :: action -> Attribute action
onRequestClose action = on "requestclose" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindshowoverlay
--
-- Triggered when the overlay is displayed.
--
onShowOverlay :: action -> Attribute action
onShowOverlay action = on "showoverlay" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
