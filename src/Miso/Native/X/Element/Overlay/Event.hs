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
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Overlay.Event
  ( -- *** Events
    onDismissOverlay
  , onDismissOverlayWith
  , onDismissOverlayMain
  , onDismissOverlayMainWith
  , onError
  , onErrorWith
  , onErrorMain
  , onErrorMainWith
  , onOverlayTouch
  , onOverlayTouchWith
  , onOverlayTouchMain
  , onOverlayTouchMainWith
  , onRequestClose
  , onRequestCloseWith
  , onRequestCloseMain
  , onRequestCloseMainWith
  , onShowOverlay
  , onShowOverlayWith
  , onShowOverlayMain
  , onShowOverlayMainWith
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
import           Miso.Types (Attribute, EventHandler, DOMRef)
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
-- | Like 'onDismissOverlay', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Dismissed
--
-- view_ [ event (static (onDismissOverlayMain Dismissed)) ] [ "some view" ]
-- @
--
onDismissOverlayMain :: action -> EventHandler model action
onDismissOverlayMain action = onMain "dismissoverlay" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onDismissOverlayMain', but the handler also receives read-only access
-- to the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Dismissed Model DOMRef
--
-- view_ [ event (static (onDismissOverlayMainWith Dismissed)) ] [ "some view" ]
-- @
--
onDismissOverlayMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onDismissOverlayMainWith action = onMain "dismissoverlay" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#binderror
--
-- *Android 2.18+*. Triggered on an overlay error.
--
onError :: (OverlayErrorEvent -> action) -> Attribute model action
onError action = on "error" overlayErrorDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onError', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Errored OverlayErrorEvent
--
-- view_ [ event (static (onErrorMain Errored)) ] [ "some view" ]
-- @
--
onErrorMain :: (OverlayErrorEvent -> action) -> EventHandler model action
onErrorMain action = onMain "error" overlayErrorDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onErrorMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Errored OverlayErrorEvent Model DOMRef
--
-- view_ [ event (static (onErrorMainWith Errored)) ] [ "some view" ]
-- @
--
onErrorMainWith :: (OverlayErrorEvent -> model -> DOMRef -> action) -> EventHandler model action
onErrorMainWith action = onMain "error" overlayErrorDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindoverlaytouch
--
-- Triggered when the overlay is touched.
--
onOverlayTouch :: (OverlayTouchEvent -> action) -> Attribute model action
onOverlayTouch action = on "overlaytouch" overlayTouchDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onOverlayTouch', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Touched OverlayTouchEvent
--
-- view_ [ event (static (onOverlayTouchMain Touched)) ] [ "some view" ]
-- @
--
onOverlayTouchMain :: (OverlayTouchEvent -> action) -> EventHandler model action
onOverlayTouchMain action = onMain "overlaytouch" overlayTouchDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onOverlayTouchMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Touched OverlayTouchEvent Model DOMRef
--
-- view_ [ event (static (onOverlayTouchMainWith Touched)) ] [ "some view" ]
-- @
--
onOverlayTouchMainWith :: (OverlayTouchEvent -> model -> DOMRef -> action) -> EventHandler model action
onOverlayTouchMainWith action = onMain "overlaytouch" overlayTouchDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindrequestclose
--
-- Triggered when the back button is clicked.
--
onRequestClose :: action -> Attribute model action
onRequestClose action = on "requestclose" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onRequestClose', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = RequestedClose
--
-- view_ [ event (static (onRequestCloseMain RequestedClose)) ] [ "some view" ]
-- @
--
onRequestCloseMain :: action -> EventHandler model action
onRequestCloseMain action = onMain "requestclose" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onRequestCloseMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = RequestedClose Model DOMRef
--
-- view_ [ event (static (onRequestCloseMainWith RequestedClose)) ] [ "some view" ]
-- @
--
onRequestCloseMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onRequestCloseMainWith action = onMain "requestclose" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/overlay.html#bindshowoverlay
--
-- Triggered when the overlay is displayed.
--
onShowOverlay :: action -> Attribute model action
onShowOverlay action = on "showoverlay" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onShowOverlay', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Shown
--
-- view_ [ event (static (onShowOverlayMain Shown)) ] [ "some view" ]
-- @
--
onShowOverlayMain :: action -> EventHandler model action
onShowOverlayMain action = onMain "showoverlay" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onShowOverlayMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Shown Model DOMRef
--
-- view_ [ event (static (onShowOverlayMainWith Shown)) ] [ "some view" ]
-- @
--
onShowOverlayMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onShowOverlayMainWith action = onMain "showoverlay" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | Like 'onDismissOverlay', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onDismissOverlayWith :: (DOMRef -> action) -> Attribute model action
onDismissOverlayWith action = on "dismissoverlay" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onError', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onErrorWith :: (OverlayErrorEvent -> DOMRef -> action) -> Attribute model action
onErrorWith action = on "error" overlayErrorDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onOverlayTouch', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onOverlayTouchWith :: (OverlayTouchEvent -> DOMRef -> action) -> Attribute model action
onOverlayTouchWith action = on "overlaytouch" overlayTouchDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onRequestClose', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onRequestCloseWith :: (DOMRef -> action) -> Attribute model action
onRequestCloseWith action = on "requestclose" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
-- | Like 'onShowOverlay', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onShowOverlayWith :: (DOMRef -> action) -> Attribute model action
onShowOverlayWith action = on "showoverlay" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
