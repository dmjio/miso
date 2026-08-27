-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Refresh.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Refresh.Event
  ( -- *** Events
    onHeaderOffset
  , onHeaderOffsetWith
  , onHeaderOffsetMain
  , onHeaderOffsetMainWith
  , onRefreshStateChange
  , onRefreshStateChangeWith
  , onRefreshStateChangeMain
  , onRefreshStateChangeMainWith
  , onStartRefresh
  , onStartRefreshWith
  , onStartRefreshMain
  , onStartRefreshMainWith
    -- *** Types
  , HeaderOffsetEvent (..)
  , RefreshStateChangeEvent (..)
  , RefreshState (..)
  , StartRefreshEvent (..)
    -- *** Decoders
  , headerOffsetDecoder
  , refreshStateChangeDecoder
  , startRefreshDecoder
    -- *** Event Map
  , refreshEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.Types (Attribute, EventHandler, DOMRef)
-----------------------------------------------------------------------------
-- | The 'Events' map for the Lynx @<refresh>@ element.
--
-- Combine with other element maps using @<>@ and pass the result to
-- 'Miso.Native.native', so the delegator listens for these events.
--
-- @since 1.13.0.0
refreshEvents :: Events
refreshEvents
  = M.fromList
  [ ("headeroffset", BUBBLE)
  , ("refreshstatechange", BUBBLE)
  , ("startrefresh", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Payload of the @bindheaderoffset@ event.
data HeaderOffsetEvent
  = HeaderOffsetEvent
  { isDragging :: Bool
    -- ^ Whether the \<refresh-header\> is being dragged
  , offsetPercent :: Double
    -- ^ Ratio of the pull-down distance to the header's own height
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | The state of a \<refresh-header\>, mirrored from Lynx's @RefreshState@ enum.
--
-- @since 1.13.0.0
data RefreshState
  = Idle
  | OverDragRelease
  | Refreshing
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Numbering matches Lynx's @RefreshState@ enum (@IDLE = 0@ … @REFRESHING
-- = 2@), the shape the wire actually sends.
instance FromJSON RefreshState where
  parseJSON = withNumber "RefreshState" $ \case
    0 -> pure Idle
    1 -> pure OverDragRelease
    2 -> pure Refreshing
    x -> typeMismatch "RefreshState" (toJSON x)
-----------------------------------------------------------------------------
-- | Payload of the @bindrefreshstatechange@ event.
newtype RefreshStateChangeEvent
  = RefreshStateChangeEvent
  { state :: RefreshState
    -- ^ The @RefreshState@ of the \<refresh-header\>
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Payload of the @bindstartrefresh@ event.
newtype StartRefreshEvent
  = StartRefreshEvent
  { isManual :: Bool
    -- ^ Whether the @startrefresh@ event was triggered by a manual drag
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'HeaderOffsetEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
headerOffsetDecoder :: Decoder HeaderOffsetEvent
headerOffsetDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      HeaderOffsetEvent
        <$> o .: "isDragging"
        <*> o .: "offsetPercent"
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'RefreshStateChangeEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
refreshStateChangeDecoder :: Decoder RefreshStateChangeEvent
refreshStateChangeDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      RefreshStateChangeEvent <$> o .: "state"
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'StartRefreshEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
startRefreshDecoder :: Decoder StartRefreshEvent
startRefreshDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      StartRefreshEvent <$> o .: "isManual"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/refresh.html#bindheaderoffset
--
-- Triggered during movement while the \<refresh-header\> is exposed.
--
onHeaderOffset :: (HeaderOffsetEvent -> action) -> Attribute model action
onHeaderOffset action = on "headeroffset" headerOffsetDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onHeaderOffset', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Offset HeaderOffsetEvent
--
-- view_ [ event (static (onHeaderOffsetMain Offset)) ] [ "some view" ]
-- @
--
onHeaderOffsetMain :: (HeaderOffsetEvent -> action) -> EventHandler model action
onHeaderOffsetMain action = onMain "headeroffset" headerOffsetDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onHeaderOffsetMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Offset HeaderOffsetEvent Model DOMRef
--
-- view_ [ event (static (onHeaderOffsetMainWith Offset)) ] [ "some view" ]
-- @
--
onHeaderOffsetMainWith :: (HeaderOffsetEvent -> model -> DOMRef -> action) -> EventHandler model action
onHeaderOffsetMainWith action = onMain "headeroffset" headerOffsetDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/refresh.html#bindrefreshstatechange
--
-- Triggered when the \<refresh-header\> state changes.
--
onRefreshStateChange :: (RefreshStateChangeEvent -> action) -> Attribute model action
onRefreshStateChange action = on "refreshstatechange" refreshStateChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onRefreshStateChange', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = StateChanged RefreshStateChangeEvent
--
-- view_ [ event (static (onRefreshStateChangeMain StateChanged)) ] [ "some view" ]
-- @
--
onRefreshStateChangeMain :: (RefreshStateChangeEvent -> action) -> EventHandler model action
onRefreshStateChangeMain action = onMain "refreshstatechange" refreshStateChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onRefreshStateChangeMain', but the handler also receives read-only
-- access to the @model@ and the target element's 'DOMRef' (for imperative MTS
-- mutation).
--
-- @
-- data Action = StateChanged RefreshStateChangeEvent Model DOMRef
--
-- view_ [ event (static (onRefreshStateChangeMainWith StateChanged)) ] [ "some view" ]
-- @
--
onRefreshStateChangeMainWith :: (RefreshStateChangeEvent -> model -> DOMRef -> action) -> EventHandler model action
onRefreshStateChangeMainWith action = onMain "refreshstatechange" refreshStateChangeDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/refresh.html#bindstartrefresh
--
-- Triggered when the pull threshold is reached or @autoStartRefresh@ is called.
--
onStartRefresh :: (StartRefreshEvent -> action) -> Attribute model action
onStartRefresh action = on "startrefresh" startRefreshDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onStartRefresh', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Started StartRefreshEvent
--
-- view_ [ event (static (onStartRefreshMain Started)) ] [ "some view" ]
-- @
--
onStartRefreshMain :: (StartRefreshEvent -> action) -> EventHandler model action
onStartRefreshMain action = onMain "startrefresh" startRefreshDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onStartRefreshMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Started StartRefreshEvent Model DOMRef
--
-- view_ [ event (static (onStartRefreshMainWith Started)) ] [ "some view" ]
-- @
--
onStartRefreshMainWith :: (StartRefreshEvent -> model -> DOMRef -> action) -> EventHandler model action
onStartRefreshMainWith action = onMain "startrefresh" startRefreshDecoder action
-----------------------------------------------------------------------------
-- | Like 'onHeaderOffset', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onHeaderOffsetWith :: (HeaderOffsetEvent -> DOMRef -> action) -> Attribute model action
onHeaderOffsetWith action = on "headeroffset" headerOffsetDecoder $ \h _ domRef -> action h domRef
-----------------------------------------------------------------------------
-- | Like 'onRefreshStateChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onRefreshStateChangeWith :: (RefreshStateChangeEvent -> DOMRef -> action) -> Attribute model action
onRefreshStateChangeWith action = on "refreshstatechange" refreshStateChangeDecoder $ \h _ domRef -> action h domRef
-----------------------------------------------------------------------------
-- | Like 'onStartRefresh', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onStartRefreshWith :: (StartRefreshEvent -> DOMRef -> action) -> Attribute model action
onStartRefreshWith action = on "startrefresh" startRefreshDecoder $ \h _ domRef -> action h domRef
-----------------------------------------------------------------------------
