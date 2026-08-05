-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Refresh.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Refresh.Event
  ( -- *** Events
    onHeaderOffset
  , onHeaderOffsetWith
  , onRefreshStateChange
  , onRefreshStateChangeWith
  , onStartRefresh
  , onStartRefreshWith
    -- *** Types
  , HeaderOffsetEvent (..)
  , RefreshStateChangeEvent (..)
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
import           Miso.String (MisoString)
import           Miso.Types (Attribute, DOMRef)
-----------------------------------------------------------------------------
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
-- | Payload of the @bindrefreshstatechange@ event.
newtype RefreshStateChangeEvent
  = RefreshStateChangeEvent
  { state :: MisoString
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
headerOffsetDecoder :: Decoder HeaderOffsetEvent
headerOffsetDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      HeaderOffsetEvent
        <$> o .: "isDragging"
        <*> o .: "offsetPercent"
-----------------------------------------------------------------------------
refreshStateChangeDecoder :: Decoder RefreshStateChangeEvent
refreshStateChangeDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      RefreshStateChangeEvent <$> o .: "state"
-----------------------------------------------------------------------------
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
-- | https://lynxjs.org/api/elements/built-in/refresh.html#bindrefreshstatechange
--
-- Triggered when the \<refresh-header\> state changes.
--
onRefreshStateChange :: (RefreshStateChangeEvent -> action) -> Attribute model action
onRefreshStateChange action = on "refreshstatechange" refreshStateChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/refresh.html#bindstartrefresh
--
-- Triggered when the pull threshold is reached or @autoStartRefresh@ is called.
--
onStartRefresh :: (StartRefreshEvent -> action) -> Attribute model action
onStartRefresh action = on "startrefresh" startRefreshDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onHeaderOffset', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onHeaderOffsetWith :: (HeaderOffsetEvent -> DOMRef -> action) -> Attribute model action
onHeaderOffsetWith action = on "headeroffset" headerOffsetDecoder $ \h _ domRef -> action h domRef
-----------------------------------------------------------------------------
-- | Like 'onRefreshStateChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onRefreshStateChangeWith :: (RefreshStateChangeEvent -> DOMRef -> action) -> Attribute model action
onRefreshStateChangeWith action = on "refreshstatechange" refreshStateChangeDecoder $ \h _ domRef -> action h domRef
-----------------------------------------------------------------------------
-- | Like 'onStartRefresh', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onStartRefreshWith :: (StartRefreshEvent -> DOMRef -> action) -> Attribute model action
onStartRefreshWith action = on "startrefresh" startRefreshDecoder $ \h _ domRef -> action h domRef
-----------------------------------------------------------------------------
