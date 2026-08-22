-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Frame.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.Element.Frame.Event
  ( -- *** Events
    onLoad
  , onLoadWith
  , onLoadMain
  , onLoadMainWith
  , onLoadMetrics
  , onLoadMetricsWith
  , onLoadMetricsMain
  , onLoadMetricsMainWith
    -- *** Types
  , FrameLoadEvent (..)
  , FrameLoadMetricsEvent (..)
    -- *** Decoders
  , frameLoadDecoder
  , frameLoadMetricsDecoder
    -- *** Event Map
  , frameEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.String (MisoString)
import           Miso.Types (Attribute, EventHandler, DOMRef)
-----------------------------------------------------------------------------
frameEvents :: Events
frameEvents
  = M.fromList
  [ ("load", BUBBLE)
  , ("loadmetrics", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#bindload
--
-- Triggered when the embedded \<frame\> page finishes loading.
data FrameLoadEvent
  = FrameLoadEvent
  { loadStatusCode :: Int
    -- ^ The load status code
  , loadStatusMessage :: MisoString
    -- ^ The load status message
  , loadUrl :: MisoString
    -- ^ The url of the loaded \<frame\> resource
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
frameLoadDecoder :: Decoder FrameLoadEvent
frameLoadDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      FrameLoadEvent
        <$> o .: "statusCode"
        <*> o .: "statusMessage"
        <*> o .: "url"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#bindloadmetrics
--
-- Triggered with performance metrics for the embedded \<frame\> page load.
data FrameLoadMetricsEvent
  = FrameLoadMetricsEvent
  { metricsEntry :: Object
    -- ^ The @FrameLoadMetricsEntry@ payload
  , metricsMode :: MisoString
    -- ^ The load mode
  , metricsUrl :: MisoString
    -- ^ The url of the loaded \<frame\> resource
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
frameLoadMetricsDecoder :: Decoder FrameLoadMetricsEvent
frameLoadMetricsDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      FrameLoadMetricsEvent
        <$> o .: "entry"
        <*> o .: "mode"
        <*> o .: "url"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#bindload
--
-- @
--
-- data Action = HandleLoad FrameLoadEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = frame_ [ src_ "http://url", onLoad HandleLoad ] []
--
-- update :: Action -> Effect props Model Action
-- update (HandleLoad FrameLoadEvent {..}) =
--   io_ (consoleLog "frame load event received")
--
-- @
--
onLoad :: (FrameLoadEvent -> action) -> Attribute model action
onLoad action = on "load" frameLoadDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onLoad', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleLoad FrameLoadEvent
--
-- view_ [ event (static (onLoadMain HandleLoad)) ] [ "some view" ]
-- @
--
onLoadMain :: (FrameLoadEvent -> action) -> EventHandler model action
onLoadMain action = onMain "load" frameLoadDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onLoadMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleLoad FrameLoadEvent Model DOMRef
--
-- view_ [ event (static (onLoadMainWith HandleLoad)) ] [ "some view" ]
-- @
--
onLoadMainWith :: (FrameLoadEvent -> model -> DOMRef -> action) -> EventHandler model action
onLoadMainWith action = onMain "load" frameLoadDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#bindloadmetrics
--
-- @
--
-- data Action = HandleMetrics FrameLoadMetricsEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = frame_ [ src_ "http://url", onLoadMetrics HandleMetrics ] []
--
-- update :: Action -> Effect props Model Action
-- update (HandleMetrics FrameLoadMetricsEvent {..}) =
--   io_ (consoleLog "frame load metrics event received")
--
-- @
--
onLoadMetrics :: (FrameLoadMetricsEvent -> action) -> Attribute model action
onLoadMetrics action = on "loadmetrics" frameLoadMetricsDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onLoadMetrics', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleMetrics FrameLoadMetricsEvent
--
-- view_ [ event (static (onLoadMetricsMain HandleMetrics)) ] [ "some view" ]
-- @
--
onLoadMetricsMain :: (FrameLoadMetricsEvent -> action) -> EventHandler model action
onLoadMetricsMain action = onMain "loadmetrics" frameLoadMetricsDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onLoadMetricsMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleMetrics FrameLoadMetricsEvent Model DOMRef
--
-- view_ [ event (static (onLoadMetricsMainWith HandleMetrics)) ] [ "some view" ]
-- @
--
onLoadMetricsMainWith :: (FrameLoadMetricsEvent -> model -> DOMRef -> action) -> EventHandler model action
onLoadMetricsMainWith action = onMain "loadmetrics" frameLoadMetricsDecoder action
-----------------------------------------------------------------------------
-- | Like 'onLoad', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onLoadWith :: (FrameLoadEvent -> DOMRef -> action) -> Attribute model action
onLoadWith action = on "load" frameLoadDecoder $ \f _ domRef -> action f domRef
-----------------------------------------------------------------------------
-- | Like 'onLoadMetrics', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onLoadMetricsWith :: (FrameLoadMetricsEvent -> DOMRef -> action) -> Attribute model action
onLoadMetricsWith action = on "loadmetrics" frameLoadMetricsDecoder $ \f _ domRef -> action f domRef
-----------------------------------------------------------------------------
