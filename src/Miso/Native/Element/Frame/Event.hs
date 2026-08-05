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
----------------------------------------------------------------------------
module Miso.Native.Element.Frame.Event
  ( -- *** Events
    onLoad
  , onLoadWith
  , onLoadMetrics
  , onLoadMetricsWith
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
import           Miso.Types (Attribute, DOMRef)
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
-- | Like 'onLoad', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLoadWith :: (FrameLoadEvent -> DOMRef -> action) -> Attribute model action
onLoadWith action = on "load" frameLoadDecoder $ \f _ domRef -> action f domRef
-----------------------------------------------------------------------------
-- | Like 'onLoadMetrics', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLoadMetricsWith :: (FrameLoadMetricsEvent -> DOMRef -> action) -> Attribute model action
onLoadMetricsWith action = on "loadmetrics" frameLoadMetricsDecoder $ \f _ domRef -> action f domRef
-----------------------------------------------------------------------------
