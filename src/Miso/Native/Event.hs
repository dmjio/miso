-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Event
  ( -- * Events
    nativeEvents
  , nativeXEvents
  ) where
----------------------------------------------------------------------------
import           Miso.Native.Element.Frame.Event      (frameEvents)
import           Miso.Native.Element.Image.Event      (imageEvents)
import           Miso.Native.Element.List.Event       (listEvents)
import           Miso.Native.Element.ScrollView.Event (scrollViewEvents)
import           Miso.Native.Element.Text.Event       (textEvents)
import           Miso.Native.Element.View.Event       (viewEvents)
----------------------------------------------------------------------------
import           Miso.Native.X.Element.Input.Event    (inputEvents)
import           Miso.Native.X.Element.Overlay.Event  (overlayEvents)
import           Miso.Native.X.Element.Refresh.Event  (refreshEvents)
import           Miso.Native.X.Element.ScrollCoordinator.Event (scrollCoordinatorEvents)
import           Miso.Native.X.Element.Svg.Event      (svgEvents)
import           Miso.Native.X.Element.Textarea.Event (textareaEvents)
import           Miso.Native.X.Element.Viewpager.Event (viewpagerEvents)
import           Miso.Native.X.Element.Webview.Event  (webviewEvents)
----------------------------------------------------------------------------
import           Miso.Event                           (Events)
----------------------------------------------------------------------------
nativeEvents :: Events
nativeEvents = mconcat
  [ frameEvents
  , imageEvents
  , listEvents
  , scrollViewEvents
  , textEvents
  , viewEvents
  ]
----------------------------------------------------------------------------
nativeXEvents :: Events
nativeXEvents = mconcat
  [ inputEvents
  , overlayEvents
  , refreshEvents
  , scrollCoordinatorEvents
  , svgEvents
  , textareaEvents
  , viewpagerEvents
  , webviewEvents
  ]
----------------------------------------------------------------------------
