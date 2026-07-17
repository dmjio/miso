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
  ) where
----------------------------------------------------------------------------
import           Miso.Native.Element.Image.Event      (imageEvents)
import           Miso.Native.Element.List.Event       (listEvents)
import           Miso.Native.Element.ScrollView.Event (scrollViewEvents)
import           Miso.Native.Element.Text.Event       (textEvents)
import           Miso.Native.Element.View.Event       (viewEvents)
import           Miso.Event                           (Events)
----------------------------------------------------------------------------
nativeEvents :: Events
nativeEvents = mconcat
  [ imageEvents
  , listEvents
  , scrollViewEvents
  , textEvents
  , viewEvents
  ]
----------------------------------------------------------------------------
