-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Svg.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Svg.Event
  ( -- *** Events
    onLoad
  , onLoadWith
    -- *** Event Map
  , svgEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.Types (EventHandler, DOMRef)
-----------------------------------------------------------------------------
svgEvents :: Events
svgEvents = backgroundEvents [ ("load", BUBBLE) ]
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/svg.html#bindload
--
-- Triggered when the SVG finishes loading.
--
onLoad :: action -> EventHandler action
onLoad action = on "load" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Like 'onLoad', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLoadWith :: (DOMRef -> action) -> EventHandler action
onLoadWith action = on "load" emptyDecoder (\() ref -> action ref)
-----------------------------------------------------------------------------
