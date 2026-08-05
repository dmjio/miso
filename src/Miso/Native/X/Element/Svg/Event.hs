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
  , onLoadMain
  , onLoadMainWith
    -- *** Event Map
  , svgEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.Types (Attribute, EventHandler, DOMRef)
-----------------------------------------------------------------------------
svgEvents :: Events
svgEvents = M.fromList [ ("load", BUBBLE) ]
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/svg.html#bindload
--
-- Triggered when the SVG finishes loading.
--
onLoad :: action -> Attribute model action
onLoad action = on "load" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onLoad', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Loaded
--
-- view_ [ event (static (onLoadMain Loaded)) ] [ "some view" ]
-- @
--
onLoadMain :: action -> EventHandler model action
onLoadMain action = onMain "load" emptyDecoder (\() _ _ -> action)
-----------------------------------------------------------------------------
-- | Like 'onLoadMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Loaded Model DOMRef
--
-- view_ [ event (static (onLoadMainWith Loaded)) ] [ "some view" ]
-- @
--
onLoadMainWith :: (model -> DOMRef -> action) -> EventHandler model action
onLoadMainWith action = onMain "load" emptyDecoder (\() m ref -> action m ref)
-----------------------------------------------------------------------------
-- | Like 'onLoad', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onLoadWith :: (DOMRef -> action) -> Attribute model action
onLoadWith action = on "load" emptyDecoder (\() _ ref -> action ref)
-----------------------------------------------------------------------------
