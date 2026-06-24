-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Html" is the HTML DSL re-export hub. It collects element smart
-- constructors, pre-wired event handlers, and the server-side rendering
-- typeclass into a single convenient import.
--
-- The top-level "Miso" module already re-exports everything from
-- "Miso.Html", so applications that @import Miso@ have the entire HTML
-- layer in scope without an additional import.  Import "Miso.Html"
-- directly only when you want the HTML DSL in isolation — for example,
-- in a view-only library that should not depend on the miso runtime.
--
-- __Note:__ "Miso.Html.Property" (@'Miso.Html.Property.id_'@,
-- @'Miso.Html.Property.class_'@, @'Miso.Html.Property.href_'@, …) is
-- /not/ re-exported here.  Import it separately, or use the top-level
-- "Miso" import which includes everything.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Html.Property" ('Miso.Html.Property.class_')
--
-- data Action = Increment | Decrement | Reset
--
-- view :: Int -> 'Miso.Types.View' Int Action
-- view n =
--   'div_' [ 'Miso.Html.Property.class_' \"counter\" ]
--     [ 'h1_' [] [ 'Miso.text' \"Counter\" ]
--     , 'p_'  [] [ 'Miso.text' ('Miso.String.ms' n) ]
--     , 'button_' [ 'onClick' Increment ] [ 'Miso.text' \"+\" ]
--     , 'button_' [ 'onClick' Decrement ] [ 'Miso.text' \"-\" ]
--     , 'button_' [ 'onClick' Reset ]     [ 'Miso.text' \"Reset\" ]
--     ]
-- @
--
-- = Re-exported modules
--
-- ["Miso.Html.Element"]
--   Smart constructors for every standard HTML element (@'div_'@,
--   @'button_'@, @'input_'@, @'table_'@, …).  All names are suffixed
--   with @_@ to avoid clashing with 'Prelude' identifiers.
--
-- ["Miso.Html.Event"]
--   Pre-wired event-handler attributes (@'onClick'@, @'onInput'@,
--   @'onKeyDown'@, @'onDrop'@, …).  Covers mouse, keyboard, form, focus,
--   pointer, drag, touch, media, and lifecycle events.
--
-- ["Miso.Html.Render"]
--   The 'Miso.Html.Render.ToHtml' typeclass for serialising a
--   'Miso.Types.View' tree to a lazy @ByteString@ of UTF-8 HTML
--   (server-side rendering \/ SSR).
--
-- = See also
--
-- * "Miso.Html.Element" — full element reference with groupings
-- * "Miso.Html.Event" — full event-handler reference with naming conventions
-- * "Miso.Html.Property" — DOM properties and attributes (@'Miso.Html.Property.id_'@,
--   @'Miso.Html.Property.class_'@, @'Miso.Html.Property.src_'@, …)
-- * "Miso.Html.Render" — SSR rendering rules and @-fssr@ flag details
-- * "Miso.Svg" — SVG element, event, and property combinators
-- * "Miso.CSS" — structured CSS property DSL
-- * "Miso" — complete miso API including runtime, routing, and FFI
--
-- More information and examples are available at <http://github.com/dmjio/miso>.
--
----------------------------------------------------------------------------
module Miso.Html
   ( -- * Elements
     module Miso.Html.Element
     -- * Events
   , module Miso.Html.Event
     -- * Rendering
   , module Miso.Html.Render
   ) where
-----------------------------------------------------------------------------
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Render
-----------------------------------------------------------------------------
