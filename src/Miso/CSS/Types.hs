-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.CSS.Types
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.CSS.Types" defines the core data types that back the CSS DSL in
-- "Miso.CSS". There are three layers:
--
-- * 'Style' — a single CSS property\/value pair (@(\"color\", \"red\")@).
-- * 'Styles' — one rule block: either a selector rule, a
--   <https://developer.mozilla.org/en-US/docs/Web/CSS/@keyframes \@keyframes>
--   animation, or a
--   <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_media_queries \@media>
--   query.
-- * 'StyleSheet' — an ordered list of 'Styles' rule blocks that together form
--   a complete
--   <https://developer.mozilla.org/en-US/docs/Web/API/StyleSheet stylesheet>.
--
-- In normal usage you never construct these types directly — the smart
-- constructors in "Miso.CSS" (@sheet_@, @selector_@, @keyframes_@, @media_@)
-- build them for you. This module is exported for downstream code that
-- inspects or extends the CSS representation.
--
-- = Type hierarchy
--
-- @
-- 'StyleSheet'          -- rendered to a \<style\> tag
--   └─ ['Styles']       -- one rule block each
--        ├─ 'Styles'    (selector → ['Style'])
--        ├─ 'KeyFrame'  (animation-name → [(stop, ['Style'])])
--        └─ 'Media'     (media-query   → [(selector, ['Style'])])
--
-- 'Style' = ('MisoString', 'MisoString')   -- property, value
-- @
--
-- = See also
--
-- * "Miso.CSS" — smart constructors and property combinators built on these types
-- * "Miso.CSS.Color" — 'Miso.CSS.Color.Color' type used as property values
-----------------------------------------------------------------------------
module Miso.CSS.Types
  ( -- *** Types
    Style
  , Styles (..)
  , StyleSheet (..)
  , TransformFn (..)
  ) where
-----------------------------------------------------------------------------
import Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Type for a CSS StyleSheet. Internally it maps From CSS selectors to t'Styles'.
--
-- @
-- testSheet :: StyleSheet
-- testSheet =
--    sheet_
--    [ selector_ ".name"
--        [ backgroundColor red
--        , alignContent "top"
--        ]
--    , selector_ "#container"
--        [ backgroundColor blue
--        , alignContent "center"
--        ]
--    , keyframes_ "slide-in"
--      [ "from" =:
--        [ transform "translateX(0%)"
--        ]
--      , "to" =:
--        [ transform "translateX(100%)"
--        , backgroundColor red
--        , backgroundSize "10px"
--        , backgroundRepeat "true"
--        ]
--      , pct 10 =:
--        [ "foo" =: "bar"
--        ]
--      ]
--    , media_ "screen and (min-width: 480px)"
--      [ "header" =:
--        [ height "auto"
--        ]
--      , "ul" =:
--        [ display "block"
--        ]
--      ]
--    ]
-- @
--
newtype StyleSheet = StyleSheet
  { getStyleSheet :: [Styles]
  -- ^ Ordered list of CSS rule blocks that make up the stylesheet
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | Type for a CSS 'Style'
--
type Style = (MisoString, MisoString)
-----------------------------------------------------------------------------
-- | An individual CSS <https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function transform function>.
-- Construct values with @translate@, @rotate@, @scale@, etc., then combine with @transforms@.
--
-- @
-- transforms [ translate (px 10) (pct 50), rotate (deg 45), scaleX 1.5 ]
-- @
--
newtype TransformFn = TransformFn
  { renderTransformFn :: MisoString
  -- ^ The serialised CSS transform function string (e.g. @\"rotate(45deg)\"@)
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | A CSS rule block. One of: a selector rule, a @\@keyframes@ animation, or a @\@media@ query.
data Styles
  = Styles (MisoString, [Style])
  | KeyFrame MisoString [(MisoString, [Style])]
  | Media MisoString [(MisoString, [Style])]
  deriving (Eq, Show)
-----------------------------------------------------------------------------
