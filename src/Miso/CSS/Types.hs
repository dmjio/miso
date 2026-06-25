-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.CSS.Types
-- Copyright   :  (C) 2016-2026 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Types for CSS, including inline styles and [StyleSheets](https://developer.mozilla.org/en-US/docs/Web/API/StyleSheet).
--
----------------------------------------------------------------------------
module Miso.CSS.Types
  ( -- *** Types
    Style
  , Styles (..)
  , StyleSheet (..)
  , TransformFn (..)
  , KeyframeStop (..)
  , MediaRule (..)
  , MediaQuery (..)
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
--      [ from_ [ transforms [ translateX (pct 0) ] ]
--      , at (pct 50)
--        [ backgroundColor red
--        , backgroundSize "10px"
--        ]
--      , to_ [ transforms [ translateX (pct 100) ] ]
--      ]
--    , media_ (screen_ `and_` minWidth_ (px 480))
--      [ rule_ "header" [ height "auto" ]
--      , rule_ "ul"     [ display "block" ]
--      ]
--    ]
-- @
--
newtype StyleSheet = StyleSheet { getStyleSheet :: [Styles] }
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | Type for a CSS 'Style'
--
type Style = (MisoString, MisoString)
-----------------------------------------------------------------------------
-- | An individual CSS [transform function](https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function).
-- Construct values with 'translate', 'rotate', 'scale', etc., then combine with 'transforms'.
--
-- @
-- transforms [ translate (px 10) (pct 50), rotate (deg 45), scaleX 1.5 ]
-- @
--
newtype TransformFn = TransformFn { renderTransformFn :: MisoString }
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | Type for a @Map@ of CSS 'Style'. Used with @StyleSheet@.
-- It maps CSS properties to their values.
data Styles
  = Styles (MisoString, [Style])
  | KeyFrame MisoString [(MisoString, [Style])]
  | Media MisoString [(MisoString, [Style])]
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | A single stop in a '@keyframes' rule. Construct with 'from_', 'to_', or 'at'.
newtype KeyframeStop = KeyframeStop { getKeyframeStop :: (MisoString, [Style]) }
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | A selector rule inside a '@media' block. Construct with 'rule_'.
newtype MediaRule = MediaRule { getMediaRule :: (MisoString, [Style]) }
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | A CSS [media query](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_media_queries).
-- Construct with 'screen_', 'print_', 'all_', 'minWidth_', etc.,
-- and compose with 'and_', 'or_', 'not_'.
newtype MediaQuery = MediaQuery { renderMediaQuery :: MisoString }
  deriving (Eq, Show)
-----------------------------------------------------------------------------
