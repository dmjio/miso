-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.CSS.Types
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
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
newtype StyleSheet = StyleSheet { getStyleSheet :: [Styles] }
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | Type for a CSS 'Style'
--
type Style = (MisoString, MisoString)
-----------------------------------------------------------------------------
-- | Type for a @Map@ of CSS 'Style'. Used with @StyleSheet@.
-- It maps CSS properties to their values.
data Styles
  = Styles (MisoString, [Style])
  | KeyFrame MisoString [(MisoString, [Style])]
  | Media MisoString [(MisoString, [Style])]
  deriving (Eq, Show)
-----------------------------------------------------------------------------
