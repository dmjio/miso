-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Example usage:
--
-- @
-- import Miso
-- import Miso.String
--
-- data Action = Add | Subtract
--
-- view_ :: Int -> View Int Action
-- view_ n = div_
--  [ class_ "main" ]
--  [ button_ [ onClick Add ] [ text_ "+" ]
--  , text_ (ms n)
--  , button_ [ onClick Subtract ] [ text_ "-" ]
--  ]
-- @
--
-- More information on how to use @miso@ is available on GitHub.
--
-- <http://github.com/dmjio/miso>
--
----------------------------------------------------------------------------
module Miso.Html
   ( -- ** Elements
     module Miso.Html.Element
     -- ** Events
   , module Miso.Html.Event
     -- ** Render
   , module Miso.Html.Render
   ) where
-----------------------------------------------------------------------------
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Render
-----------------------------------------------------------------------------
