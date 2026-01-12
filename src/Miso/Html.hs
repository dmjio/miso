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
-- data IntAction = Add | Subtract
--
-- counterView :: Int -> View IntAction
-- counterView n = div_
--  [ class_ "main" ]
--  [ btn_ [ onClick Add ] [ text_ "+" ]
--  , text_ (ms n)
--  , btn_ [ onClick Subtract ] [ text_ "-" ]
--  ]
-- @
--
-- More information on how to use miso is available on GitHub
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
