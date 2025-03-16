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
--
-- data IntAction = Add | Subtract
--
-- intView :: Int -> View IntAction
-- intView n
--  = div_
--  [ class_ "main"
--  ]
--  [ btn_
--    [ onClick Add
--    ]
--    [ text_ "+"
--    ]
--  , text_ $ pack (show n)
--  , btn_
--    [ onClick Subtract
--    ]
--    [ text_ "-"
--    ]
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
     -- ** Attributes
   , module Miso.Html.Property
     -- ** Events
   , module Miso.Html.Event
     -- ** Virtual DOM
   , module Miso.Html.Types
   ) where
-----------------------------------------------------------------------------
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Types
import Miso.Html.Property
-----------------------------------------------------------------------------
