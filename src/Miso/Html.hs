-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
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
-- intView n = div_ [ class_ "main" ] [
--    btn_ [ onClick Add ] [ text_ "+" ]
--  , text_ $ pack (show n)
--  , btn_ [ onClick Subtract ] [ text_ "-" ]
--  ]
-- @
--
-- More information on how to use `miso` is available on GitHub
--
-- <http://github.com/dmjio/miso>
--
----------------------------------------------------------------------------
module Miso.Html
   ( module Miso.Html.Element
   , module Miso.Html.Event
   , module Miso.Html.Internal
   , module Miso.Html.Property
   ) where

import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Internal
import Miso.Html.Property hiding (form_)
