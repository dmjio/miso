-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Svg
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
-- import Miso.Svg
--
-- intView :: Int -> View IntAction
-- intView n = svg_ [ height_ "100", width "100" ] [
--    circle_ [ cx_ "50", cy_ "50", r_ "40", stroke_ "green", strokeWidth_ "4", fill_ "yellow" ] []
--  ]
-- @
--
-- More information on how to use miso is available on GitHub
--
-- <http://github.com/dmjio/miso>
--
----------------------------------------------------------------------------
module Miso.Svg
   ( -- ** Element
     module Miso.Svg.Element
     -- ** Property
   , module Miso.Svg.Property
     -- ** Event
   , module Miso.Svg.Event
   ) where
-----------------------------------------------------------------------------
import Miso.Svg.Property hiding (filter_, path_, mask_, clipPath_, cursor_, style_)
import Miso.Svg.Element
import Miso.Svg.Event
-----------------------------------------------------------------------------
