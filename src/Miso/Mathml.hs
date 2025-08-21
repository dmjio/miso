-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Mathml
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Example usage:
--
-- @
-- xSquared :: View action
-- xSquared  =
--     math_ []
--         [ msup_ []
--             [ mi_ [] [text "x"]
--             , mn_ [] [text "2"]
--             ]
--         ]
-- @
--
-- More complex example in [mathml](https://github.com/haskell-miso/miso-mathml).
--
----------------------------------------------------------------------------
module Miso.Mathml
   ( -- * Elements
     module Miso.Mathml.Element
   ) where
-----------------------------------------------------------------------------
import Miso.Mathml.Element
-----------------------------------------------------------------------------
