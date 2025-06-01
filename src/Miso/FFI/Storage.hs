-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.Storage
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI.Storage
  ( 
  ) where
-----------------------------------------------------------------------------
import Prelude hiding (length)
import Language.Javascript.JSaddle ((!), (#), JSM, fromJSValUnchecked, jsg, JSVal)
-----------------------------------------------------------------------------
import Miso.String hiding (length)
-----------------------------------------------------------------------------

