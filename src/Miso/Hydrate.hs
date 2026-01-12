-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Hydrate
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Functions and helpers for Virtual DOM hydration.
--
----------------------------------------------------------------------------
module Miso.Hydrate
  ( hydrate
  ) where
-----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Types
import           Miso.DSL
-----------------------------------------------------------------------------
-- | Hydration of a t'VTree'
hydrate :: LogLevel -> DOMRef -> VTree -> IO Bool
hydrate loggingLevel domRef vtree = do
  jval <- toJSVal vtree
  fromJSValUnchecked =<<
    FFI.hydrate (loggingLevel `elem` [DebugHydrate, DebugAll]) domRef jval
-----------------------------------------------------------------------------
