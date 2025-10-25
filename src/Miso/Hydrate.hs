-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
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
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import qualified Miso.FFI.Internal as FFI
import           Miso.Types
-----------------------------------------------------------------------------
-- | Hydration of a 'VTree'
hydrate :: LogLevel -> DOMRef -> VTree -> JSM ()
hydrate loggingLevel domRef vtree = do
  jval <- toJSVal vtree
  FFI.hydrate (loggingLevel `elem` [DebugHydrate, DebugAll]) domRef jval
-----------------------------------------------------------------------------
