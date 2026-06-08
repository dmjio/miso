-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Hydrate
-- Copyright   :  (C) 2016-2026 David M. Johnson
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
-- | Hydrates a t'VTree' from the existing server-rendered DOM.
--
-- Walks the actual DOM under @domRef@ and populates the virtual tree
-- from the real DOM nodes instead of performing an initial render.
-- Returns 'True' if hydration succeeded, 'False' if a mismatch was detected.
hydrate
  :: LogLevel
  -- ^ Logging verbosity; 'DebugHydrate' or 'DebugAll' enables hydration logs
  -> DOMRef
  -- ^ The DOM element containing server-rendered HTML to hydrate from
  -> VTree
  -- ^ The initial virtual DOM tree (should match the server-rendered HTML)
  -> IO Bool
hydrate loggingLevel domRef vtree = do
  jval <- toJSVal vtree
  fromJSValUnchecked =<<
    FFI.hydrate (loggingLevel `elem` [DebugHydrate, DebugAll]) domRef jval
-----------------------------------------------------------------------------
