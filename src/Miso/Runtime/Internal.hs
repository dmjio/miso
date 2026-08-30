-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Runtime.Internal
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Runtime.Internal" is a __testing-only__ facade that re-exports
-- the miso runtime's global mutable state from the internal runtime module.
--
-- __Do not import this module in application code.__ Mutating any of
-- the exported 'Data.IORef.IORef' values will corrupt the component
-- lifecycle and produce undefined behaviour. The module exists solely to
-- give the @miso-tests@ integration-test package direct access to
-- component state for assertions.
--
-- = Exported names
--
-- * 'components' — global 'Data.IORef.IORef' mapping 'Miso.Effect.ComponentId'
--   to t'ComponentState' for every mounted component.
-- * 'componentIds' — monotonically increasing 'Data.IORef.IORef' used to
--   assign fresh component identifiers.
-- * 'rootComponentId' — the well-known identifier of the top-level app component.
-- * t'ComponentState' — record holding the live model, scheduler mailbox, and
--   other per-component runtime fields.
-- * 'schedulerThread' — 'Data.IORef.IORef' holding the 'Control.Concurrent.ThreadId'
--   of the event-loop scheduler thread.
-- * 'unmountComponent' / 'freeLifecycleHooks' — teardown functions normally
--   only invoked internally, exposed here so tests can exercise unmounting a
--   specific t'ComponentState' directly instead of going through a full
--   diff-driven removal.
--
-- = See also
--
-- * "Miso.Reload" — uses these internals to kill and restart the scheduler on @:r@
----------------------------------------------------------------------------
module Miso.Runtime.Internal
  ( components
  , componentIds
  , rootComponentId
  , ComponentState(..)
  , ComponentIds
  , schedulerThread
  , unmountComponent
  , freeLifecycleHooks
  , module FFI
  ) where
----------------------------------------------------------------------------
import Miso.Runtime (components, ComponentState(..), ComponentIds, componentIds, rootComponentId, schedulerThread, unmountComponent, freeLifecycleHooks)
import Miso.FFI.Internal as FFI
----------------------------------------------------------------------------
