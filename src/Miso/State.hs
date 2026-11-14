-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.State
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.State" re-exports the t'Control.Monad.RWS.Strict.RWS' combinators that are
-- most useful inside an t'Miso.Effect.Effect' handler. Because
-- t'Miso.Effect.Effect' is an @RWS@ monad, the full
-- t'Control.Monad.State.Class.MonadState',
-- t'Control.Monad.Reader.Class.MonadReader', and
-- t'Control.Monad.Writer.Class.MonadWriter' interfaces are available
-- without importing @mtl@ directly.
--
-- This module is re-exported in its entirety by "Miso", so most
-- applications do not need to import it explicitly.
--
-- = Quick start
--
-- @
-- import "Miso"          -- re-exports Miso.State
-- -- or
-- import "Miso.State"    -- explicit import
--
-- data Model = Model { _count :: Int } deriving (Eq)
-- data Action = Increment | Decrement | Reset | Log
--
-- update :: Action -> t'Miso.Effect.Effect' p props Model Action
-- update Increment = 'modify' (\\m -> m { _count = _count m + 1 })
-- update Decrement = 'modify'' (\\m -> m { _count = _count m - 1 })
-- update Reset     = 'put' (Model 0)
-- update Log       = do
--   n <- 'gets' _count
--   'Miso.Effect.io_' (consoleLog ('Miso.String.ms' n))
-- @
--
-- When using "Miso.Lens" or "Miso.Lens.TH", the lens update operators
-- (@.=@, @+=@, @%=@, …) are built directly on 'modify', so
-- explicit calls to 'modify' \/ 'put' are rarely needed.
--
-- = Exported combinators
--
-- * __Reader__ (component metadata): 'ask', 'asks'
-- * __State__ (model): 'get', 'gets', 'modify', 'modify'', 'put'
-- * __Writer__ (schedule IO): 'tell'
-- * __IO lift__: 'liftIO'
--
-- = See also
--
-- * "Miso.Effect" — t'Miso.Effect.Effect', 'Miso.Effect.io', 'Miso.Effect.io_', 'Miso.Effect.sync'
-- * "Miso.Lens" — lens operators that wrap 'modify'
-- * "Miso.Lens.TH" — Template Haskell lens generation
----------------------------------------------------------------------------
module Miso.State
  ( ask
  , asks
  , modify
  , modify'
  , get
  , gets
  , put
  , tell
  , liftIO
  ) where
----------------------------------------------------------------------------
import Control.Monad.RWS (get, gets, modify, modify', tell, put, ask, asks)
import Control.Monad.IO.Class (liftIO)
----------------------------------------------------------------------------
