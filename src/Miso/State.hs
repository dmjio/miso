-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.State
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Similar to how one manages state in [React](https://react.dev/learn/managing-state),
-- 'miso' applications manage state with the @State@ monad.
--
-- The @State@ 'Monad' works well with @MonadState@ lenses as seen in 'Miso\.Lens' and the [lens](haddocks.haskell.org/pacakges/lens) library.
--
-- @
-- updateModel :: Action -> Effect Model Action
-- updateModel (AddOne event) = do
--   modify (+1)
--   io_ (consoleLog "Added One!")
-- @
--
-- This module re-exports select combinators from 'Control.Monad.RWS' and serves as a placeholder to add new state management combinators.
--
----------------------------------------------------------------------------
module Miso.State
  ( ask
  , modify
  , modify'
  , get
  , gets
  , put
  , tell
  ) where
----------------------------------------------------------------------------
import Control.Monad.RWS (get, gets, modify, modify', tell, put, ask)
----------------------------------------------------------------------------
