-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Types
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Types
  ( App (..)
  ) where

import qualified Data.Map           as M
import           Miso.Effect
import           Miso.Html.Internal

-- | Application entry point
data App model action = App
  { model :: model
  -- ^ initial model
  , update :: action -> model -> Effect model action
  -- ^ Function to update model, optionally provide effects
  , view :: model -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub action model ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: M.Map MisoString Bool
  -- ^ List of delegated events that the body element will listen for
  }

