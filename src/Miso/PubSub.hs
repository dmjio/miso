-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.PubSub
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Publish / Subscribe primitives for communication between components.
----------------------------------------------------------------------------
module Miso.PubSub
  ( -- ** Publishers / Subscribers
    subscribe
  , unsubscribe
  , publish
  , Topic
  , topic
  ) where
----------------------------------------------------------------------------
import Miso.Runtime
----------------------------------------------------------------------------
