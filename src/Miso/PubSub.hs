-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.PubSub
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Publish \/ Subscribe API for asynchronous communication between
-- t'Miso.Types.Component'. Use 'topic' to create a t'Topic', 'subscribe' to
-- receive messages, 'unsubscribe' to stop receiving, and 'publish' to send.
--
----------------------------------------------------------------------------
module Miso.PubSub
  ( -- * Pub\/Sub
    Topic
  , topic
  , subscribe
  , unsubscribe
  , publish
  ) where
----------------------------------------------------------------------------
import Miso.Runtime
----------------------------------------------------------------------------
