-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.PubSub
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.PubSub" provides a lightweight publish\/subscribe channel for
-- passing messages between independent 'Miso.Types.Component' trees that
-- do not share a parent-child relationship.
--
-- A 'Topic' is an untyped broadcast channel. Any component can
-- 'publish' a message to it; every component that has called 'subscribe'
-- on that topic will receive the message as an action.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.PubSub"
--
-- -- 1. Create a shared topic (typically at the top level or in a shared module)
-- chatTopic :: IO 'Topic'
-- chatTopic = 'topic'
--
-- -- 2. Subscribe in a component's subs list
-- myChatSub :: 'Topic' -> 'Miso.Effect.Sub' Action
-- myChatSub t = 'subscribe' t GotMessage
--
-- -- 3. Publish from any component's update function
-- update :: Action -> 'Miso.Effect.Effect' p props Model Action
-- update (SendMessage msg) =
--   'Miso.Effect.io_' ('publish' chatTopic msg)
-- update (GotMessage msg) = do
--   ...
-- @
--
-- = API
--
-- * 'topic' — create a new broadcast channel
-- * 'subscribe' — register a component subscription that receives published values
-- * 'unsubscribe' — deregister a subscription
-- * 'publish' — broadcast a value to all current subscribers
--
-- = See also
--
-- * "Miso.Binding" — lens-based parent↔child model synchronisation
-- * "Miso.Effect" — 'Miso.Effect.Sub', 'Miso.Effect.withSink'
-- * "Miso.Runtime" — where 'Topic', 'subscribe', 'publish' are defined
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
