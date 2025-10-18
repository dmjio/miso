-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Concurrent
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Concurrent
  ( -- * Synchronization primitives
    Waiter (..)
  , waiter
  , Mailbox
  , Mail
  , newMailbox
  , copyMailbox
  , cloneMailbox
  , sendMail
  , readMail
  ) where
-----------------------------------------------------------------------------
import Control.Concurrent
import Control.Concurrent.STM
import Data.Aeson
-----------------------------------------------------------------------------
-- | Synchronization primitive for event loop
data Waiter
  = Waiter
  { wait :: IO ()
    -- ^ Blocks on MVar
  , serve :: IO ()
    -- ^ Unblocks threads waiting on MVar
  }
-----------------------------------------------------------------------------
-- | Creates a new @Waiter@
waiter :: IO Waiter
waiter = do
  mvar <- newEmptyMVar
  pure Waiter
    { wait = takeMVar mvar
    , serve = do
        _ <- tryPutMVar mvar ()
        pure ()
    }
-----------------------------------------------------------------------------
-- | Type for expressing 'Mail' (or message payloads) put into a 'Mailbox' for delivery
type Mail = Value
-----------------------------------------------------------------------------
-- | Publish / Subscribe concurrency primitive
--
-- A 'Mailbox' is a broadcast 'TChan' that can express the following concurrency patterns
--
-- * Broadcast (one-to-all, 1:n)
-- * Multicast (one-to-many, 1:n)
-- * Unicast (one-to-one, 1:1)
--
-- All the above are supported as well in a bidirectional setting.
--
-- * Bidirectional (multicast \/ broadcast \/ unicast) (n:m)
--
-- Practically this pattern resembles cloud notifcation services like
--
-- * Amazon SNS
-- * Google Pub/Sub
--
type Mailbox = TChan Mail
-----------------------------------------------------------------------------
-- | Constructs a new 'Mailbox'
newMailbox :: IO Mailbox
newMailbox = newBroadcastTChanIO
-----------------------------------------------------------------------------
-- | Duplicates a 'Mailbox', all new 'Mail' is sent to all duplicated 'Mailbox'
copyMailbox :: Mailbox -> IO Mailbox
copyMailbox mailbox = atomically (dupTChan mailbox)
-----------------------------------------------------------------------------
-- | Duplicates a 'Mailbox', all new 'Mail' is sent to all cloned 'Mailbox'
-- Messages in original 'Mailbox' are retained (unlike `copyMailbox`).
cloneMailbox :: Mailbox -> IO Mailbox
cloneMailbox mailbox = atomically (cloneTChan mailbox)
-----------------------------------------------------------------------------
-- | Sends mail to a mailbox, all duplicated 'Mailbox' receive the same message.
sendMail :: Mailbox -> Mail -> IO ()
sendMail mailbox mail = atomically (writeTChan mailbox mail)
-----------------------------------------------------------------------------
-- | Reads mail from a 'Mailbox'. This only works on a duplicated 'Mailbox'.
-- So call this function only on 'Mailbox' that have been created from 'copyMailbox'.
readMail :: Mailbox -> IO Mail
readMail mailbox = atomically (readTChan mailbox)
-----------------------------------------------------------------------------
