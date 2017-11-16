-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription
  ( module Miso.Subscription.Mouse
  , module Miso.Subscription.Keyboard
  , module Miso.Subscription.History
  , module Miso.Subscription.WebSocket
  , module Miso.Subscription.Window
  , module Miso.Subscription.SSE
  , Sub, addSub
  ) where

import Data.IORef (readIORef)

import Miso.Subscription.Mouse
import Miso.Subscription.Keyboard
import Miso.Subscription.History
import Miso.Subscription.WebSocket
import Miso.Subscription.Window
import Miso.Subscription.SSE

import Miso.Html.Internal (Sub)
import Miso.Types (AppContext(..), writeEvent)

-- | Add a subscription to a running app
addSub :: AppContext action model -> Sub action model -> IO ()
addSub ctx sub = sub (readIORef $ modelRef ctx) (writeEvent ctx)
