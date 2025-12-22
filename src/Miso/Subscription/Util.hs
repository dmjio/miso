-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Util
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Subscription.Util
   ( -- ** Utilities
     createSub
   ) where
----------------------------------------------------------------------------
import           Control.Exception (bracket)
import           Control.Concurrent.MVar (newEmptyMVar, takeMVar)
-----------------------------------------------------------------------------
import           Miso.Effect
-----------------------------------------------------------------------------
-- | Utility function to allow resource finalization on 'Sub'.
createSub
  :: IO a
  -- ^ Acquire resource
  -> (a -> IO b)
  -- ^ Release resource
  -> Sub action
createSub acquire release = \_ -> do
  mvar <- newEmptyMVar
  bracket acquire release (\_ -> takeMVar mvar)
----------------------------------------------------------------------------
