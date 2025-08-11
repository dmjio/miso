-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import           Language.Javascript.JSaddle (JSM, bracket)
-----------------------------------------------------------------------------
import           Miso.Effect
-----------------------------------------------------------------------------
-- | Utility function to allow resource finalization on 'Sub'.
createSub
  :: JSM a
  -- ^ Acquire resource
  -> (a -> JSM b)
  -- ^ Release resource
  -> Sub action
createSub acquire release = \_ -> do
  mvar <- liftIO newEmptyMVar
  bracket acquire release (\_ -> liftIO (takeMVar mvar))
----------------------------------------------------------------------------
