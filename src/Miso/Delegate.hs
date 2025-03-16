-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Delegate
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Delegate
  ( delegator
  , undelegator
  ) where
-----------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, readIORef)
import qualified Data.Map.Strict as M
import           Language.Javascript.JSaddle (JSM, JSVal, Object(..), toJSVal)
import           Miso.FFI (delegateEvent, undelegateEvent)
import           Miso.Html.Types (VTree(..))
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Entry point for event delegation
delegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> JSM ()
delegator mountPointElement vtreeRef es = do
  evts <- toJSVal (M.toList es)
  delegateEvent mountPointElement evts $ do
    VTree (Object val) <- liftIO (readIORef vtreeRef)
    pure val
-----------------------------------------------------------------------------
-- | Entry point for deinitalizing event delegation
undelegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> JSM ()
undelegator mountPointElement vtreeRef es = do
  evts <- toJSVal (M.toList es)
  undelegateEvent mountPointElement evts $ do
    VTree (Object val) <- liftIO (readIORef vtreeRef)
    pure val
-----------------------------------------------------------------------------
