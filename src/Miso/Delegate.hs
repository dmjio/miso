-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Delegate
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Types and functions related to [event delegation](https://developer.mozilla.org/en-US/docs/Learn_web_development/Core/Scripting/Event_bubbling#event_delegation)
--
----------------------------------------------------------------------------
module Miso.Delegate
  ( delegator
  , undelegator
  ) where
-----------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef (IORef, readIORef)
import qualified Data.Map.Strict as M
import           Language.Javascript.JSaddle (create, JSM, JSVal, Object(..), ToJSVal(toJSVal))
import           Miso.Types (VTree(..))
import           Miso.String (MisoString)
import qualified Miso.FFI.Internal as FFI
-----------------------------------------------------------------------------
-- | Local Event type, used to create field names for a delegated event
data Event
  = Event
  { name :: MisoString
  -- ^ Event name
  , capture :: Bool
  -- ^ Capture settings for event
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Instance used to initialize event delegation
instance ToJSVal Event where
  toJSVal Event {..} = do
    o <- create
    flip (FFI.set "name") o =<< toJSVal name
    flip (FFI.set "capture") o =<< toJSVal capture
    toJSVal o
-----------------------------------------------------------------------------
-- | Entry point for event delegation
delegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> Bool
  -> JSM ()
delegator mountPointElement vtreeRef es debug = do
  evts <- toJSVal (uncurry Event <$> M.toList es)
  FFI.delegateEvent mountPointElement evts debug $ do
    VTree (Object vtree) <- liftIO (readIORef vtreeRef)
    pure vtree
-----------------------------------------------------------------------------
-- | Entry point for deinitalizing event delegation
undelegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> Bool
  -> JSM ()
undelegator mountPointElement vtreeRef es debug = do
  events <- toJSVal (M.toList es)
  FFI.undelegateEvent mountPointElement events debug $ do
    VTree (Object vtree) <- liftIO (readIORef vtreeRef)
    pure vtree
-----------------------------------------------------------------------------
