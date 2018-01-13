-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Delegate
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Delegate where

import           Data.IORef
import qualified Data.Map                 as M
import           Miso.Html.Internal
import           Miso.String
import           Miso.FFI
import qualified JavaScript.Object.Internal as OI
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types (JSVal)

-- | Entry point for event delegation
delegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> IO ()
delegator mountPointElement vtreeRef es = do
  evts <- toJSVal (M.toList es)
  getVTreeFromRef <- syncCallback' $ do
    VTree (OI.Object val) <- readIORef vtreeRef
    pure val
  delegateEvent mountPointElement evts getVTreeFromRef

