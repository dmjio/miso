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

import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.Map as M
import           GHCJS.Marshal
import           GHCJS.Types (JSVal)
import qualified JavaScript.Object.Internal as OI
import           Miso.FFI
import           Miso.Html.Types
import           Miso.String

-- | Entry point for event delegation
delegator
  :: JSVal
  -> IORef VTree
  -> M.Map MisoString Bool
  -> JSM ()
delegator mountPointElement vtreeRef es = do
  evts <- toJSVal (M.toList es)
  delegateEvent mountPointElement evts $ do
    VTree (OI.Object val) <- liftIO (readIORef vtreeRef)
    pure val
