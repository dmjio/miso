module Miso.Delegate where

import           Data.IORef
import qualified Data.Map                 as M
import           Miso.Html.Internal
import           Miso.String
import           Miso.FFI
import qualified JavaScript.Object.Internal as OI
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal

-- | Entry point for event delegation
delegator
  :: IORef VTree
  -> M.Map MisoString Bool
  -> IO ()
delegator vtreeRef es = do
  evts <- toJSVal (M.toList es)
  getVTreeFromRef <- syncCallback' $ do
    VTree (OI.Object val) <- readIORef vtreeRef
    pure val
  delegateEvent evts getVTreeFromRef

