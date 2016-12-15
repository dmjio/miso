{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
module Miso.Event.Delegate where

import           Control.Concurrent.MVar
import qualified Data.Map                   as M
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object.Internal
import           Miso.Html.Internal

foreign import javascript unsafe "delegate($1, $2);"
  delegateEvent
     :: JSVal                     -- ^ Events
     -> Callback (IO JSVal)       -- ^ Virtual DOM callback
     -> IO ()

delegator
  :: MVar (VTree action)
  -> M.Map JSString Bool
  -> IO ()
delegator vtreeRef es = do
  evts <- toJSVal (M.toList es)
  getVTreeFromRef <- syncCallback' $ do
    VTree (Object val) <- readMVar vtreeRef
    pure val
  delegateEvent evts getVTreeFromRef
