{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
module Miso.Event.Delegate where

import qualified Data.Foldable          as F
import qualified Data.Map as M
import           Data.IORef
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object.Internal
import           Miso.Html.Internal

foreign import javascript unsafe "delegate($1, $2, $3);"
  delegateEvent
     :: JSVal                     -- ^ Events
     -> Callback (IO JSVal)       -- ^ Virtual DOM callback
     -> Callback (JSVal -> IO ()) -- ^ FRP callback
     -> IO ()

delegator
  :: forall action . FromJSVal action
  => (action -> IO ())
  -> IORef (VTree action)
  -> M.Map JSString Bool
  -> IO ()
delegator writer vtreeRef es = do
  evts <- toJSVal (M.toList es)
  getVTreeFromRef <- syncCallback' $ do
    VTree (Object val) <- readIORef vtreeRef
    pure val
  cb <- asyncCallback1 $ \e ->
    flip F.forM_ writer =<< fromJSVal e
  delegateEvent evts getVTreeFromRef cb
