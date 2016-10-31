{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
module Miso.Event.Delegate where

import qualified Data.Foldable          as F
import           Data.IORef
import           Data.Proxy
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object.Internal
import           Miso.Html.Internal
import           Miso.Types

foreign import javascript unsafe "delegate($1, $2, $3);"
  delegateEvent
     :: JSVal                     -- ^ Events
     -> Callback (IO JSVal)       -- ^ Virtual DOM callback
     -> Callback (JSVal -> IO ()) -- ^ FRP callback
     -> IO ()

delegator
  :: forall action events . ( ExtractEvents events, FromJSVal action )
  => (action -> IO ())
  -> IORef (VTree action)
  -> Proxy events
  -> IO ()
  -> IO ()
delegator writer vtreeRef prox notify = do
  evts <- toJSVal $ extractEvents prox
  getVTreeFromRef <- syncCallback' $ do
    VTree (Object val) <- readIORef vtreeRef
    pure val
  cb <- asyncCallback1 $ \e -> do
    flip F.forM_ writer =<< fromJSVal e
    notify
  delegateEvent evts getVTreeFromRef cb

