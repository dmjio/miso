{-# LANGUAGE LambdaCase #-}
module Miso.FFI where

import           Data.Aeson hiding (Object)
import           Data.IORef
import           Data.JSString
import qualified Data.Map as M
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Array
import           JavaScript.Object.Internal
import           Miso.Html.Internal

foreign import javascript unsafe "window.addEventListener($1, $2);"
  windowAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "window.removeEventListener($1, $2);"
  windowRemoveEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

foreign import javascript unsafe "console.log($1);"
  consoleLog :: JSVal -> IO ()

foreign import javascript unsafe "$r = JSON.stringify($1);"
  stringify' :: JSVal -> IO JSString

foreign import javascript unsafe "$r = JSON.parse($1);"
  parse' :: JSVal -> IO JSVal

stringify :: ToJSON json => json -> IO JSString
{-# INLINE stringify #-}
stringify j = stringify' =<< toJSVal (toJSON j)

parse :: FromJSON json => JSVal -> IO (Either String json)
{-# INLINE parse #-}
parse jval = do
  Just val <- fromJSVal =<< parse' jval
  pure $ case fromJSON val of
    Success x -> Right x
    Error y -> Left y

foreign import javascript unsafe "$r = $1[$2];"
  item :: JSVal -> Int -> IO JSVal

foreign import javascript unsafe "$r = $1[$2].apply($1, $3);"
  applyFunction :: JSVal -> JSString -> JSArray -> IO JSVal

foreign import javascript unsafe "copyDOMIntoVTree($1);"
  copyDOMIntoVTree :: VTree -> IO ()

foreign import javascript unsafe "delegate($1, $2);"
  delegateEvent
     :: JSVal                     -- ^ Events
     -> Callback (IO JSVal)       -- ^ Virtual DOM callback
     -> IO ()

delegator
  :: IORef VTree
  -> M.Map JSString Bool
  -> IO ()
delegator vtreeRef es = do
  evts <- toJSVal (M.toList es)
  getVTreeFromRef <- syncCallback' $ do
    VTree (Object val) <- readIORef vtreeRef
    pure val
  delegateEvent evts getVTreeFromRef
 
