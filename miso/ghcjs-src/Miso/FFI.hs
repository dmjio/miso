{-# LANGUAGE LambdaCase #-}
module Miso.FFI where

import Data.Aeson
import Data.JSString
import GHCJS.Foreign.Callback
import GHCJS.Types
import GHCJS.Marshal

foreign import javascript unsafe "window.addEventListener($1, $2);"
  windowAddEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "window.removeEventListener($1, $2);"
  windowRemoveEventListener :: JSString -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$r = performance.now();"
  now :: IO Double

foreign import javascript unsafe "$r = JSON.stringify($1);"
  stringify' :: JSVal -> IO JSString

foreign import javascript unsafe "$r = JSON.parse($1);"
  parse' :: JSVal -> IO JSVal

stringify :: ToJSON json => json -> IO JSString
stringify j = stringify' =<< toJSVal (toJSON j)

parse :: FromJSON json => JSVal -> IO (Either String json)
parse jval = do
  Just val <- fromJSVal =<< parse' jval
  pure $ case fromJSON val of
    Success x -> Right x
    Error y -> Left y
