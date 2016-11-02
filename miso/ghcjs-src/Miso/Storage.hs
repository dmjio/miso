{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
module Miso.Storage
  ( -- * Local and Session Storage APIs
    --- * Get storage
    getLocalStorage
  , getSessionStorage
    --- * Set storage
  , setLocalStorage
  , setSessionStorage
    --- * Remove storage
  , removeLocalStorage
  , removeSessionStorage
    --- * Clear storage
  , clearLocalStorage
  , clearSessionStorage
    --- * Get storage length
  , localStorageLength
  , sessionStorageLength
  ) where

import Data.Aeson              hiding (Object, String)
import Data.JSString
import Data.JSString.Text
import Data.String.Conversions
import GHCJS.Foreign
import GHCJS.Types
import Miso.Html.Internal
import Unsafe.Coerce

getLocalStorage, getSessionStorage ::
  FromJSON model => MisoString -> IO (Either String model)
getStorageCommon
  :: (Monad m, FromJSON b) => (t -> m JSVal) -> t -> m (Either String b)
getStorageCommon f key = do
  result <- f key
  pure $ case jsTypeOf result of
    Object -> Left "Not found"
    String -> eitherDecode . cs $ textFromJSString (unsafeCoerce result)
    _ -> Left "Unknown JS type, cannot decode"
getSessionStorage = getStorageCommon getItemSS
getLocalStorage = getStorageCommon getItemLS

setLocalStorage, setSessionStorage ::
  ToJSON model => MisoString -> model -> IO ()
setLocalStorage key model = setItemLS key val
  where
    val = textToJSString $ cs (encode model)
setSessionStorage key model = setItemSS key val
  where
    val = textToJSString $ cs (encode model)

foreign import javascript unsafe "$r = window.localStorage.getItem($1);"
  getItemLS :: JSString -> IO JSVal

foreign import javascript unsafe "$r = window.sessionStorage.getItem($1);"
  getItemSS :: JSString -> IO JSVal

foreign import javascript unsafe "window.localStorage.removeItem($1);"
  removeLocalStorage :: JSString -> IO ()

foreign import javascript unsafe "window.sessionStorage.removeItem($1);"
  removeSessionStorage :: JSString -> IO ()

foreign import javascript unsafe "window.localStorage.setItem($1, $2);"
  setItemLS :: JSString -> JSString -> IO ()

foreign import javascript unsafe "window.sessionStorage.setItem($1, $2);"
  setItemSS :: JSString -> JSString -> IO ()

foreign import javascript unsafe "$r = window.localStorage.length;"
  localStorageLength :: IO Int

foreign import javascript unsafe "$r = window.sessionStorage.length;"
  sessionStorageLength :: IO Int

foreign import javascript unsafe "window.localStorage.clear()"
  clearLocalStorage :: IO ()

foreign import javascript unsafe "window.sessionStorage.clear()"
  clearSessionStorage :: IO ()
