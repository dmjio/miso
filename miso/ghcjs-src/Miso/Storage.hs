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

import           Data.Aeson
import           Data.JSString
import           Data.JSString.Text
import           Data.String.Conversions
import           Miso.Html.Internal

getLocalStorage, getSessionStorage ::
  FromJSON model => MisoString -> IO (Either String model)
getLocalStorage key =
   eitherDecode . cs . textFromJSString <$> getItemLS key
getSessionStorage key =
   eitherDecode . cs . textFromJSString <$> getItemSS key

setLocalStorage, setSessionStorage ::
  ToJSON model => MisoString -> model -> IO ()
setLocalStorage key model = setItemLS key val
  where
    val = textToJSString $ cs (encode model)
setSessionStorage key model = setItemSS key val
  where
    val = textToJSString $ cs (encode model)

foreign import javascript unsafe "$r = window.localStorage.getItem($1);"
  getItemLS :: JSString -> IO JSString

foreign import javascript unsafe "$r = window.sessionStorage.getItem($1);"
  getItemSS :: JSString -> IO JSString

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
