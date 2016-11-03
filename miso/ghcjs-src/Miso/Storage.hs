{-# LANGUAGE ScopedTypeVariables       #-}
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

import Control.Monad
import Data.Aeson              hiding (Object, String)
import Data.JSString
import GHCJS.Types
import GHCJS.Marshal

getLocalStorage, getSessionStorage ::
  FromJSON model => JSString -> IO (Either String model)
getStorageCommon
  :: FromJSON b => (t -> IO JSVal) -> t -> IO (Either String b)
getStorageCommon f key = do
  result :: Maybe Value <- fromJSVal =<< f key
  pure $ case fromJSON <$> result of
    Nothing -> Left "Not Found"
    Just y -> case y of
      Error x -> Left x
      Success x -> Right x
getSessionStorage = getStorageCommon getItemSS
getLocalStorage = getStorageCommon getItemLS

setLocalStorage, setSessionStorage ::
  ToJSON model => JSString -> model -> IO ()
setLocalStorage key model =
  setItemLS key =<< do stringify <=< toJSVal $ toJSON model
setSessionStorage key model =
  setItemSS key =<< do stringify <=< toJSVal $ toJSON model

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

foreign import javascript unsafe "window.localStorage.clear();"
  clearLocalStorage :: IO ()

foreign import javascript unsafe "window.sessionStorage.clear();"
  clearSessionStorage :: IO ()

foreign import javascript unsafe "$r = JSON.stringify($1);"
 stringify :: JSVal -> IO JSString
