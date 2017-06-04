{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect.Storage
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Effect.Storage
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

-- | Retrieve local storage
getLocalStorage, getSessionStorage ::
  FromJSON model => JSString -> IO (Either String model)

-- | Helper for retrieving either local or session storage
getStorageCommon
  :: FromJSON b => (t -> IO JSVal) -> t -> IO (Either String b)
getStorageCommon f key = do
  result :: Maybe Value <- fromJSVal =<< f key
  pure $ case result of
    Nothing -> Left "Not Found"
    Just Null -> Left "Not Found"
    Just j ->
      case fromJSON j of
        Error x -> Left x
        Success x -> Right x
-- | Retrieve session storage
getSessionStorage = getStorageCommon getItemSS
-- | Retrieve local storage
getLocalStorage = getStorageCommon getItemLS

setLocalStorage, setSessionStorage ::
  ToJSON model => JSString -> model -> IO ()
-- | Set local storage
setLocalStorage key model =
  setItemLS key =<< do stringify <=< toJSVal $ toJSON model
-- | Set session storage
setSessionStorage key model =
  setItemSS key =<< do stringify <=< toJSVal $ toJSON model

foreign import javascript unsafe "$r = window.localStorage.getItem($1);"
  getItemLS :: JSString -> IO JSVal

foreign import javascript unsafe "$r = window.sessionStorage.getItem($1);"
  getItemSS :: JSString -> IO JSVal

-- | Removes item from local storage by key name
foreign import javascript unsafe "window.localStorage.removeItem($1);"
  removeLocalStorage :: JSString -> IO ()

-- | Removes item from session storage by key name
foreign import javascript unsafe "window.sessionStorage.removeItem($1);"
  removeSessionStorage :: JSString -> IO ()

foreign import javascript unsafe "window.localStorage.setItem($1, $2);"
  setItemLS :: JSString -> JSString -> IO ()

foreign import javascript unsafe "window.sessionStorage.setItem($1, $2);"
  setItemSS :: JSString -> JSString -> IO ()

-- | Retrieves the number of items in local storage
foreign import javascript unsafe "$r = window.localStorage.length;"
  localStorageLength :: IO Int

-- | Retrieves the number of items in session storage
foreign import javascript unsafe "$r = window.sessionStorage.length;"
  sessionStorageLength :: IO Int

-- | Clears local storage
foreign import javascript unsafe "window.localStorage.clear();"
  clearLocalStorage :: IO ()

-- | Clears session storage
foreign import javascript unsafe "window.sessionStorage.clear();"
  clearSessionStorage :: IO ()

foreign import javascript unsafe "$r = JSON.stringify($1);"
 stringify :: JSVal -> IO JSString
