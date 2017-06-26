{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE LambdaCase                #-}
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

import Data.Aeson     hiding (Object, String)
import Data.JSString
import GHCJS.Nullable
import GHCJS.Types

import Miso.FFI

-- | Retrieve local storage
getLocalStorage, getSessionStorage ::
  FromJSON model => JSString -> IO (Either String model)

-- | Helper for retrieving either local or session storage
getStorageCommon
  :: FromJSON b => (t -> IO (Maybe JSVal)) -> t -> IO (Either String b)
getStorageCommon f key = do
  result :: Maybe JSVal <- f key
  case result of
    Nothing -> pure $ Left "Not Found"
    Just v -> do
      r <- parse v
      pure $ case fromJSON r of
        Success x -> Right x
        Error y -> Left y

-- | Retrieve session storage
getSessionStorage =
  getStorageCommon $ \t -> do
    r <- getItemSS t
    pure (nullableToMaybe r)
-- | Retrieve local storage
getLocalStorage = getStorageCommon $ \t -> do
    r <- getItemLS t
    pure (nullableToMaybe r)

setLocalStorage, setSessionStorage ::
  ToJSON model => JSString -> model -> IO ()
-- | Set local storage
setLocalStorage key model =
  setItemLS key =<< stringify model
-- | Set session storage
setSessionStorage key model =
  setItemSS key =<< stringify model

foreign import javascript unsafe "$r = window.localStorage.getItem($1);"
  getItemLS :: JSString -> IO (Nullable JSVal)

foreign import javascript unsafe "$r = window.sessionStorage.getItem($1);"
  getItemSS :: JSString -> IO (Nullable JSVal)

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
