{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE LambdaCase                #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Effect.Storage
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides an interface to the
-- [Web Storage API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API).
----------------------------------------------------------------------------
module Miso.Effect.Storage
  ( -- * Retrieve storage
    getLocalStorage
  , getSessionStorage
    -- * Set items in storage
  , setLocalStorage
  , setSessionStorage
    -- * Remove items from storage
  , removeLocalStorage
  , removeSessionStorage
    -- * Clear storage
  , clearLocalStorage
  , clearSessionStorage
    -- * Get number of items in storage
  , localStorageLength
  , sessionStorageLength
  ) where

import Data.Aeson     hiding (Object, String)
import Data.JSString
import GHCJS.Nullable
import GHCJS.Types

import Miso.FFI

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
getSessionStorage :: FromJSON model => JSString -> IO (Either String model)
getSessionStorage =
  getStorageCommon $ \t -> do
    r <- getItemSS t
    pure (nullableToMaybe r)

-- | Retrieve local storage
getLocalStorage :: FromJSON model => JSString -> IO (Either String model)
getLocalStorage = getStorageCommon $ \t -> do
    r <- getItemLS t
    pure (nullableToMaybe r)

-- | Set the value of a key in local storage.
--
-- @setLocalStorage key value@ sets the value of @key@ to @value@.
setLocalStorage :: ToJSON model => JSString -> model -> IO ()
setLocalStorage key model =
  setItemLS key =<< stringify model

-- | Set the value of a key in session storage.
--
-- @setSessionStorage key value@ sets the value of @key@ to @value@.
setSessionStorage :: ToJSON model => JSString -> model -> IO ()
setSessionStorage key model =
  setItemSS key =<< stringify model

foreign import javascript unsafe "$r = window.localStorage.getItem($1);"
  getItemLS :: JSString -> IO (Nullable JSVal)

foreign import javascript unsafe "$r = window.sessionStorage.getItem($1);"
  getItemSS :: JSString -> IO (Nullable JSVal)

-- | Removes item from local storage by key name.
foreign import javascript unsafe "window.localStorage.removeItem($1);"
  removeLocalStorage :: JSString -> IO ()

-- | Removes item from session storage by key name.
foreign import javascript unsafe "window.sessionStorage.removeItem($1);"
  removeSessionStorage :: JSString -> IO ()

foreign import javascript unsafe "window.localStorage.setItem($1, $2);"
  setItemLS :: JSString -> JSString -> IO ()

foreign import javascript unsafe "window.sessionStorage.setItem($1, $2);"
  setItemSS :: JSString -> JSString -> IO ()

-- | Retrieves the number of items in local storage.
foreign import javascript unsafe "$r = window.localStorage.length;"
  localStorageLength :: IO Int

-- | Retrieves the number of items in session storage.
foreign import javascript unsafe "$r = window.sessionStorage.length;"
  sessionStorageLength :: IO Int

-- | Clears local storage.
foreign import javascript unsafe "window.localStorage.clear();"
  clearLocalStorage :: IO ()

-- | Clears session storage.
foreign import javascript unsafe "window.sessionStorage.clear();"
  clearSessionStorage :: IO ()
