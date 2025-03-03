{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE LambdaCase                #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Storage
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides an interface to the
-- [Web Storage API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API).
----------------------------------------------------------------------------
module Miso.Storage
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

import           Data.Aeson hiding (Object, String)

import           Miso.FFI
import           Miso.String

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

-- | Retrieve a value stored under given key in session storage
getSessionStorage :: FromJSON model => MisoString -> IO (Either String model)
getSessionStorage =
  getStorageCommon $ \t -> do
    s <- sessionStorage
    r <- getItem s t
    undefined -- fromJSVal r

-- | Retrieve a value stored under given key in local storage
getLocalStorage :: FromJSON model => MisoString -> IO (Either String model)
getLocalStorage = getStorageCommon $ \t -> do
    s <- localStorage
    r <- getItem s t
    undefined -- fromJSVal r

-- | Set the value of a key in local storage.
--
-- @setLocalStorage key value@ sets the value of @key@ to @value@.
setLocalStorage :: ToJSON model => MisoString -> model -> IO ()
setLocalStorage key model = do
  s <- localStorage
  setItem s key =<< stringify model

-- | Set the value of a key in session storage.
--
-- @setSessionStorage key value@ sets the value of @key@ to @value@.
setSessionStorage :: ToJSON model => MisoString -> model -> IO ()
setSessionStorage key model = do
  s <- sessionStorage
  setItem s key =<< stringify model

-- | Removes an item from local storage
--
-- @removeLocalStorage key@ removes the value of @key@.
removeLocalStorage :: MisoString -> IO ()
removeLocalStorage key = do
  s <- localStorage
  removeItem s key

-- | Removes an item from session storage.
--
-- @removeSessionStorage key@ removes the value of @key@.
removeSessionStorage :: MisoString -> IO ()
removeSessionStorage key = do
  s <- sessionStorage
  removeItem s key

-- | Clear local storage
--
-- @clearLocalStorage@ removes all values from local storage.
clearLocalStorage :: IO ()
clearLocalStorage = clearStorage =<< localStorage

-- | Clear session storage
--
-- @clearSessionStorage@ removes all values from session storage.
clearSessionStorage :: IO ()
clearSessionStorage = clearStorage =<< sessionStorage

-- | Local storage length
--
-- @localStorageLength@ returns the count of items in local storage
localStorageLength :: IO Int
localStorageLength = storageLength =<< localStorage

-- | Session storage length
--
-- @sessionStorageLength@ returns the count of items in session storage
sessionStorageLength :: IO Int
sessionStorageLength = storageLength =<< sessionStorage
