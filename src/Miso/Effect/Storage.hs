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

import           Data.Aeson hiding (Object, String)
import           GHCJS.Marshal
import           GHCJS.Types

import           Miso.FFI
import qualified Miso.FFI.Storage as Storage
import           Miso.String

-- | Helper for retrieving either local or session storage
getStorageCommon
  :: FromJSON b => (t -> JSM (Maybe JSVal)) -> t -> JSM (Either String b)
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
getSessionStorage :: FromJSON model => MisoString -> JSM (Either String model)
getSessionStorage =
  getStorageCommon $ \t -> do
    s <- Storage.sessionStorage
    r <- Storage.getItem s t
    fromJSVal r

-- | Retrieve a value stored under given key in local storage
getLocalStorage :: FromJSON model => MisoString -> JSM (Either String model)
getLocalStorage = getStorageCommon $ \t -> do
    s <- Storage.localStorage
    r <- Storage.getItem s t
    fromJSVal r

-- | Set the value of a key in local storage.
--
-- @setLocalStorage key value@ sets the value of @key@ to @value@.
setLocalStorage :: ToJSON model => MisoString -> model -> JSM ()
setLocalStorage key model = do
  s <- Storage.localStorage
  Storage.setItem s key =<< stringify model

-- | Set the value of a key in session storage.
--
-- @setSessionStorage key value@ sets the value of @key@ to @value@.
setSessionStorage :: ToJSON model => MisoString -> model -> JSM ()
setSessionStorage key model = do
  s <- Storage.sessionStorage
  Storage.setItem s key =<< stringify model

-- | Removes an item from local storage
--
-- @removeLocalStorage key@ removes the value of @key@.
removeLocalStorage :: MisoString -> JSM ()
removeLocalStorage key = do
  s <- Storage.localStorage
  Storage.removeItem s key

-- | Removes an item from session storage.
--
-- @removeSessionStorage key@ removes the value of @key@.
removeSessionStorage :: MisoString -> JSM ()
removeSessionStorage key = do
  s <- Storage.sessionStorage
  Storage.removeItem s key

-- | Clear local storage
--
-- @clearLocalStorage@ removes all values from local storage.
clearLocalStorage :: JSM ()
clearLocalStorage = Storage.clear =<< Storage.localStorage

-- | Clear session storage
--
-- @clearSessionStorage@ removes all values from session storage.
clearSessionStorage :: JSM ()
clearSessionStorage = Storage.clear =<< Storage.sessionStorage

-- | Local storage length
--
-- @localStorageLength@ returns the count of items in local storage
localStorageLength :: JSM Int
localStorageLength = Storage.length =<< Storage.localStorage

-- | Session storage length
--
-- @sessionStorageLength@ returns the count of items in session storage
sessionStorageLength :: JSM Int
sessionStorageLength = Storage.length =<< Storage.sessionStorage
