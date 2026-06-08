-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Storage
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides an interface to the
-- [Web Storage API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API).
----------------------------------------------------------------------------
module Miso.Storage
  ( -- ** Local
    getLocalStorage
  , setLocalStorage
  , removeLocalStorage
  , clearLocalStorage
  , localStorageLength
    -- ** Session
  , getSessionStorage
  , setSessionStorage
  , removeSessionStorage
  , clearSessionStorage
  , sessionStorageLength
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void)
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Storage/getItem>
--
-- Retrieves a value from @sessionStorage@, returning 'Nothing' if the key does not exist.
getSessionStorage
  :: MisoString
  -- ^ Storage key to look up
  -> IO (Maybe MisoString)
getSessionStorage key = do
  fromJSValUnchecked =<< flip getItem key =<< sessionStorage
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Storage/getItem>
--
-- Retrieves a value from @localStorage@, returning 'Nothing' if the key does not exist.
getLocalStorage
  :: MisoString
  -- ^ Storage key to look up
  -> IO (Maybe MisoString)
getLocalStorage key =
  fromJSValUnchecked =<< flip getItem key =<< localStorage
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Storage/setItem>
--
-- Stores a value under the given key in @localStorage@, creating or overwriting the entry.
setLocalStorage
  :: MisoString
  -- ^ Key to store the value under
  -> MisoString
  -- ^ Value to persist
  -> IO ()
setLocalStorage key value = do
  s <- localStorage
  setItem s key value
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Storage/setItem>
--
-- Stores a value under the given key in @sessionStorage@, creating or overwriting the entry.
setSessionStorage
  :: MisoString
  -- ^ Key to store the value under
  -> MisoString
  -- ^ Value to persist
  -> IO ()
setSessionStorage key value = do
  s <- sessionStorage
  setItem s key value
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Storage/removeItem>
--
-- Removes the entry for the given key from @localStorage@. No-op if the key does not exist.
removeLocalStorage
  :: MisoString
  -- ^ Key to remove
  -> IO ()
removeLocalStorage key = do
  s <- localStorage
  removeItem s key
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Storage/removeItem>
--
-- Removes the entry for the given key from @sessionStorage@. No-op if the key does not exist.
removeSessionStorage
  :: MisoString
  -- ^ Key to remove
  -> IO ()
removeSessionStorage key = do
  s <- sessionStorage
  removeItem s key
-----------------------------------------------------------------------------
-- | Clears local storage.
--
-- @clearLocalStorage@ removes all values from local storage.
clearLocalStorage :: IO ()
clearLocalStorage = clear =<< localStorage
-----------------------------------------------------------------------------
-- | Clears session storage.
--
-- @clearSessionStorage@ removes all values from session storage.
clearSessionStorage :: IO ()
clearSessionStorage = clear =<< sessionStorage
-----------------------------------------------------------------------------
-- | Returns the number of items in local storage.
--
-- @localStorageLength@ returns the count of items in local storage
localStorageLength :: IO Int
localStorageLength = fromJSValUnchecked =<< localStorage ! "length"
-----------------------------------------------------------------------------
-- | Returns the number of items in session storage.
--
-- @sessionStorageLength@ returns the count of items in session storage
sessionStorageLength :: IO Int
sessionStorageLength = fromJSValUnchecked =<< sessionStorage ! "length"
-----------------------------------------------------------------------------
localStorage :: IO Storage
localStorage = Storage <$> (jsg "window" ! "localStorage")
-----------------------------------------------------------------------------
sessionStorage :: IO Storage
sessionStorage = Storage <$> (jsg "window" ! "sessionStorage")
-----------------------------------------------------------------------------
getItem :: Storage -> MisoString -> IO JSVal
getItem (Storage s) key = s # "getItem" $ [key]
-----------------------------------------------------------------------------
removeItem :: Storage -> MisoString -> IO ()
removeItem (Storage s) key = void $ s # "removeItem" $ [key]
-----------------------------------------------------------------------------
setItem :: Storage -> MisoString -> MisoString -> IO ()
setItem (Storage s) key val = do
  _ <- s # "setItem" $ (key, val)
  pure ()
-----------------------------------------------------------------------------
clear :: Storage -> IO ()
clear (Storage s) = do
  _ <- s # "clear" $ ()
  pure ()
-----------------------------------------------------------------------------
newtype Storage = Storage JSVal
  deriving (ToObject, ToJSVal)
-----------------------------------------------------------------------------
