-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Miso.JSON
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Helper for retrieving either local or session storage.
getStorageCommon
  :: FromJSON b
  => (t -> IO (Maybe JSVal))
  -> t
  -> IO (Either MisoString b)
getStorageCommon f key = do
  result <- f key
  case result of
    Nothing ->
      pure (Left "Not Found")
    Just v -> do
      s <- fromJSValUnchecked v
      pure (eitherDecode s)
-----------------------------------------------------------------------------
-- | Retrieves a value stored under the given key in session storage.
getSessionStorage
  :: FromJSON model
  => MisoString
  -> IO (Either MisoString model)
getSessionStorage =
  getStorageCommon $ \t -> do
    s <- sessionStorage
    r <- getItem s t
    fromJSVal r
-----------------------------------------------------------------------------
-- | Retrieves a value stored under the given key in local storage.
getLocalStorage
  :: FromJSON model
  => MisoString
  -> IO (Either MisoString model)
getLocalStorage = getStorageCommon $ \t -> do
    s <- localStorage
    r <- getItem s t
    fromJSVal r
-----------------------------------------------------------------------------
-- | Sets the value of a key in local storage.
--
-- @setLocalStorage key value@ sets the value of @key@ to @value@.
setLocalStorage
  :: ToJSON model
  => MisoString
  -> model
  -> IO ()
setLocalStorage key model = do
  s <- localStorage
  setItem s key (encode model)
-----------------------------------------------------------------------------
-- | Sets the value of a key in session storage.
--
-- @setSessionStorage key value@ sets the value of @key@ to @value@.
setSessionStorage
  :: ToJSON model
  => MisoString
  -> model
  -> IO ()
setSessionStorage key model = do
  s <- sessionStorage
  setItem s key (encode model)
-----------------------------------------------------------------------------
-- | Removes an item from local storage.
--
-- @removeLocalStorage key@ removes the value of @key@.
removeLocalStorage
  :: MisoString
  -> IO ()
removeLocalStorage key = do
  s <- localStorage
  removeItem s key
-----------------------------------------------------------------------------
-- | Removes an item from session storage.
--
-- @removeSessionStorage key@ removes the value of @key@.
removeSessionStorage
  :: MisoString
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
