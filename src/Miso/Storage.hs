-----------------------------------------------------------------------------
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
import           Data.Aeson (FromJSON(..), ToJSON, fromJSON)
import qualified Data.Aeson as A
import           Language.Javascript.JSaddle hiding (obj, val)
-----------------------------------------------------------------------------
import           Miso.FFI.Internal (jsonParse, jsonStringify)
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
-- | Helper for retrieving either local or session storage.
getStorageCommon
  :: FromJSON b
  => (t -> JSM (Maybe JSVal))
  -> t
  -> JSM (Either String b)
getStorageCommon f key = do
  result <- f key
  case result of
    Nothing ->
      pure (Left "Not Found")
    Just v -> do
      r <- jsonParse v
      pure $ case fromJSON r of
        A.Success x -> Right x
        A.Error y -> Left y
-----------------------------------------------------------------------------
-- | Retrieves a value stored under the given key in session storage.
getSessionStorage
  :: FromJSON model
  => MisoString
  -> JSM (Either String model)
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
  -> JSM (Either String model)
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
  -> JSM ()
setLocalStorage key model = do
  s <- localStorage
  setItem s key =<< fromJSValUnchecked =<< jsonStringify model
-----------------------------------------------------------------------------
-- | Sets the value of a key in session storage.
--
-- @setSessionStorage key value@ sets the value of @key@ to @value@.
setSessionStorage
  :: ToJSON model
  => MisoString
  -> model
  -> JSM ()
setSessionStorage key model = do
  s <- sessionStorage
  setItem s key =<< fromJSValUnchecked =<< jsonStringify model
-----------------------------------------------------------------------------
-- | Removes an item from local storage.
--
-- @removeLocalStorage key@ removes the value of @key@.
removeLocalStorage
  :: MisoString
  -> JSM ()
removeLocalStorage key = do
  s <- localStorage
  removeItem s key
-----------------------------------------------------------------------------
-- | Removes an item from session storage.
--
-- @removeSessionStorage key@ removes the value of @key@.
removeSessionStorage
  :: MisoString
  -> JSM ()
removeSessionStorage key = do
  s <- sessionStorage
  removeItem s key
-----------------------------------------------------------------------------
-- | Clears local storage.
--
-- @clearLocalStorage@ removes all values from local storage.
clearLocalStorage :: JSM ()
clearLocalStorage = clear =<< localStorage
-----------------------------------------------------------------------------
-- | Clears session storage.
--
-- @clearSessionStorage@ removes all values from session storage.
clearSessionStorage :: JSM ()
clearSessionStorage = clear =<< sessionStorage
-----------------------------------------------------------------------------
-- | Returns the number of items in local storage.
--
-- @localStorageLength@ returns the count of items in local storage
localStorageLength :: JSM Int
localStorageLength = fromJSValUnchecked =<< localStorage ! (ms "length")
-----------------------------------------------------------------------------
-- | Returns the number of items in session storage.
--
-- @sessionStorageLength@ returns the count of items in session storage
sessionStorageLength :: JSM Int
sessionStorageLength = fromJSValUnchecked =<< sessionStorage ! (ms "length")
-----------------------------------------------------------------------------
localStorage :: JSM Storage
localStorage = Storage <$> (jsg "window" ! "localStorage")
-----------------------------------------------------------------------------
sessionStorage :: JSM Storage
sessionStorage = Storage <$> (jsg "window" ! "sessionStorage")
-----------------------------------------------------------------------------
getItem :: Storage -> MisoString -> JSM JSVal
getItem (Storage s) key = s # "getItem" $ [key]
-----------------------------------------------------------------------------
removeItem :: Storage -> MisoString -> JSM ()
removeItem (Storage s) key = void $ s # "removeItem" $ [key]
-----------------------------------------------------------------------------
setItem :: Storage -> MisoString -> MisoString -> JSM ()
setItem (Storage s) key val = do
  _ <- s # "setItem" $ (key, val)
  pure ()
-----------------------------------------------------------------------------
clear :: Storage -> JSM ()
clear (Storage s) = do
  _ <- s # "clear" $ ()
  pure ()
-----------------------------------------------------------------------------
newtype Storage = Storage JSVal
  deriving (MakeObject, ToJSVal)
-----------------------------------------------------------------------------
