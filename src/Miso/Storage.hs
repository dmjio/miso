-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
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
-- = Overview
--
-- "Miso.Storage" wraps the browser's
-- <https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API Web Storage API>,
-- providing access to both
-- <https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage localStorage>
-- and
-- <https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage sessionStorage>
-- through symmetric 'IO' functions.
--
-- Both stores map 'Miso.String.MisoString' keys to 'Miso.String.MisoString'
-- values (the Web Storage API only persists strings). Reads return
-- @'Maybe' 'Miso.String.MisoString'@ — 'Nothing' when the key is absent.
--
-- * __localStorage__ — persists across browser sessions until explicitly
--   cleared.
-- * __sessionStorage__ — persists only for the lifetime of the current
--   browser tab\/session; cleared automatically when the tab is closed.
--
-- = Quick start
--
-- @
-- import "Miso.Storage"
-- import "Miso.String" ('Miso.String.ms')
--
-- -- Persist a preference
-- saveTheme :: 'Miso.String.MisoString' -> IO ()
-- saveTheme theme = 'setLocalStorage' \"theme\" theme
--
-- -- Restore it on startup
-- loadTheme :: IO ('Maybe' 'Miso.String.MisoString')
-- loadTheme = 'getLocalStorage' \"theme\"
--
-- -- Session-scoped token (cleared when tab closes)
-- saveToken :: 'Miso.String.MisoString' -> IO ()
-- saveToken tok = 'setSessionStorage' \"auth_token\" tok
-- @
--
-- = API groups
--
-- * __localStorage__: 'getLocalStorage', 'setLocalStorage',
--   'removeLocalStorage', 'clearLocalStorage', 'localStorageLength'
-- * __sessionStorage__: 'getSessionStorage', 'setSessionStorage',
--   'removeSessionStorage', 'clearSessionStorage', 'sessionStorageLength'
--
-- = See also
--
-- * "Miso.Effect" — schedule storage reads\/writes with 'Miso.Effect.io' \/ 'Miso.Effect.io_'
-- * "Miso.Subscription.History" — URL-based state that survives page reloads
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
#ifdef NATIVE
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Miso.JSON (toJSON)
import           Miso.Native.Module (callNativeModule, callNativeModuleWith)
#endif
-----------------------------------------------------------------------------
-- | Retrieves a value stored under the given key in session storage.
getSessionStorage
  :: MisoString
  -- ^ Storage key to look up
  -> IO (Maybe MisoString)
getSessionStorage key = do
  fromJSValUnchecked =<< flip getItem key =<< sessionStorage
-----------------------------------------------------------------------------
-- | Retrieves a value stored under the given key in local storage.
getLocalStorage
  :: MisoString
  -- ^ Storage key to look up
  -> IO (Maybe MisoString)
#ifdef NATIVE
-- On Lynx the browser Web Storage API is unavailable; route through the
-- @NativeLocalStorageModule@ native module. Its @getStorageItem@ is
-- callback-based, so block on an 'MVar' until the native side responds.
getLocalStorage key = do
  var <- newEmptyMVar
  callNativeModuleWith "NativeLocalStorageModule" "getStorageItem" [toJSON key] (putMVar var)
  takeMVar var
#else
getLocalStorage key =
  fromJSValUnchecked =<< flip getItem key =<< localStorage
#endif
-----------------------------------------------------------------------------
-- | Sets the value of a key in local storage.
--
-- @setLocalStorage key value@ sets the value of @key@ to @value@.
setLocalStorage
  :: MisoString
  -- ^ Storage key to set
  -> MisoString
  -- ^ Value to store
  -> IO ()
#ifdef NATIVE
setLocalStorage key value =
  callNativeModule "NativeLocalStorageModule" "setStorageItem" [toJSON key, toJSON value]
#else
setLocalStorage key value = do
  s <- localStorage
  setItem s key value
#endif
-----------------------------------------------------------------------------
-- | Sets the value of a key in session storage.
--
-- @setSessionStorage key value@ sets the value of @key@ to @value@.
setSessionStorage
  :: MisoString
  -- ^ Storage key to set
  -> MisoString
  -- ^ Value to store
  -> IO ()
setSessionStorage key value = do
  s <- sessionStorage
  setItem s key value
-----------------------------------------------------------------------------
-- | Removes an item from local storage.
--
-- @removeLocalStorage key@ removes the value of @key@.
removeLocalStorage
  :: MisoString
  -- ^ Storage key to remove
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
  -- ^ Storage key to remove
  -> IO ()
removeSessionStorage key = do
  s <- sessionStorage
  removeItem s key
-----------------------------------------------------------------------------
-- | Clears local storage.
--
-- @clearLocalStorage@ removes all values from local storage.
clearLocalStorage :: IO ()
#ifdef NATIVE
clearLocalStorage = callNativeModule "NativeLocalStorageModule" "clearStorage" []
#else
clearLocalStorage = clear =<< localStorage
#endif
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
