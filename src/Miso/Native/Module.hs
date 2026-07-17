-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Module
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Bindings to Lynx <https://lynxjs.org/guide/use-native-modules.html native modules>.
--
-- Native modules are exposed to JavaScript through a single global
-- @NativeModules@ object and support two call shapes:
--
--   * __synchronous / void__ — @NativeModules.\<module\>.\<method\>(args…)@
--   * __asynchronous__ — @NativeModules.\<module\>.\<method\>(args…, callback)@,
--     where the native side invokes @callback@ with the result.
--
-- __N.B.__ Per the Lynx documentation, native modules can /only/ be used on the
-- background thread (BTS). These are plain 'IO' actions, so the caller is
-- responsible for running them on the BTS — e.g. from within
-- 'Miso.Effect.runOnBG' or a background-thread subscription.
--
----------------------------------------------------------------------------
module Miso.Native.Module
  ( -- * Combinators
    callNativeModule
  , callNativeModuleWith
    -- * Low-level handles
  , nativeModules
  , getNativeModule
  ) where
----------------------------------------------------------------------------
import           Control.Monad (void)
----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.FFI (consoleError)
import           Miso.JSON
  ( Value, FromJSON, fromJSON, Result(..), toJSVal_Value, fromJSVal_Value )
import           Miso.String (MisoString, ms)
----------------------------------------------------------------------------
-- | The global Lynx @NativeModules@ object.
--
-- __N.B.__ only available on the background thread (BTS).
nativeModules :: IO JSVal
nativeModules = jsg "NativeModules"
----------------------------------------------------------------------------
-- | Look up a native module by name: @NativeModules.\<name\>@.
--
-- __N.B.__ only available on the background thread (BTS).
getNativeModule :: MisoString -> IO JSVal
getNativeModule name = nativeModules ! name
----------------------------------------------------------------------------
-- | Invoke a synchronous (void-returning) native-module method.
--
-- > callNativeModule "NativeLocalStorageModule" "setStorageItem"
-- >   [ String "myKey", String "myValue" ]
--
-- __N.B.__ must be run on the background thread (BTS).
callNativeModule
  :: MisoString
  -- ^ Module name
  -> MisoString
  -- ^ Method name
  -> [Value]
  -- ^ Arguments
  -> IO ()
callNativeModule name method args = do
  m      <- getNativeModule name
  jsArgs <- traverse toJSVal_Value args
  void $ m # method $ jsArgs
----------------------------------------------------------------------------
-- | Invoke a callback-based native-module method. The native result is decoded
-- via 'FromJSON' and handed to the supplied continuation.
--
-- > callNativeModuleWith "NativeLocalStorageModule" "getStorageItem"
-- >   [ String "myKey" ] (\\value -> …)
--
-- The callback is appended to @args@ automatically.
--
-- __N.B.__ must be run on the background thread (BTS).
callNativeModuleWith
  :: FromJSON result
  => MisoString
  -- ^ Module name
  -> MisoString
  -- ^ Method name
  -> [Value]
  -- ^ Arguments (callback appended automatically)
  -> (result -> IO ())
  -- ^ Continuation invoked when the native callback fires
  -> IO ()
callNativeModuleWith name method args k = do
  m      <- getNativeModule name
  jsArgs <- traverse toJSVal_Value args
  cb     <- toJSVal =<< asyncCallback1 (\jval -> do
              result <- fromJSVal_Value jval
              case fromJSON <$> result of
                Just (Success x) -> k x
                Just (Error e)   -> consoleError ("callNativeModuleWith: " <> ms e)
                Nothing          -> consoleError "callNativeModuleWith: unreadable native result")
  void $ m # method $ (jsArgs ++ [cb])
----------------------------------------------------------------------------
