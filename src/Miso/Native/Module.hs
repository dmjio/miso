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
import           Control.Monad (void, when)
import           Control.Concurrent.MVar (newMVar, modifyMVar)
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
  undef  <- isUndefined m
  -- A fire-and-forget call is otherwise silent: if the module isn't present
  -- (e.g. run on the MTS, where @NativeModules@ doesn't exist), the call throws
  -- and the failure vanishes. Surface it. Visible on device via
  -- 'Miso.Native.FFI.enableDebugging'.
  if undef
    then consoleError ("callNativeModule: NativeModules." <> name <> " is undefined")
    else do
      jsArgs <- traverse toJSVal_Value args
      void $ m # method $ jsArgs
----------------------------------------------------------------------------
-- | Invoke a callback-based native-module method. The native result is decoded
-- via 'FromJSON' and handed to the supplied continuation, which fires exactly
-- once — with @'Left' error@ if the native call errored or the result failed
-- to decode. Callers that block awaiting the continuation (e.g. via an
-- 'Control.Concurrent.MVar.MVar') can therefore rely on it always firing,
-- instead of hanging forever on the error path.
--
-- > callNativeModuleWith "NativeLocalStorageModule" "getStorageItem"
-- >   [ String "myKey" ] (either (const Nothing) Just)
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
  -> (Either MisoString result -> IO ())
  -- ^ Continuation invoked exactly once when the native callback fires
  -> IO ()
callNativeModuleWith name method args k = do
  m      <- getNativeModule name
  mUndef <- isUndefined m
  when mUndef $
    consoleError ("callNativeModuleWith: NativeModules." <> name <> " is undefined")
  jsArgs <- traverse toJSVal_Value args
  -- Fire the continuation exactly once, from whichever path responds first: some
  -- Lynx native modules are callback-based (@method(args…, cb)@), others return
  -- the value synchronously (@method(args…) -> value@). We pass a callback AND
  -- inspect the synchronous return, guarding with a one-shot 'MVar' so a module
  -- that does both (or neither) still yields a single 'k'.
  fired  <- newMVar False
  let deliver jval = do
        already <- modifyMVar fired (\f -> pure (True, f))
        if already then pure () else do
          rawNull  <- isNull jval
          rawUndef <- isUndefined jval
          consoleError ("callNativeModuleWith: " <> name <> "." <> method
            <> " delivered null=" <> ms (show rawNull) <> " undef=" <> ms (show rawUndef))
          result <- fromJSVal_Value jval
          case fromJSON <$> result of
            Just (Success x) -> k (Right x)
            Just (Error e)   -> do
              consoleError ("callNativeModuleWith: " <> ms e)
              k (Left (ms e))
            Nothing          -> do
              consoleError "callNativeModuleWith: unreadable native result"
              k (Left "callNativeModuleWith: unreadable native result")
  cb     <- toJSVal =<< asyncCallback1 deliver
  ret    <- m # method $ (jsArgs ++ [cb])
  -- If the module returned a usable value synchronously, deliver it now; if it
  -- returned @undefined@ (the async shape), leave delivery to the callback.
  isUndef <- isUndefined ret
  isNul   <- isNull ret
  if isUndef || isNul then pure () else deliver ret
----------------------------------------------------------------------------
