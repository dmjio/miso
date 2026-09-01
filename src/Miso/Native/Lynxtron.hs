-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Lynxtron
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Bindings to the <https://github.com/lynx-family/lynxtron Lynxtron> desktop
-- host. Lynxtron is Electron with the Chromium half replaced by Lynx: a Node.js
-- __main process__ (@app@, @LynxWindow@, @Menu@, @Tray@, @dialog@, …) hosts
-- ordinary @.lynx.bundle@s as windows. The bundle miso already produces for
-- iOS \/ Android (see @mkLynxBundle@) loads unchanged; this module covers the
-- one thing that is Lynxtron-specific — the __card ↔ Node bridge__.
--
-- Four channels exist, all on the background thread (BTS):
--
-- +--------------------+-----------------------------------+----------------------------------------------------+
-- | Direction          | Haskell (card, BTS)               | Node (main process)                                |
-- +====================+===================================+====================================================+
-- | card → node, reply | 'invokeNode'                      | @lynxBridge.handle(name, (ev, args) => result)@    |
-- +--------------------+-----------------------------------+----------------------------------------------------+
-- | card → node, fire  | 'sendNode'                        | @win.on('-lynx-message', (name, args) => …)@       |
-- +--------------------+-----------------------------------+----------------------------------------------------+
-- | node → card, event | 'nodeEventSub'                    | @win.sendGlobalEvent(name, payload)@               |
-- +--------------------+-----------------------------------+----------------------------------------------------+
-- | node → card, API   | 'callExposed'                     | preload: @contextBridge.exposeInLynxBTS({ f })@    |
-- +--------------------+-----------------------------------+----------------------------------------------------+
--
-- The bridge lives on the BTS @NativeModules@ object, so — like everything in
-- "Miso.Native.Module" — these are plain 'IO' actions that must run on the
-- BTS: from the @update@ of a BTS handler (@event . static@), an 'Miso.Effect.io_',
-- or a background-thread subscription. 'nodeEventSub' guards itself with
-- 'onBTS'; the rest log to the console and no-op if the bridge is missing
-- (e.g. when the same bundle runs on a mobile host or the web).
--
-- @since 1.14.0.0
----------------------------------------------------------------------------
module Miso.Native.Lynxtron
  ( -- * Card → Node
    invokeNode
  , sendNode
    -- * Node → Card
  , nodeEventSub
  , callExposed
  , callExposedWith
    -- * Introspection
  , isLynxtron
  ) where
----------------------------------------------------------------------------
import           Control.Exception (try, SomeException, displayException)
import           Control.Monad (void, when)
----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Effect (Sub)
import           Miso.FFI (consoleError, onBTS)
import           Miso.JSON
  ( Value, ToJSON(..), FromJSON, fromJSON, Result(..), toJSVal_Value, fromJSVal_Value )
import           Miso.Native.Module (callNativeModule, callNativeModuleWith, getNativeModule)
import           Miso.String (MisoString, ms, fromMisoString)
----------------------------------------------------------------------------
-- | 'True' when running inside a Lynxtron window — i.e. @NativeModules.bridge@
-- is present. Lets one bundle branch between desktop and mobile behaviour.
--
-- __N.B.__ must be run on the background thread (BTS); always 'False' on the MTS.
isLynxtron :: IO Bool
isLynxtron = do
  bts <- onBTS
  if not bts then pure False else do
    r <- try (getNativeModule "bridge")
    case r of
      Left (_ :: SomeException) -> pure False
      Right b -> not <$> isUndefined b
----------------------------------------------------------------------------
-- | Request \/ reply round-trip to the main process:
-- @NativeModules.bridge.call(name, params, callback)@. Node answers with
-- @lynxBridge.handle(name, (event, args) => result)@ (or
-- @event.sendReply(result)@) and the continuation receives the decoded reply.
--
-- > invokeNode "read-file" (object ["path" .= "/etc/hosts"]) $ \case
-- >   Right (contents :: MisoString) -> sink (GotFile contents)
-- >   Left err                       -> sink (BridgeError err)
--
-- __N.B.__ must be run on the background thread (BTS).
invokeNode
  :: (ToJSON params, FromJSON result)
  => MisoString
  -- ^ Channel name (matches @lynxBridge.handle(name, …)@)
  -> params
  -- ^ Payload, JSON-encoded
  -> (Either MisoString result -> IO ())
  -- ^ Continuation, fired exactly once
  -> IO ()
invokeNode name params k =
  callNativeModuleWith "bridge" "call" [toJSON' name, toJSON params] k
----------------------------------------------------------------------------
-- | Fire-and-forget message to the main process:
-- @NativeModules.bridge.send(name, params)@. Node observes it via
-- @win.on('-lynx-message', (name, params) => …)@.
--
-- __N.B.__ must be run on the background thread (BTS).
sendNode :: ToJSON params => MisoString -> params -> IO ()
sendNode name params =
  callNativeModule "bridge" "send" [toJSON' name, toJSON params]
----------------------------------------------------------------------------
-- | Subscribe to global events pushed from the main process with
-- @win.sendGlobalEvent(name, payload)@. Wraps
-- @lynx.getJSModule('GlobalEventEmitter').addListener(name, …)@.
--
-- > subs = [ nodeEventSub "tick" (either BridgeError Tick) ]
--
-- Subs start on both threads (see "Miso.Native"); this one is BTS-only and the
-- MTS copy returns immediately.
nodeEventSub
  :: FromJSON payload
  => MisoString
  -- ^ Event name
  -> (Either MisoString payload -> action)
  -- ^ Decode result → action
  -> Sub action
nodeEventSub name f sink = do
  bts <- onBTS
  when bts $ do
    emitter <- jsg "lynx" # "getJSModule" $ ["GlobalEventEmitter" :: MisoString]
    undef   <- isUndefined emitter
    if undef
      then consoleError "nodeEventSub: GlobalEventEmitter is undefined"
      else do
        cb <- asyncCallback1 $ \jval -> do
          mv <- fromJSVal_Value jval
          case fromJSON <$> mv of
            Just (Success x) -> sink (f (Right x))
            Just (Error e)   -> sink (f (Left (ms e)))
            Nothing          -> sink (f (Left "nodeEventSub: unreadable payload"))
        lynx  <- jsg "lynx"
        nameV <- toJSVal name
        void $ emitter # "addListener" $ [nameV, cb, lynx]
----------------------------------------------------------------------------
-- | Call a function the main process exposed through a preload script with
-- @contextBridge.exposeInLynxBTS({ f: … })@ — i.e.
-- @NativeModules.nodejs.exposed.f(args…)@. The exposed function may be sync or
-- @async@; a returned Promise is awaited. Nested objects are addressed with a
-- dotted path (@"fileApi.exists"@).
--
-- > r <- callExposed "hostname" []
-- > r <- callExposed "fileApi.exists" [String "/tmp/x"]
--
-- __N.B.__ must be run on the background thread (BTS).
callExposed :: FromJSON result => MisoString -> [Value] -> IO (Either MisoString result)
callExposed path args = do
  r <- try $ do
    exposed <- getNativeModule "nodejs" ! ("exposed" :: MisoString)
    undef   <- isUndefined exposed
    when undef $ consoleError "callExposed: NativeModules.nodejs.exposed is undefined"
    (owner, fn) <- resolve exposed (splitDots path)
    jsArgs  <- traverse toJSVal_Value args
    ret     <- owner # fn $ jsArgs
    settled <- isPromise ret >>= \p -> if p then await ret else pure ret
    mv <- fromJSVal_Value settled
    pure $ case fromJSON <$> mv of
      Just (Success x) -> Right x
      Just (Error e)   -> Left (ms e)
      Nothing          -> Left "callExposed: unreadable result"
  case r of
    Left (e :: SomeException) -> do
      let msg = ms (displayException e)
      consoleError ("callExposed: " <> msg)
      pure (Left msg)
    Right v -> pure v
  where
    resolve obj [fn]     = pure (obj, fn)
    resolve obj (p : ps) = (obj ! p) >>= \o -> resolve o ps
    resolve obj []       = pure (obj, "")
----------------------------------------------------------------------------
-- | Continuation-passing 'callExposed', convenient inside 'Miso.Effect.io_'.
callExposedWith
  :: FromJSON result
  => MisoString -> [Value] -> (Either MisoString result -> IO ()) -> IO ()
callExposedWith path args k = callExposed path args >>= k
----------------------------------------------------------------------------
isPromise :: JSVal -> IO Bool
isPromise v = do
  undef <- isUndefined v
  nul   <- isNull v
  if undef || nul then pure False else do
    th <- v ! ("then" :: MisoString)
    u  <- isUndefined th
    pure (not u)

toJSON' :: MisoString -> Value
toJSON' = toJSON

splitDots :: MisoString -> [MisoString]
splitDots s = case break (== '.') (fromMisoString s) of
  (a, [])      -> [ms a]
  (a, _ : rest) -> ms a : splitDots (ms rest)
----------------------------------------------------------------------------
