-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- DOM event handlers and component lifecycle hooks for 'Miso.Types.View'.
--
-- There are two axes of event handling:
--
-- * __DOM events__ — 'on', 'onCapture', 'onWithOptions': attach JavaScript
--   event listeners to VDOM nodes. Decoded event payloads are dispatched as
--   @action@ values through the MVU loop.
--
-- * __Lifecycle hooks__ — 'onCreated', 'onDestroyed', etc.: fire Haskell
--   callbacks at specific points in a DOM element's mount\/unmount lifecycle.
--
-- See "Miso.Event.Decoder" for building custom t'Decoder' values and
-- "Miso.Event.Types" for structured payload types (@KeyboardEvent@,
-- t'PointerEvent', etc.).
--
----------------------------------------------------------------------------
module Miso.Event
   ( -- *** Smart constructors
     on
   , onMain
   , onCapture
   , onWithOptions
   , onMainWithOptions
   , Phase (..)
   -- *** Lifecycle events
   , onCreated
   , onCreatedWith
   , onBeforeCreated
   , onDestroyed
   , onDestroyedWith
   , onBeforeDestroyed
   , onBeforeDestroyedWith
    -- *** Exports
   , module Miso.Event.Decoder
   , module Miso.Event.Types
   ) where
-----------------------------------------------------------------------------
import           Control.Monad (when)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import           Data.IORef
import           Miso.JSON (parseEither)
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Event.Decoder
import           Miso.Event.Types
import qualified Miso.FFI.Internal as FFI
import           Miso.Types (LogLevel(..), DOMRef, VTree(..), EventHandler(..), Attribute(..))
import           Miso.Runtime
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
-- | Like 'on' but meant to used with the "Miso.Native" namespace.
--
-- @
-- view_ [ event $ static ('onMain' "tap" emptyDecoder ) ] [ text_ \"+\" ]
-- @
--
-- @since 1.13.0.0
onMain :: MisoString
   -- ^ DOM event name (e.g. @\"click\"@, @\"input\"@)
   -> Decoder result
   -- ^ How to extract a Haskell value from the browser event object
   -> (result -> model -> DOMRef -> action)
   -- ^ Converts the decoded payload and the element's DOM reference to an @action@
   -> EventHandler model action
onMain = onMainWithOptions BUBBLE defaultOptions
-----------------------------------------------------------------------------
-- | Attach a bubble-phase event handler to a VDOM node.
-- Convenience wrapper for @'onWithOptions' 'BUBBLE' 'defaultOptions'@.
--
-- The decoded event payload is converted to an @action@ by @toAction@ and
-- dispatched into the component's @update@ function.
--
-- @
-- let clickHandler = on \"click\" emptyDecoder $ \\() _ -> MyAction
-- in button_ [ clickHandler, class_ \"add\" ] [ text_ \"+\" ]
-- @
--
on :: MisoString
   -- ^ DOM event name (e.g. @\"click\"@, @\"input\"@)
   -> Decoder result
   -- ^ How to extract a Haskell value from the browser event object
   -> (result -> model -> DOMRef -> action)
   -- ^ Converts the decoded payload and the element's DOM reference to an @action@
   -> Attribute model action
on = onWithOptions BUBBLE defaultOptions
-----------------------------------------------------------------------------
-- | Attach a capture-phase event handler to a VDOM node.
-- Convenience wrapper for @'onWithOptions' 'CAPTURE' 'defaultOptions'@.
--
-- Events in the capture phase propagate from the document root down to the
-- target element, before any bubble-phase handlers run.
--
-- @
-- let captureClick = onCapture \"click\" emptyDecoder $ \\() _ -> MyAction
-- in button_ [ captureClick ] [ text_ \"capture me\" ]
-- @
--
onCapture
   :: MisoString
   -- ^ DOM event name (e.g. @\"click\"@)
   -> Decoder result
   -- ^ How to extract a Haskell value from the browser event object
   -> (result -> model -> DOMRef -> action)
   -- ^ Converts the decoded payload and the element's DOM reference to an @action@
   -> Attribute model action
onCapture = onWithOptions CAPTURE defaultOptions
-----------------------------------------------------------------------------
-- | Mark an event handler to be dispatched on the Lynx __main thread__ (@MTS@)
-- rather than the background thread. This is the analog of Lynx's
-- @main-thread:bind@ prefix, and is decided __per handler__ — so @tap@ can be a
-- main-thread handler on one element and a background handler on another.
--
-- A main-thread handler runs imperatively on the MTS (no VDOM diff, no repaint);
-- pair it with a @*With@ combinator to receive the target 'DOMRef' and mutate it
-- via "Miso.Native.MainThread". No-op on the browser\/WASM runtime.
--
-- @
-- view_ [ event (static (mainThread (onTapWith Grow))) ] children
-- @
--
-- @since 1.13.0.0
onMainWithOptions
  :: Phase
  -- ^ Event propagation phase: 'BUBBLE' (default) or 'CAPTURE'
  -> Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> MisoString
  -- ^ DOM event name (e.g. @\"click\"@, @\"keydown\"@)
  -> Decoder result
  -- ^ How to extract a Haskell value from the browser event object
  -> (result -> model -> DOMRef -> action)
  -- ^ Converts the decoded payload and the element's DOM reference to an @action@
  -> EventHandler model action
onMainWithOptions phase opts name decoder conversion =
  EventHandler
    { eventHandlerInstall = \m snk tree ll events ->
        case onWithOptions phase opts name decoder conversion of
          On cb -> do
            FFI.set "pendingMainThread" True =<< toObject tree
            cb m snk tree ll events
          _ ->
            error "onMainWithOptions: impossible"
    , eventHandlerDecoder = decoder
    , eventHandlerConvert = conversion
    }
-----------------------------------------------------------------------------
-- | Attach an event handler with explicit phase and propagation options.
--
-- * @phase@    — 'BUBBLE' (default) or 'CAPTURE': which DOM propagation phase
--   the listener is registered on.
-- * @options@  — 'defaultOptions' or a custom t'Options' value: controls
--   @preventDefault@ and @stopPropagation@ behaviour.
-- * @eventName@ — the DOM event name, e.g. @\"click\"@, @\"keydown\"@.
-- * @decoder@  — a t'Decoder' that extracts relevant fields from the JS event object.
-- * @toAction@ — maps the decoded payload and the element's 'DOMRef' to an @action@.
--
-- @
-- let clickHandler = onWithOptions BUBBLE defaultOptions \"click\" emptyDecoder $ \\() _ -> Action
-- in button_ [ clickHandler, class_ \"add\" ] [ text_ \"+\" ]
-- @
--
onWithOptions
  :: Phase
  -- ^ Event propagation phase: 'BUBBLE' (default) or 'CAPTURE'
  -> Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> MisoString
  -- ^ DOM event name (e.g. @\"click\"@, @\"keydown\"@)
  -> Decoder result
  -- ^ How to extract a Haskell value from the browser event object
  -> (result -> model -> DOMRef -> action)
  -- ^ Converts the decoded payload and the element's DOM reference to an @action@
  -> Attribute model action
onWithOptions phase options eventName Decoder{..} toAction =
  On $ \_model sink (VTree n) logLevel events -> do
    when (logLevel == DebugAll || logLevel == DebugEvents) $
      case M.lookup eventName events of
        Nothing ->
            FFI.consoleError $ mconcat
              [ "Event \""
              , eventName
              , "\" is not being listened on. To use this event, "
              , "add to the 'events' Map in Component"
              ]
        _ -> pure ()
    eventsVal <-
      getProp "events" n
    eventObj <-
      case phase of
        CAPTURE -> getProp "captures" (Object eventsVal)
        BUBBLE -> getProp "bubbles" (Object eventsVal)
    eventHandlerObject@(Object eo) <- create
    jsOptions <- toJSVal options
    decodeAtVal <- toJSVal decodeAt
    cb <- FFI.asyncCallback2 $ \e domRef -> do
        Just v <- fromJSVal =<< FFI.eventJSON decodeAtVal e
        case parseEither decoder v of
          Left msg -> FFI.consoleError ("[EVENT DECODE ERROR]: " <> ms msg)
          Right event -> do
            vcompId <- fromJSValUnchecked =<< getProp "pendingComponentId" n
            IM.lookup vcompId <$> readIORef components >>= \case
              Nothing ->
                FFI.consoleError ("[COMPONENT]: No component found at ID: " <> ms vcompId)
              Just ComponentState {..} ->
                sink (toAction event _componentModel domRef)
    -- The runtime frees this callback when the vtree that owns it is
    -- replaced; see Note [Freeing event handler callbacks] in "Miso.Runtime".
    registerEventHandler cb
    FFI.set "runEvent" cb eventHandlerObject
    FFI.set "options" jsOptions eventHandlerObject
    -- Only 'mainThread'-marked handlers carry their 'StaticKey' \/ @ComponentId@
    -- (stashed on the node by 'setAttrs') onto the per-event object. That is what
    -- puts them in the node's @eventKeys@, telling the native delegator to
    -- dispatch this handler on the main thread. Unmarked handlers (and the
    -- browser\/WASM runtime) leave these off and delegate to the background.
    pendingMT <- getProp "pendingMainThread" n
    isMainThread <- fromJSVal pendingMT :: IO (Maybe Bool)
    when (isMainThread == Just True) $ do
      pendingKey <- getProp "pendingStaticKey" n
      mKey <- fromJSVal pendingKey :: IO (Maybe MisoString)
      maybe (pure ()) (\k -> FFI.set "staticKey" (k :: MisoString) eventHandlerObject) mKey
      pendingCid <- getProp "pendingComponentId" n
      mCid <- fromJSVal pendingCid :: IO (Maybe Int)
      maybe (pure ()) (\c -> FFI.set "componentId" (c :: Int) eventHandlerObject) mCid
    FFI.set eventName eo (Object eventObj)
    -- The handler object is now reachable from the node; release the scratch
    -- handles. @decodeAtVal@, @cb@ and @n@ are captured by the callback and
    -- must stay alive. See Note [Freeing VTree handles] in "Miso.Runtime".
    mapM_ freeJSVal [eventsVal, eventObj, eo, jsOptions, pendingMT]
-----------------------------------------------------------------------------
-- | Fire an action immediately after the DOM element is inserted into the document.
--
-- Use this to trigger imperative setup (focus, measurements, third-party widget
-- initialisation) that requires the element to be live in the page.
--
-- @since 1.9.0.0
--
onCreated
  :: action
  -- ^ Action to dispatch after the element is inserted into the DOM
  -> Attribute model action
onCreated action =
  On $ \_model sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onCreated" callback object
-----------------------------------------------------------------------------
-- | Like 'onCreated' but also receives the element's 'DOMRef'.
--
-- Useful when you need to store or forward the raw DOM node to a JS library.
--
-- @since 1.9.0.0
--
onCreatedWith
  :: (DOMRef -> action)
  -- ^ Callback receiving the element's 'DOMRef' after it is inserted into the DOM
  -> Attribute model action
onCreatedWith action =
  On $ \_model sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback1 (sink . action)
    FFI.set "onCreated" callback object
-----------------------------------------------------------------------------
-- | Fire an action immediately after the DOM element is removed from the document.
--
-- The element has already been detached from the DOM when this fires.
--
-- @since 1.9.0.0
--
onDestroyed
  :: action
  -- ^ Action to dispatch after the element is removed from the DOM
  -> Attribute model action
onDestroyed action =
  On $ \_model sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onDestroyed" callback object
-----------------------------------------------------------------------------
-- | Like 'onDestroyed' but also receives the element's 'DOMRef'.
--
-- The element has already been detached from the DOM when this fires: the
-- 'DOMRef' has no parent and reports a zero bounding rect. Use it to clean
-- up out-of-band references to the element (unregister it from a JS
-- library, drop it from a lookup table) — for teardown that needs the
-- element still live in the document, use 'onBeforeDestroyedWith'.
--
-- @since 1.13.0.0
--
onDestroyedWith
  :: (DOMRef -> action)
  -- ^ Callback receiving the element's (detached) 'DOMRef' after it is removed from the DOM
  -> Attribute model action
onDestroyedWith action =
  On $ \_model sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback1 (sink . action)
    FFI.set "onDestroyed" callback object
-----------------------------------------------------------------------------
-- | Fire an action just before the DOM element is removed from the document.
--
-- The element is still present in the DOM when this fires, making it suitable
-- for teardown logic (cancel animations, disconnect observers, etc.).
--
-- @since 1.9.0.0
--
onBeforeDestroyed
  :: action
  -- ^ Action to dispatch just before the element is removed from the DOM
  -> Attribute model action
onBeforeDestroyed action =
  On $ \_model sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeDestroyed" callback object
-----------------------------------------------------------------------------
-- | Like 'onBeforeDestroyed' but also receives the element's 'DOMRef'.
--
-- @since 1.9.0.0
--
onBeforeDestroyedWith
  :: (DOMRef -> action)
  -- ^ Callback receiving the element's 'DOMRef' just before it is removed from the DOM
  -> Attribute model action
onBeforeDestroyedWith action =
  On $ \_model sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback1 (sink . action)
    FFI.set "onBeforeDestroyed" callback object
-----------------------------------------------------------------------------
-- | Fire an action just before the DOM element is inserted into the document.
--
-- The element has been constructed but is not yet attached to the live DOM when
-- this fires.
--
-- @since 1.9.0.0
--
onBeforeCreated
  :: action
  -- ^ Action to dispatch just before the element is inserted into the DOM
  -> Attribute model action
onBeforeCreated action =
  On $ \_model sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeCreated" callback object
-----------------------------------------------------------------------------
