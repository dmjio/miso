-----------------------------------------------------------------------------
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
-- See "Miso.Event.Decoder" for building custom 'Decoder' values and
-- "Miso.Event.Types" for structured payload types ('KeyboardEvent',
-- 'PointerEvent', etc.).
--
----------------------------------------------------------------------------
module Miso.Event
   ( -- *** Smart constructors
     on
   , onCapture
   , onWithOptions
   , Phase (..)
   -- *** Lifecycle events
   , onCreated
   , onCreatedWith
   , onBeforeCreated
   , onDestroyed
   , onBeforeDestroyed
   , onBeforeDestroyedWith
    -- *** Exports
   , module Miso.Event.Decoder
   , module Miso.Event.Types
   ) where
-----------------------------------------------------------------------------
import           Control.Monad (when)
import qualified Data.Map.Strict as M
import           Miso.JSON (parseEither)
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Event.Decoder
import           Miso.Event.Types
import qualified Miso.FFI.Internal as FFI
import           Miso.Types (Attribute (On), LogLevel(..), DOMRef, VTree(..))
import           Miso.String (MisoString, ms)
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
   -> Decoder r
   -- ^ How to extract a Haskell value from the browser event object
   -> (r -> DOMRef -> action)
   -- ^ Converts the decoded payload and the element's DOM reference to an @action@
   -> Attribute action
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
   -> Decoder r
   -- ^ How to extract a Haskell value from the browser event object
   -> (r -> DOMRef -> action)
   -- ^ Converts the decoded payload and the element's DOM reference to an @action@
   -> Attribute action
onCapture = onWithOptions CAPTURE defaultOptions
-----------------------------------------------------------------------------
-- | Attach an event handler with explicit phase and propagation options.
--
-- * @phase@    — 'BUBBLE' (default) or 'CAPTURE': which DOM propagation phase
--   the listener is registered on.
-- * @options@  — 'defaultOptions' or a custom 'Options' value: controls
--   @preventDefault@ and @stopPropagation@ behaviour.
-- * @eventName@ — the DOM event name, e.g. @\"click\"@, @\"keydown\"@.
-- * @decoder@  — a 'Decoder' that extracts relevant fields from the JS event object.
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
  -> Decoder r
  -- ^ How to extract a Haskell value from the browser event object
  -> (r -> DOMRef -> action)
  -- ^ Converts the decoded payload and the element's DOM reference to an @action@
  -> Attribute action
onWithOptions phase options eventName Decoder{..} toAction =
  On $ \sink (VTree n) logLevel events -> do
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
          Right event -> sink (toAction event domRef)
    FFI.set "runEvent" cb eventHandlerObject
    FFI.set "options" jsOptions eventHandlerObject
    FFI.set eventName eo (Object eventObj)
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
  -> Attribute action
onCreated action =
  On $ \sink (VTree object) _ _ -> do
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
  -> Attribute action
onCreatedWith action =
  On $ \sink (VTree object) _ _ -> do
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
  -> Attribute action
onDestroyed action =
  On $ \sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
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
  -> Attribute action
onBeforeDestroyed action =
  On $ \sink (VTree object) _ _ -> do
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
  -> Attribute action
onBeforeDestroyedWith action =
  On $ \sink (VTree object) _ _ -> do
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
  -> Attribute action
onBeforeCreated action =
  On $ \sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeCreated" callback object
-----------------------------------------------------------------------------
