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
-- Functions for specifying component lifecycle events and event handlers in 'Miso.Types.View'.
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
-- | Attaches an event listener that fires during the browser bubble phase.
-- Convenience wrapper for @'onWithOptions' 'BUBBLE' 'defaultOptions'@.
--
-- @
-- let clickHandler = on "click" emptyDecoder $ \() _ -> Action
-- in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
-- @
--
on
  :: MisoString
  -- ^ DOM event name (e.g. @"click"@, @"input"@, @"keydown"@)
  -> Decoder r
  -- ^ Decoder for extracting data from the event object
  -> (r -> DOMRef -> action)
  -- ^ Callback receiving the decoded value and the target DOM element reference
  -> Attribute action
on = onWithOptions BUBBLE defaultOptions
-----------------------------------------------------------------------------
-- | Like 'on' but fires during the browser capture phase instead of the bubble phase.
-- Convenience wrapper for @'onWithOptions' 'CAPTURE' 'defaultOptions'@.
--
-- @
-- let clickHandler = onCapture "click" emptyDecoder $ \() _ -> Action
-- in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
-- @
--
onCapture
  :: MisoString
  -- ^ DOM event name (e.g. @"click"@, @"input"@)
  -> Decoder r
  -- ^ Decoder for extracting data from the event object
  -> (r -> DOMRef -> action)
  -- ^ Callback receiving the decoded value and the target DOM element reference
  -> Attribute action
onCapture = onWithOptions CAPTURE defaultOptions
-----------------------------------------------------------------------------
-- | Full-control event attribute constructor. Sets an event handler on the
-- associated DOM node that decodes the event object, converts the result to
-- an action via @toAction@, and dispatches it to the @update@ function.
--
-- Use 'on' / 'onCapture' for the common cases.
--
-- @
-- -- Prevent default and stop propagation on a form submit:
-- let submitHandler =
--       onWithOptions BUBBLE (defaultOptions { _preventDefault = True })
--         "submit" emptyDecoder $ \() _ -> FormSubmitted
-- in form_ [ submitHandler ] [ ... ]
-- @
--
onWithOptions
  :: Phase
  -- ^ Whether to listen during the @'CAPTURE'@ or @'BUBBLE'@ phase
  -> Options
  -- ^ Event options controlling @preventDefault@ and @stopPropagation@
  -> MisoString
  -- ^ DOM event name (e.g. @"click"@, @"submit"@)
  -> Decoder r
  -- ^ Decoder for extracting data from the event object
  -> (r -> DOMRef -> action)
  -- ^ Callback receiving the decoded value and the target DOM element reference
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
-- | Lifecycle hook fired after the DOM element is inserted into the document.
--
-- @since 1.9.0.0
--
onCreated
  :: action
  -- ^ Action to dispatch once the DOM node has been created
  -> Attribute action
onCreated action =
  On $ \sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onCreated" callback object
-----------------------------------------------------------------------------
-- | Like 'onCreated' but passes the t'DOMRef' of the created element to the callback.
--
-- @since 1.9.0.0
--
onCreatedWith
  :: (DOMRef -> action)
  -- ^ Callback receiving the t'DOMRef' of the newly created DOM node
  -> Attribute action
onCreatedWith action =
  On $ \sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback1 (sink . action)
    FFI.set "onCreated" callback object
-----------------------------------------------------------------------------
-- | Lifecycle hook fired after the DOM element is removed from the document.
--
-- @since 1.9.0.0
--
onDestroyed
  :: action
  -- ^ Action to dispatch once the DOM node has been removed
  -> Attribute action
onDestroyed action =
  On $ \sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onDestroyed" callback object
-----------------------------------------------------------------------------
-- | Lifecycle hook fired just before the DOM element is removed from the document.
--
-- @since 1.9.0.0
--
onBeforeDestroyed
  :: action
  -- ^ Action to dispatch just before the DOM node is removed
  -> Attribute action
onBeforeDestroyed action =
  On $ \sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeDestroyed" callback object
-----------------------------------------------------------------------------
-- | Like 'onBeforeDestroyed' but passes the t'DOMRef' of the element to the callback.
--
-- @since 1.9.0.0
--
onBeforeDestroyedWith
  :: (DOMRef -> action)
  -- ^ Callback receiving the t'DOMRef' of the DOM node about to be removed
  -> Attribute action
onBeforeDestroyedWith action =
  On $ \sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback1 (sink . action)
    FFI.set "onBeforeDestroyed" callback object
-----------------------------------------------------------------------------
-- | Lifecycle hook fired just before the DOM element is inserted into the document.
--
-- @since 1.9.0.0
--
onBeforeCreated
  :: action
  -- ^ Action to dispatch just before the DOM node is created
  -> Attribute action
onBeforeCreated action =
  On $ \sink (VTree object) _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeCreated" callback object
-----------------------------------------------------------------------------
