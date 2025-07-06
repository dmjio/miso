-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Event
   ( -- *** Smart constructors
     on
   , onWithOptions
   -- *** Lifecycle events
   , onMounted
   , onMountedWith
   , onBeforeMounted
   , onUnmounted
   , onUnmountedWith
   , onBeforeUnmounted
   , onCreated
   , onCreatedWith
   , onBeforeCreated
   , onDestroyed
   , onBeforeDestroyed
    -- *** Exports
   , module Miso.Event.Decoder
   , module Miso.Event.Types
   ) where
-----------------------------------------------------------------------------
import           Control.Monad (when)
import qualified Data.Map.Strict as M
import           Data.Aeson.Types (parseEither)
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso.Event.Decoder
import           Miso.Event.Types
import qualified Miso.FFI.Internal as FFI
import           Miso.Types ( Attribute (Event), LogLevel(..), DOMRef )
import           Miso.String (MisoString, unpack)
-----------------------------------------------------------------------------
-- | Convenience wrapper for @onWithOptions defaultOptions@.
--
-- > let clickHandler = on "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: MisoString
   -> Decoder r
   -> (r -> DOMRef -> action)
   -> Attribute action
on = onWithOptions defaultOptions
-----------------------------------------------------------------------------
-- | @onWithOptions opts eventName decoder toAction@ is an attribute
-- that will set the event handler of the associated DOM node to a function that
-- decodes its argument using @decoder@, converts it to an action
-- using @toAction@ and then feeds that action back to the @update@ function.
--
-- @opts@ can be used to disable further event propagation.
--
-- > let clickHandler = onWithOptions defaultOptions "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
onWithOptions
  :: Options
  -> MisoString
  -> Decoder r
  -> (r -> DOMRef -> action)
  -> Attribute action
onWithOptions options eventName Decoder{..} toAction =
  Event $ \sink n logLevel events ->
    case M.lookup eventName events of
      Nothing ->
        when (logLevel `elem` [ DebugAll, DebugEvents ]) $
          FFI.consoleError $ mconcat
            [ "Event \""
            , eventName
            , "\" is not being listened on. To use this event, "
            , "add to the 'events' Map in 'Component'"
            ]
      Just _ -> do
        eventObj <- getProp "events" n
        eventHandlerObject@(Object eo) <- create
        jsOptions <- toJSVal options
        decodeAtVal <- toJSVal decodeAt
        cb <- FFI.asyncCallback2 $ \e domRef -> do
            Just v <- fromJSVal =<< FFI.eventJSON decodeAtVal e
            case parseEither decoder v of
              Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
              Right r -> sink (toAction r domRef)
        FFI.set "runEvent" cb eventHandlerObject
        FFI.set "options" jsOptions eventHandlerObject
        FFI.set eventName eo (Object eventObj)
-----------------------------------------------------------------------------
-- | @onMounted action@ is an event that gets called after the actual DOM
-- element is created.
--
onMounted :: action -> Attribute action
onMounted action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onMounted" callback object
-----------------------------------------------------------------------------
-- | @onMountedWith action@ is an event that gets called after the actual DOM
-- element is created. It returns the /component-id/ from the component.
-- Returning /component-id/ is useful when creating 'Component' dynamically.
--
-- Use this or @onMounted@, but not both in the same @[Attribute action]@ list.
--
onMountedWith :: (DOMRef -> action) -> Attribute action
onMountedWith action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback1 (sink . action)
    FFI.set "onMounted" callback object
-----------------------------------------------------------------------------
-- | @onBeforeMounted action@ is an event that gets called before the actual DOM
-- element is created.
--
onBeforeMounted :: action -> Attribute action
onBeforeMounted action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeMounted" callback object
-----------------------------------------------------------------------------
-- | @onCreated action@ is an event that gets called after the actual DOM
-- element is created.
--
onCreated :: action -> Attribute action
onCreated action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onCreated" callback object
-----------------------------------------------------------------------------
-- | Like @onCreated action@ but passes along the `DOMRef`
--
onCreatedWith :: (DOMRef -> action) -> Attribute action
onCreatedWith action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback1 (sink . action)
    FFI.set "onCreated" callback object
-----------------------------------------------------------------------------
-- | @onDestroyed action@ is an event that gets called after the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
onDestroyed :: action -> Attribute action
onDestroyed action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onDestroyed" callback object
-----------------------------------------------------------------------------
-- | @onUnmounted action@ is an event that gets called after the DOM element
-- is removed from the DOM.
--
onUnmounted :: action -> Attribute action
onUnmounted action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onUnmounted" callback object
-----------------------------------------------------------------------------
-- | @onUnmounted action@ is an event that gets called after the DOM element
-- is removed from the DOM. It returns the /component-id/ after the unmount call.
--
-- Use this or @onUnmounted@, but not both in the same @[Attribute action]@ list.
--
onUnmountedWith :: (DOMRef -> action) -> Attribute action
onUnmountedWith action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback1 (sink . action)
    FFI.set "onUnmounted" callback object
-----------------------------------------------------------------------------
-- | @onBeforeUnmounted action@ is an event that gets called before the DOM element
-- is removed from the DOM.
--
onBeforeUnmounted :: action -> Attribute action
onBeforeUnmounted action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeUnmounted" callback object
-----------------------------------------------------------------------------
-- | @onBeforeDestroyed action@ is an event that gets called before the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
onBeforeDestroyed :: action -> Attribute action
onBeforeDestroyed action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeDestroyed" callback object
-----------------------------------------------------------------------------
-- | @onBeforeCreated action@ is an event that gets called before the DOM element
-- is created on the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
onBeforeCreated :: action -> Attribute action
onBeforeCreated action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback (sink action)
    FFI.set "onBeforeCreated" callback object
-----------------------------------------------------------------------------
