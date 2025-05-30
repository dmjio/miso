-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Miso.Html.Event
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
  , onBeforeCreated
  , onDestroyed
  , onBeforeDestroyed
   -- *** Mouse
  , onClick
  , onClickWith
  , onDoubleClick
  , onMouseDown
  , onMouseUp
  , onMouseEnter
  , onMouseLeave
  , onMouseOver
  , onMouseOut
  -- *** Keyboard
  , onKeyDown
  , onKeyDownWithInfo
  , onKeyPress
  , onKeyUp
  -- *** Form
  , onInput
  , onChange
  , onChecked
  , onSubmit
  -- *** Focus
  , onBlur
  , onFocus
  -- *** Drag
  , onDrag
  , onDragLeave
  , onDragEnter
  , onDragEnd
  , onDragStart
  , onDragOver
  -- *** Drop
  , onDrop
  -- *** Select
  , onSelect
  -- *** Pointer
  , onPointerDown
  , onPointerUp
  , onPointerEnter
  , onPointerLeave
  , onPointerOver
  , onPointerOut
  , onPointerCancel
  , onPointerMove
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (when, forM_)
import qualified Data.Map.Strict as M
import           Data.Aeson.Types (parseEither)
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso.Event
import qualified Miso.FFI.Internal as FFI
import           Miso.Types ( Attribute (Event), LogLevel(..) )
import           Miso.String (MisoString, unpack)
-----------------------------------------------------------------------------
-- | Convenience wrapper for @onWithOptions defaultOptions@.
--
-- > let clickHandler = on "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: MisoString
   -> Decoder r
   -> (r -> JSVal -> action)
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
  -> (r -> JSVal -> action)
  -> Attribute action
onWithOptions options eventName Decoder{..} toAction =
  Event $ \sink n logLevel events ->
    case M.lookup eventName events of
      Nothing ->
        when (logLevel `elem` [ DebugAll, DebugEvents ]) $
          FFI.consoleWarn $ mconcat
            [ "[DEBUG_EVENTS] \""
            , eventName
            , "\" is not being listened on. To use this event, "
            , "add to the 'events' @Map@ in @Component@"
            ]
      Just _ -> do
        eventObj <- getProp "events" n
        eventHandlerObject@(Object eo) <- create
        jsOptions <- toJSVal options
        decodeAtVal <- toJSVal decodeAt
        cb <- FFI.asyncCallback2 $ \e domRef -> do
            Just v <- fromJSVal =<< FFI.eventJSON decodeAtVal e
            case parseEither decoder v of
              Left s -> error $ "[ERROR] Parse error on " <> unpack eventName <> ": " <> s
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
-- This way the parent can maintain a 'Map' of the child 'Component' IDs. When
-- the parent 'Component' wants to send a child 'Component' a message it can use
-- @notify'@.
--
-- Use this or @onMounted@, but not both in the same @[Attribute action]@ list.
--
onMountedWith :: (MisoString -> action) -> Attribute action
onMountedWith action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback1 $ \jval -> do
      maybeName <- fromJSVal jval
      forM_ maybeName (sink . action)
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
-- Returning /component-id/ is useful when dynamically created @Component@ need
-- to notify their parents about their own destruction.
--
-- This way the parent can maintain a @Map@ of the child @Component@ IDs. When
-- the parent @Component@ wants to send a child @Component@ a message it can use
-- @notify'@.
--
-- Use this or @onUnmounted@, but not both in the same @[Attribute action]@ list.
--
onUnmountedWith :: (MisoString -> action) -> Attribute action
onUnmountedWith action =
  Event $ \sink object _ _ -> do
    callback <- FFI.syncCallback1 $ \jval -> do
      maybeName <- fromJSVal jval
      forM_ maybeName (sink . action)
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
-- | blur event defined with custom options
--
-- <https://developer.mozilla.org/en-US/docs/Web/Events/blur>
--
onBlur :: action -> Attribute action
onBlur action = on "blur" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChecked :: (Checked -> action) -> Attribute action
onChecked f = on "change" checkedDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
onClick :: action -> Attribute action
onClick action = on "click" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/click
-- Like 'onClick', but passes the DOM reference along (akin to 'getElementBydId').
onClickWith :: (JSVal -> action) -> Attribute action
onClickWith action = on "click" emptyDecoder $ \() domRef -> action domRef
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/focus
onFocus :: action -> Attribute action
onFocus action = on "focus" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dblclick
onDoubleClick :: action -> Attribute action
onDoubleClick action = on "dblclick" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/input
onInput :: (MisoString -> action) -> Attribute action
onInput f = on "input" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/change
onChange :: (MisoString -> action) -> Attribute action
onChange f = on "change" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/select
onSelect :: (MisoString -> action) -> Attribute action
onSelect f = on "select" valueDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDownWithInfo :: (KeyInfo -> action) -> Attribute action
onKeyDownWithInfo f = on "keydown" keyInfoDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keydown
onKeyDown :: (KeyCode -> action) -> Attribute action
onKeyDown f = on "keydown" keycodeDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keypress
onKeyPress :: (KeyCode -> action) -> Attribute action
onKeyPress f = on "keypress" keycodeDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/keyup
onKeyUp :: (KeyCode -> action) -> Attribute action
onKeyUp f = on "keyup" keycodeDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseup
onMouseUp :: action -> Attribute action
onMouseUp action = on "mouseup" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mousedown
onMouseDown :: action -> Attribute action
onMouseDown action = on "mousedown" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter
onMouseEnter :: action -> Attribute action
onMouseEnter action = on "mouseenter" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave
onMouseLeave :: action -> Attribute action
onMouseLeave action = on "mouseleave" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseover
onMouseOver :: action -> Attribute action
onMouseOver action = on "mouseover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/mouseout
onMouseOut :: action -> Attribute action
onMouseOut action = on "mouseout" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragstart
onDragStart :: action -> Attribute action
onDragStart action = on "dragstart" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragover
onDragOver :: action -> Attribute action
onDragOver action = on "dragover" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragend
onDragEnd :: action -> Attribute action
onDragEnd action = on "dragend" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragenter
onDragEnter :: action -> Attribute action
onDragEnter action = on "dragenter" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/dragleave
onDragLeave :: action -> Attribute action
onDragLeave action = on "dragleave" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drag
onDrag :: action -> Attribute action
onDrag action = on "drag" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/drop
onDrop :: AllowDrop -> action -> Attribute action
onDrop (AllowDrop allowDrop) action =
  onWithOptions defaultOptions { preventDefault = allowDrop }
    "drop" emptyDecoder (\() _ -> action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/submit
onSubmit :: action -> Attribute action
onSubmit action =
  onWithOptions defaultOptions { preventDefault = True }
    "submit" emptyDecoder $ \() _ -> action
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerup
onPointerUp :: (PointerEvent -> action) -> Attribute action
onPointerUp f = on "pointerup" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerdown
onPointerDown :: (PointerEvent -> action) -> Attribute action
onPointerDown f = on "pointerdown" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerenter
onPointerEnter :: (PointerEvent -> action) -> Attribute action
onPointerEnter f = on "pointerenter" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerleave
onPointerLeave :: (PointerEvent -> action) -> Attribute action
onPointerLeave f = on "pointerleave" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerover
onPointerOver :: (PointerEvent -> action) -> Attribute action
onPointerOver f = on "pointerover" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointerout
onPointerOut :: (PointerEvent -> action) -> Attribute action
onPointerOut f = on "pointerout" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointercancel
onPointerCancel :: (PointerEvent -> action) -> Attribute action
onPointerCancel f = on "pointercancel" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/Events/pointermove
onPointerMove :: (PointerEvent -> action) -> Attribute action
onPointerMove f = on "pointermove" pointerDecoder (\action _ -> f action)
-----------------------------------------------------------------------------
