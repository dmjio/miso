-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Window
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Subscription.Window" provides subscriptions that listen to
-- <https://developer.mozilla.org/en-US/docs/Web/API/Window#events window-level events>.
-- It also exposes 'windowSub' and 'windowSubWithOptions' as the generic
-- primitives on which 'Miso.Subscription.Mouse.mouseSub' and other
-- per-event wrappers are built.
--
-- = Quick start
--
-- @
-- import "Miso"
-- import "Miso.Subscription.Window"
--
-- data Action
--   = MouseMoved 'Miso.Canvas.Coord'         -- (clientX, clientY)
--   | PointerMoved 'Miso.Event.Types.PointerEvent' -- full pointer data
--
-- subs :: ['Miso.Effect.Sub' Action]
-- subs =
--   [ 'windowCoordsSub'      MouseMoved    -- simple (x, y) pair
--   , 'windowPointerMoveSub' PointerMoved  -- full PointerEvent
--   ]
-- @
--
-- = Subscription variants
--
-- * 'windowCoordsSub' — fires @(clientX, clientY)@ as a 'Miso.Canvas.Coord'
--   on every @pointermove@. Simplest option when only screen position is needed.
--
-- * 'windowPointerMoveSub' — fires the full 'Miso.Event.Types.PointerEvent'
--   (pressure, tilt, pointer type, …) on every @pointermove@.
--
-- * 'windowSub' — listen to __any named window event__ by providing an event
--   name and a 'Miso.Event.Decoder.Decoder':
--
-- @
-- resizeSub :: ('Miso.Canvas.Coord' -> action) -> 'Miso.Effect.Sub' action
-- resizeSub f = 'windowSub' \"resize\" 'Miso.Event.Decoder.emptyDecoder' (const (f (0,0)))
-- @
--
-- * 'windowSubWithOptions' — same as 'windowSub' but accepts
--   'Miso.Event.Types.Options' to call @preventDefault@ or @stopPropagation@
--   on the raw event before forwarding it.
--
-- = See also
--
-- * "Miso.Subscription.Mouse" — 'Miso.Subscription.Mouse.mouseSub' (thin alias over 'windowPointerMoveSub')
-- * "Miso.Event.Decoder" — 'Miso.Event.Decoder.Decoder', 'Miso.Event.Decoder.pointerDecoder'
-- * "Miso.Event.Types" — 'Miso.Event.Types.PointerEvent', 'Miso.Event.Types.Options'
-- * "Miso.Subscription.Util" — 'Miso.Subscription.Util.createSub' used internally
----------------------------------------------------------------------------
module Miso.Subscription.Window
  ( -- *** Subscription
    windowSub
  , windowCoordsSub
  , windowPointerMoveSub
  , windowSubWithOptions
  -- *** Types
  , Coord
  ) where
-----------------------------------------------------------------------------
import           Control.Monad
-----------------------------------------------------------------------------
import           Miso.DSL
import           Miso.Event
import           Miso.Effect
import qualified Miso.FFI.Internal as FFI
import           Miso.JSON hiding (Options, defaultOptions)
import           Miso.String
import           Miso.Subscription.Util
import           Miso.Canvas (Coord)
-----------------------------------------------------------------------------
-- | Captures window coordinates changes as they occur and writes them to
-- an event sink.
windowCoordsSub
  :: (Coord -> action)
  -- ^ Callback fired with @(clientX, clientY)@ on every @pointermove@ event
  -> Sub action
windowCoordsSub f = windowPointerMoveSub (f . client)
-----------------------------------------------------------------------------
-- | @windowSub eventName decoder toAction@ provides a subscription
-- to listen to [window level events](https://developer.mozilla.org/en-US/docs/Web/API/Window#events).
windowSub
  :: MisoString
  -- ^ DOM event name to listen for on @window@ (e.g. @\"resize\"@, @\"pointermove\"@)
  -> Decoder r
  -- ^ 'Decoder' for extracting a value from the raw event object
  -> (r -> action)
  -- ^ Callback fired with the decoded value on each event
  -> Sub action
windowSub = windowSubWithOptions defaultOptions
-----------------------------------------------------------------------------
-- | @windowSubWithOptions options eventName decoder toAction@ provides a
-- subscription to listen to [window level events](https://developer.mozilla.org/en-US/docs/Web/API/Window#events).
windowSubWithOptions
  :: Options
  -- ^ Propagation options (@preventDefault@, @stopPropagation@)
  -> MisoString
  -- ^ DOM event name to listen for on @window@
  -> Decoder result
  -- ^ 'Decoder' for extracting a value from the raw event object
  -> (result -> action)
  -- ^ Callback fired with the decoded value on each event
  -> Sub action
windowSubWithOptions Options{..} eventName Decoder {..} toAction sink =
  createSub acquire release sink
    where
      release =
        FFI.windowRemoveEventListener eventName
      acquire =
        FFI.windowAddEventListener eventName $ \e -> do
          decodeAtVal <- toJSVal decodeAt
          v <- fromJSValUnchecked =<< FFI.eventJSON decodeAtVal e
          case parseEither decoder v of
            Left s ->
              FFI.consoleError ("windowSubWithOptions: Parse error on " <> eventName <> ": " <> ms s)
            Right r -> do
              when _stopPropagation (FFI.eventStopPropagation e)
              when _preventDefault (FFI.eventPreventDefault e)
              sink (toAction r)
-----------------------------------------------------------------------------
-- | @window.addEventListener ("pointermove", (event) => handle(event))@
-- A 'Sub' to handle t'PointerEvent's on window.
windowPointerMoveSub
  :: (PointerEvent -> action)
  -- ^ Callback fired with the full 'PointerEvent' on every @pointermove@
  -> Sub action
windowPointerMoveSub = windowSub "pointermove" pointerDecoder
-----------------------------------------------------------------------------
