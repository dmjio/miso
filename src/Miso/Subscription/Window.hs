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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element/pointermove_event>
--
-- Subscribes to global pointer move events and extracts the @clientX@ / @clientY@
-- coordinates as a t'Coord' pair.
--
-- @
-- app = (component m u v) { subs = [ windowCoordsSub MouseAt ] }
-- data Action = MouseAt Coord
-- @
--
windowCoordsSub
  :: (Coord -> action)
  -- ^ Callback invoked with the @(clientX, clientY)@ coordinate on every pointer move
  -> Sub action
windowCoordsSub f = windowPointerMoveSub (f . client)
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window#events>
--
-- Generic subscription for any window-level DOM event.
-- Uses a 'Decoder' to extract the desired fields from the raw JS event object.
--
-- @
-- -- Subscribe to window resize events, decoding the inner width/height:
-- windowSub "resize" sizeDecoder WindowResized
-- @
--
windowSub
  :: MisoString
  -- ^ DOM event name (e.g. @"resize"@, @"scroll"@, @"pointermove"@)
  -> Decoder r
  -- ^ Decoder for extracting data from the event object
  -> (r -> action)
  -- ^ Callback that wraps the decoded value into an action
  -> Sub action
windowSub = windowSubWithOptions defaultOptions
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Window#events>
--
-- Like 'windowSub' but accepts 'Options' to control @stopPropagation@ and
-- @preventDefault@ behaviour on the underlying event listener.
windowSubWithOptions
  :: Options
  -- ^ Event listener options controlling propagation and default behaviour
  -> MisoString
  -- ^ DOM event name (e.g. @"resize"@, @"scroll"@)
  -> Decoder result
  -- ^ Decoder for extracting data from the event object
  -> (result -> action)
  -- ^ Callback that wraps the decoded value into an action
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
-- | <https://developer.mozilla.org/en-US/docs/Web/API/Element/pointermove_event>
--
-- Subscribes to global @pointermove@ events, delivering the full t'PointerEvent'
-- payload. Prefer 'windowCoordsSub' if you only need coordinates.
--
windowPointerMoveSub
  :: (PointerEvent -> action)
  -- ^ Callback invoked with the full t'PointerEvent' on every pointer move
  -> Sub action
windowPointerMoveSub = windowSub "pointermove" pointerDecoder
-----------------------------------------------------------------------------
