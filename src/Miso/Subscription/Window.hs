-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Window
-- Copyright   :  (C) 2016-2025 David M. Johnson
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
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent.MVar
import           Control.Monad
import           Language.Javascript.JSaddle
import           Data.Aeson.Types (parseEither)
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.Effect
import qualified Miso.FFI.Internal as FFI
import           Miso.String
import           Miso.Canvas (Coord)
-----------------------------------------------------------------------------
-- | Captures window coordinates changes as they occur and writes them to
-- an event sink
windowCoordsSub :: (Coord -> action) -> Sub action
windowCoordsSub f = windowPointerMoveSub (f . client)
-----------------------------------------------------------------------------
-- | @windowSub eventName decoder toAction@ provides a subscription
-- to listen to window level events.
windowSub :: MisoString -> Decoder r -> (r -> action) -> Sub action
windowSub = windowSubWithOptions defaultOptions
-----------------------------------------------------------------------------
-- | @windowSubWithOptions options eventName decoder toAction@ provides a
-- subscription to listen to window level events.
windowSubWithOptions :: Options -> MisoString -> Decoder r -> (r -> action) -> Sub action
windowSubWithOptions Options{..} eventName Decoder {..} toAction = \sink ->
  createSub acquire release sink
    where
      release =
        FFI.windowRemoveEventListener eventName
      acquire sink =
        FFI.windowAddEventListener eventName $ \e -> do
          decodeAtVal <- toJSVal decodeAt
          v <- fromJSValUnchecked =<< FFI.eventJSON decodeAtVal e
          case parseEither decoder v of
            Left s ->
              error $ "windowSubWithOptions: Parse error on " <> unpack eventName <> ": " <> s
            Right r -> do
              when stopPropagation (FFI.eventStopPropagation e)
              when preventDefault (FFI.eventPreventDefault e)
              sink (toAction r)
-----------------------------------------------------------------------------
-- | @window.addEventListener ("pointermove", (event) => handle(event))@
-- A 'Sub' to handle @PointerEvent@s on window
windowPointerMoveSub :: (PointerEvent -> action) -> Sub action
windowPointerMoveSub = windowSub "pointermove" pointerDecoder
-----------------------------------------------------------------------------
-- | Utility function to allow resource finalization on window 'Sub'
createSub :: (Sink action -> JSM a) -> (a -> JSM b) -> Sub action
createSub acquire release = \sink -> do 
  mvar <- liftIO newEmptyMVar
  bracket
    (acquire sink)
    release
    (\_ -> liftIO (takeMVar mvar))
-----------------------------------------------------------------------------
