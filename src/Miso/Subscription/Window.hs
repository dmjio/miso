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
  ) where
-----------------------------------------------------------------------------
import Control.Monad
import Language.Javascript.JSaddle
import Data.Aeson.Types (parseEither)
-----------------------------------------------------------------------------
import Miso.Event
import Miso.Effect
import qualified Miso.FFI.Internal as FFI
import Miso.String
-----------------------------------------------------------------------------
-- | Captures window coordinates changes as they occur and writes them to
-- an event sink
windowCoordsSub :: ((Int, Int) -> action) -> Sub action
windowCoordsSub f = \sink -> do
  sink . f =<< (,) <$> FFI.windowInnerHeight <*> FFI.windowInnerWidth
  FFI.windowAddEventListener "resize" $
    \windowEvent -> do
      target <- getProp "target" (Object windowEvent)
      Just w <- fromJSVal =<< getProp "innerWidth" (Object target)
      Just h <- fromJSVal =<< getProp "innerHeight" (Object target)
      sink $ f (h, w)
-----------------------------------------------------------------------------
-- | @windowSub eventName decoder toAction@ provides a subscription
-- to listen to window level events.
windowSub :: MisoString -> Decoder r -> (r -> action) -> Sub action
windowSub = windowSubWithOptions defaultOptions
-----------------------------------------------------------------------------
-- | @windowSubWithOptions options eventName decoder toAction@ provides a
-- subscription to listen to window level events.
windowSubWithOptions :: Options -> MisoString -> Decoder r -> (r -> action) -> Sub action
windowSubWithOptions Options{..} eventName Decoder{..} toAction = \sink ->
  FFI.windowAddEventListener eventName $ \e -> do
      decodeAtVal <- toJSVal decodeAt
      Just v <- fromJSVal =<< FFI.eventJSON decodeAtVal e
      case parseEither decoder v of
        Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
        Right r -> do
          when stopPropagation (FFI.eventStopPropagation e)
          when preventDefault (FFI.eventPreventDefault e)
          sink (toAction r)
-----------------------------------------------------------------------------
-- | window.addEventListener ("pointermove", (event) => handle(event))
-- A 'Sub' for handler PointerEvent on window
windowPointerMoveSub :: (PointerEvent -> action) -> Sub action
windowPointerMoveSub = windowSub "pointermove" pointerDecoder
