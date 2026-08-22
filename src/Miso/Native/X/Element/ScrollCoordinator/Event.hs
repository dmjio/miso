-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.ScrollCoordinator.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.ScrollCoordinator.Event
  ( -- *** Events
    onOffset
  , onOffsetWith
  , onOffsetMain
  , onOffsetMainWith
    -- *** Types
  , ScrollCoordinatorOffsetEvent (..)
    -- *** Decoders
  , offsetDecoder
    -- *** Event Map
  , scrollCoordinatorEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.Types (Attribute, EventHandler, DOMRef)
-----------------------------------------------------------------------------
-- | The 'Events' map for the Lynx @<scrollcoordinator>@ element.
--
-- Combine with other element maps using @<>@ and pass the result to
-- 'Miso.Native.native', so the delegator listens for these events.
--
-- @since 1.13.0.0
scrollCoordinatorEvents :: Events
scrollCoordinatorEvents = M.fromList [ ("offset", BUBBLE) ]
-----------------------------------------------------------------------------
-- | Payload of the @bindoffset@ event.
data ScrollCoordinatorOffsetEvent
  = ScrollCoordinatorOffsetEvent
  { height :: Double
    -- ^ The scrollable distance
  , offset :: Double
    -- ^ The header scroll offset
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'ScrollCoordinatorOffsetEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
offsetDecoder :: Decoder ScrollCoordinatorOffsetEvent
offsetDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      ScrollCoordinatorOffsetEvent
        <$> o .: "height"
        <*> o .: "offset"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-coordinator.html#bindoffset
--
-- Callback reporting folding progress.
--
onOffset :: (ScrollCoordinatorOffsetEvent -> action) -> Attribute model action
onOffset action = on "offset" offsetDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onOffset', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Offset ScrollCoordinatorOffsetEvent
--
-- view_ [ event (static (onOffsetMain Offset)) ] [ "some view" ]
-- @
--
onOffsetMain :: (ScrollCoordinatorOffsetEvent -> action) -> EventHandler model action
onOffsetMain action = onMain "offset" offsetDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onOffsetMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Offset ScrollCoordinatorOffsetEvent Model DOMRef
--
-- view_ [ event (static (onOffsetMainWith Offset)) ] [ "some view" ]
-- @
--
onOffsetMainWith :: (ScrollCoordinatorOffsetEvent -> model -> DOMRef -> action) -> EventHandler model action
onOffsetMainWith action = onMain "offset" offsetDecoder action
-----------------------------------------------------------------------------
-- | Like 'onOffset', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onOffsetWith :: (ScrollCoordinatorOffsetEvent -> DOMRef -> action) -> Attribute model action
onOffsetWith action = on "offset" offsetDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
