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
----------------------------------------------------------------------------
module Miso.Native.X.Element.ScrollCoordinator.Event
  ( -- *** Events
    onOffset
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
import           Miso.Types (Attribute)
-----------------------------------------------------------------------------
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
onOffset :: (ScrollCoordinatorOffsetEvent -> action) -> Attribute action
onOffset action = on "offset" offsetDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
