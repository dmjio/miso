-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.X.Element.Viewpager.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.X.Element.Viewpager.Event
  ( -- *** Events
    onChange
  , onOffsetChange
  , onWillChange
    -- *** Types
  , ViewpagerChangeEvent (..)
    -- *** Decoders
  , viewpagerChangeDecoder
  , offsetChangeDecoder
    -- *** Event Map
  , viewpagerEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.Types (Attribute)
-----------------------------------------------------------------------------
viewpagerEvents :: Events
viewpagerEvents
  = M.fromList
  [ ("change", BUBBLE)
  , ("offsetchange", BUBBLE)
  , ("willchange", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | Payload of the @bindchange@ and @bindwillchange@ events.
data ViewpagerChangeEvent
  = ViewpagerChangeEvent
  { index :: Int
    -- ^ The page index
  , isDragged :: Bool
    -- ^ Whether the change was user-initiated
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
viewpagerChangeDecoder :: Decoder ViewpagerChangeEvent
viewpagerChangeDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      ViewpagerChangeEvent
        <$> o .: "index"
        <*> o .: "isDragged"
-----------------------------------------------------------------------------
offsetChangeDecoder :: Decoder Double
offsetChangeDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o -> o .: "offset"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#bindchange
--
-- Triggered with the current page index after the transition completes.
--
onChange :: (ViewpagerChangeEvent -> action) -> Attribute action
onChange action = on "change" viewpagerChangeDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#bindoffsetchange
--
-- Triggered with the scrolling progress during a page transition.
--
onOffsetChange :: (Double -> action) -> Attribute action
onOffsetChange action = on "offsetchange" offsetChangeDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#bindwillchange
--
-- Triggered with the next page index before the transition starts.
--
onWillChange :: (ViewpagerChangeEvent -> action) -> Attribute action
onWillChange action = on "willchange" viewpagerChangeDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
