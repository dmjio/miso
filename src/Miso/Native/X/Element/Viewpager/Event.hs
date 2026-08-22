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
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.X.Element.Viewpager.Event
  ( -- *** Events
    onChange
  , onChangeWith
  , onChangeMain
  , onChangeMainWith
  , onOffsetChange
  , onOffsetChangeWith
  , onOffsetChangeMain
  , onOffsetChangeMainWith
  , onWillChange
  , onWillChangeWith
  , onWillChangeMain
  , onWillChangeMainWith
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
import           Miso.Types (DOMRef, Attribute, EventHandler)
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
onChange :: (ViewpagerChangeEvent -> action) -> Attribute model action
onChange action = on "change" viewpagerChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onChange', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = Changed ViewpagerChangeEvent
--
-- view_ [ event (static (onChangeMain Changed)) ] [ "some view" ]
-- @
--
onChangeMain :: (ViewpagerChangeEvent -> action) -> EventHandler model action
onChangeMain action = onMain "change" viewpagerChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onChangeMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = Changed ViewpagerChangeEvent Model DOMRef
--
-- view_ [ event (static (onChangeMainWith Changed)) ] [ "some view" ]
-- @
--
onChangeMainWith :: (ViewpagerChangeEvent -> model -> DOMRef -> action) -> EventHandler model action
onChangeMainWith action = onMain "change" viewpagerChangeDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#bindoffsetchange
--
-- Triggered with the scrolling progress during a page transition.
--
onOffsetChange :: (Double -> action) -> Attribute model action
onOffsetChange action = on "offsetchange" offsetChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onOffsetChange', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = OffsetChanged Double
--
-- view_ [ event (static (onOffsetChangeMain OffsetChanged)) ] [ "some view" ]
-- @
--
onOffsetChangeMain :: (Double -> action) -> EventHandler model action
onOffsetChangeMain action = onMain "offsetchange" offsetChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onOffsetChangeMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = OffsetChanged Double Model DOMRef
--
-- view_ [ event (static (onOffsetChangeMainWith OffsetChanged)) ] [ "some view" ]
-- @
--
onOffsetChangeMainWith :: (Double -> model -> DOMRef -> action) -> EventHandler model action
onOffsetChangeMainWith action = onMain "offsetchange" offsetChangeDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/viewpager.html#bindwillchange
--
-- Triggered with the next page index before the transition starts.
--
onWillChange :: (ViewpagerChangeEvent -> action) -> Attribute model action
onWillChange action = on "willchange" viewpagerChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onWillChange', but dispatched on the Lynx __main thread__ ('MTS').
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = WillChange ViewpagerChangeEvent
--
-- view_ [ event (static (onWillChangeMain WillChange)) ] [ "some view" ]
-- @
--
onWillChangeMain :: (ViewpagerChangeEvent -> action) -> EventHandler model action
onWillChangeMain action = onMain "willchange" viewpagerChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onWillChangeMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = WillChange ViewpagerChangeEvent Model DOMRef
--
-- view_ [ event (static (onWillChangeMainWith WillChange)) ] [ "some view" ]
-- @
--
onWillChangeMainWith :: (ViewpagerChangeEvent -> model -> DOMRef -> action) -> EventHandler model action
onWillChangeMainWith action = onMain "willchange" viewpagerChangeDecoder action
-----------------------------------------------------------------------------
-- | Like 'onChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onChangeWith :: (ViewpagerChangeEvent -> DOMRef -> action) -> Attribute model action
onChangeWith action = on "change" viewpagerChangeDecoder $ \vpce _ domRef -> action vpce domRef
-----------------------------------------------------------------------------
-- | Like 'onOffsetChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onOffsetChangeWith :: (Double -> DOMRef -> action) -> Attribute model action
onOffsetChangeWith action = on "offsetchange" offsetChangeDecoder $ \vpce _ domRef -> action vpce domRef
-----------------------------------------------------------------------------
-- | Like 'onWillChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onWillChangeWith :: (ViewpagerChangeEvent -> DOMRef -> action) -> Attribute model action
onWillChangeWith action = on "willchange" viewpagerChangeDecoder $ \vpce _ domRef -> action vpce domRef
-----------------------------------------------------------------------------
