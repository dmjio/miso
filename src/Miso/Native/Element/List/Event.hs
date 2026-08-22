-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.List.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.Element.List.Event
  ( -- *** Event
    onScroll
  , onScrollWith
  , onScrollMain
  , onScrollMainWith
  , onScrollToUpper
  , onScrollToUpperWith
  , onScrollToUpperMain
  , onScrollToUpperMainWith
  , onScrollToLower
  , onScrollToLowerWith
  , onScrollToLowerMain
  , onScrollToLowerMainWith
  , onScrollStateChange
  , onScrollStateChangeWith
  , onScrollStateChangeMain
  , onScrollStateChangeMainWith
  , onLayoutComplete
  , onLayoutCompleteWith
  , onLayoutCompleteMain
  , onLayoutCompleteMainWith
  , onSnap
  , onSnapWith
  , onSnapMain
  , onSnapMainWith
  -- *** Types
  , ScrollEvent (..)
  , SnapEvent (..)
  , LayoutCompleteEvent (..)
  , DiffResult (..)
  , ListEventSource (..)
  , Cell (..)
  , ScrollStateChange (..)
  , ListItemInfo (..)
  -- *** Decoder
  , scrollDecoder
  , snapDecoder
  , layoutCompleteDecoder
  -- *** Event Map
  , listEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Event
import           Miso.JSON
import           Miso.Types (Attribute, EventHandler, DOMRef)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
listEvents :: Events
listEvents
  = M.fromList
  [ ("scroll", BUBBLE)
  , ("scrolltoupper", BUBBLE)
  , ("scrolltolower", BUBBLE)
  , ("scrollstatechange", BUBBLE)
  , ("layoutcomplete", BUBBLE)
  , ("snap", BUBBLE)
  ]
-----------------------------------------------------------------------------
scrollDecoder :: Decoder ScrollEvent
scrollDecoder = ["detail"] `at` parseJSON
-----------------------------------------------------------------------------
instance FromJSON ScrollEvent where
  parseJSON = withObject "ScrollEvent" $ \o ->
    ScrollEvent
      <$> o .:? "deltaX" .!= 0
      <*> o .:? "deltaY" .!= 0
      <*> o .:? "scrollLeft" .!= 0
      <*> o .:? "scrollTop" .!= 0
      <*> o .:? "scrollWidth" .!= 0
      <*> o .:? "scrollHeight" .!= 0
      <*> o .:? "listWidth" .!= 0
      <*> o .:? "listHeight" .!= 0
      -- `eventSource`/`attachedCells` are declared required in Lynx's
      -- ListScrollInfo, but `attachedCells` is only populated when
      -- `need-visible-item-info` is enabled (otherwise absent). Decode
      -- defensively so a @scroll@ event without them still succeeds.
      <*> o .:? "eventSource" .!= SCROLL
      <*> o .:? "attachedCells" .!= []
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scroll
data ScrollEvent
  = ScrollEvent
  { deltaX, deltaY :: Double
  -- ^ Horizontal / vertical scroll offset since the last scroll, in px
  , scrollLeft, scrollTop :: Double
  -- ^ Current horizontal / vertical scroll offset, in px
  , scrollWidth, scrollHeight :: Double
  -- ^ Current content area height / width, in px
  , listWidth, listHeight :: Double
  -- ^ List width / height in px
  , listEventSource :: ListEventSource
  -- ^ Scroll event source
  , attachedCells :: [Cell]
  -- ^ Attached cells
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Cell
  = Cell
  { cellId :: MisoString
  -- ^ Node id (Lynx types this @ListAttachedCell.id@ as a string)
  , cellItemKey :: MisoString
  -- ^ Node item-key
  , cellIndex, cellLeft, cellTop, cellRight, cellBottom :: Double
  -- ^ Node left/top/right/bottom boundary position relative to list, in px
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \cell -> Cell
    <$> cell .: "id"
    <*> cell .: "itemKey"
    <*> cell .: "index"
    <*> cell .: "left"
    <*> cell .: "top"
    <*> cell .: "right"
    <*> cell .: "bottom"
-----------------------------------------------------------------------------
data ListEventSource
  = DIFF
  | LAYOUT
  | SCROLL
  deriving (Show, Eq, Enum)
-----------------------------------------------------------------------------
instance FromJSON ListEventSource where
  parseJSON = withNumber "ListEventSource" $ \case
    0 -> pure DIFF
    1 -> pure LAYOUT
    2 -> pure SCROLL
    x -> typeMismatch "ListEventSource" (Number x)
-----------------------------------------------------------------------------
data ScrollStateChange
  = Stationary
  | Dragging
  | InertialScrolling
  | SmoothAnimationScrolling
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Numbering matches Lynx's @ScrollState@ enum (@kIdle = 1@ … @kScrollAnimation
-- = 4@), so 'toEnum'\/'fromEnum' agree with the wire and with 'FromJSON' below.
-- (A derived 'Enum' would be 0-based and disagree with the values Lynx sends.)
instance Enum ScrollStateChange where
  fromEnum Stationary               = 1
  fromEnum Dragging                 = 2
  fromEnum InertialScrolling        = 3
  fromEnum SmoothAnimationScrolling = 4
  toEnum 1 = Stationary
  toEnum 2 = Dragging
  toEnum 3 = InertialScrolling
  toEnum 4 = SmoothAnimationScrolling
  toEnum n = error ("ScrollStateChange.toEnum: bad argument " <> show n)
-----------------------------------------------------------------------------
instance FromJSON ScrollStateChange where
  parseJSON = withNumber "ScrollStateChange" $ \case
    1 -> pure Stationary
    2 -> pure Dragging
    3 -> pure InertialScrolling
    4 -> pure SmoothAnimationScrolling
    x -> typeMismatch "ScrollStateChange" (Number x)
-----------------------------------------------------------------------------
scrollStateDecoder :: Decoder ScrollStateChange
scrollStateDecoder = ["detail"] `at` withObject "ScrollStateChange" (.: "state")
-----------------------------------------------------------------------------
data SnapEvent
  = SnapEvent
  { position :: Double
  -- ^ The index of the node that will be paginated to
  , currentScrollLeft :: Double
  -- ^ Current horizontal scroll offset, in px
  , currentScrollTop :: Double
  -- ^ Current vertical scroll offset, in px
  , targetScrollLeft :: Double
  -- ^ Target horizontal scroll offset for pagination, in px
  , targetScrollTop :: Double
  -- ^ Target vertical scroll offset for pagination, in px
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
snapDecoder :: Decoder SnapEvent
snapDecoder = ["detail"] `at` do
  withObject "SnapEvent" $ \o ->
    SnapEvent
      <$> o .: "position"
      <*> o .: "currentScrollLeft"
      <*> o .: "currentScrollTop"
      <*> o .: "targetScrollLeft"
      <*> o .: "targetScrollTop"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#layoutcomplete
--
-- Enable @needLayoutCompleteInfo@ to use.
--
data LayoutCompleteEvent
  = LayoutCompleteEvent
  { layoutId :: Double
  , scrollInfo :: ScrollEvent
  -- ^ Current horizontal scroll offset, in px
  , diffResult :: Maybe DiffResult
  -- ^ Current vertical scroll offset, in px
  , visibleCellsAfterUpdate :: [ListItemInfo]
  -- ^ Target horizontal scroll offset for pagination, in px
  , visibleCellsBeforeUpdate :: [ListItemInfo]
  -- ^ Target vertical scroll offset for pagination, in px
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data DiffResult
  = DiffResult
  { insertions :: [Double]
  , moveFrom :: [Double]
  , moveTo :: [Double]
  , removals :: [Double]
  , updateFrom :: [Double]
  , updateTo :: [Double]
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON DiffResult where
  parseJSON = withObject "DiffResult" $ \o ->
    DiffResult
      <$> o .: "insertions"
      <*> o .: "move_from"
      <*> o .: "move_to"
      <*> o .: "removals"
      <*> o .: "update_from"
      <*> o .: "update_to"
-----------------------------------------------------------------------------
data ListItemInfo
  = ListItemInfo
  { listItemInfoHeight :: Double
    -- ^ Child node height
  , listItemInfoWidth :: Double
    -- ^ Child node width
  , listItemInfoItemKey :: MisoString
    -- ^ Child node ItemKey
  , listItemInfoIsBinding :: Bool
    -- ^ Whether the child node is in rendering state
  , listItemInfoOriginX :: Double
    -- ^ X coordinate position of the child node relative to the entire scroll area
  , listItemInfoOriginY :: Double
    -- ^ Y coordinate position of the child node relative to the entire scroll area
  , listItemInfoUpdated :: Bool
    -- ^ Whether the child node has been updated
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON ListItemInfo where
  parseJSON = withObject "ListItemInfo" $ \o ->
    ListItemInfo
      <$> o .: "height"
      <*> o .: "width"
      <*> o .: "itemKey"
      <*> o .: "isBinding"
      <*> o .: "originX"
      <*> o .: "originY"
      <*> o .: "updated"
-----------------------------------------------------------------------------
layoutCompleteDecoder :: Decoder LayoutCompleteEvent
layoutCompleteDecoder = ["detail"] `at` do
  withObject "LayoutCompleteEvent" $ \o ->
    LayoutCompleteEvent
      <$> o .: "layout-id"
      <*> o .: "scrollInfo"
      <*> o .: "diffResult"
      <*> o .: "visibleCellsAfterUpdate"
      <*> o .: "visibleCellsBeforeUpdate"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scroll
--
-- \<list\> scroll event.
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = list_ defaultListOptions [ onScroll HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScroll :: (ScrollEvent -> action) -> Attribute model action
onScroll action = on "scroll" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScroll', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleScroll ScrollEvent
--
-- view_ [ event (static (onScrollMain HandleScroll)) ] [ "some view" ]
-- @
--
onScrollMain :: (ScrollEvent -> action) -> EventHandler model action
onScrollMain action = onMain "scroll" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleScroll ScrollEvent Model DOMRef
--
-- view_ [ event (static (onScrollMainWith HandleScroll)) ] [ "some view" ]
-- @
--
onScrollMainWith :: (ScrollEvent -> model -> DOMRef -> action) -> EventHandler model action
onScrollMainWith action = onMain "scroll" scrollDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scrolltoupper
--
-- Callback triggered when scrolling to the top of \<list\>. The trigger
-- position of this callback can be controlled by @upperThresholdItemCount@.
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = list_ defaultListOptions [ onScrollToUpper HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScrollToUpper :: (ScrollEvent -> action) -> Attribute model action
onScrollToUpper action = on "scrolltoupper" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollToUpper', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleScroll ScrollEvent
--
-- view_ [ event (static (onScrollToUpperMain HandleScroll)) ] [ "some view" ]
-- @
--
onScrollToUpperMain :: (ScrollEvent -> action) -> EventHandler model action
onScrollToUpperMain action = onMain "scrolltoupper" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollToUpperMain', but the handler also receives read-only access
-- to the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleScroll ScrollEvent Model DOMRef
--
-- view_ [ event (static (onScrollToUpperMainWith HandleScroll)) ] [ "some view" ]
-- @
--
onScrollToUpperMainWith :: (ScrollEvent -> model -> DOMRef -> action) -> EventHandler model action
onScrollToUpperMainWith action = onMain "scrolltoupper" scrollDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scrolltolower
--
-- Callback triggered when scrolling to the bottom of \<list\>. The trigger
-- position of this callback can be controlled by 'Miso.Native.Element.List.Property.lowerThresholdItemCount_'
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = list_ defaultListOptions [ onScrollToLower HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScrollToLower :: (ScrollEvent -> action) -> Attribute model action
onScrollToLower action = on "scrolltolower" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollToLower', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleScroll ScrollEvent
--
-- view_ [ event (static (onScrollToLowerMain HandleScroll)) ] [ "some view" ]
-- @
--
onScrollToLowerMain :: (ScrollEvent -> action) -> EventHandler model action
onScrollToLowerMain action = onMain "scrolltolower" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollToLowerMain', but the handler also receives read-only access
-- to the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleScroll ScrollEvent Model DOMRef
--
-- view_ [ event (static (onScrollToLowerMainWith HandleScroll)) ] [ "some view" ]
-- @
--
onScrollToLowerMainWith :: (ScrollEvent -> model -> DOMRef -> action) -> EventHandler model action
onScrollToLowerMainWith action = onMain "scrolltolower" scrollDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scrollstatechange
--
-- Callback triggered when the scroll state of \<list\> changes. The state
-- field in the event parameter's detail indicates the scroll state:
-- * 1 for stationary
-- * 2 for dragging
-- * 3 for inertial scrolling
-- * 4 for smooth animation scrolling.
--
-- @
--
-- data Action = HandleScrollState ScrollStateChange
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = list_ defaultListOptions [ onScrollStateChange HandleScrollState ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll Stationary) =
--   io_ (consoleLog "Received Stationary scroll state change")
-- update _ = pure ()
--
-- @
--
onScrollStateChange :: (ScrollStateChange -> action) -> Attribute model action
onScrollStateChange action = on "scrollstatechange" scrollStateDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollStateChange', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleScrollState ScrollStateChange
--
-- view_ [ event (static (onScrollStateChangeMain HandleScrollState)) ] [ "some view" ]
-- @
--
onScrollStateChangeMain :: (ScrollStateChange -> action) -> EventHandler model action
onScrollStateChangeMain action = onMain "scrollstatechange" scrollStateDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollStateChangeMain', but the handler also receives read-only
-- access to the @model@ and the target element's 'DOMRef' (for imperative MTS
-- mutation).
--
-- @
-- data Action = HandleScrollState ScrollStateChange Model DOMRef
--
-- view_ [ event (static (onScrollStateChangeMainWith HandleScrollState)) ] [ "some view" ]
-- @
--
onScrollStateChangeMainWith :: (ScrollStateChange -> model -> DOMRef -> action) -> EventHandler model action
onScrollStateChangeMainWith action = onMain "scrollstatechange" scrollStateDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#layoutcomplete
--
-- Callback triggered after \<list\> layout is complete.
--
-- @
--
-- data Action = HandleLayout LayoutCompleteEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = list_ defaultListOptions [ onLayoutComplete HandleLayout ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleLayout LayoutCompleteEvent {..}) =
--   io_ (consoleLog "Received LayoutCompleteEvent")
--
-- @
--
onLayoutComplete :: (LayoutCompleteEvent -> action) -> Attribute model action
onLayoutComplete action = on "layoutcomplete" layoutCompleteDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onLayoutComplete', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleLayout LayoutCompleteEvent
--
-- view_ [ event (static (onLayoutCompleteMain HandleLayout)) ] [ "some view" ]
-- @
--
onLayoutCompleteMain :: (LayoutCompleteEvent -> action) -> EventHandler model action
onLayoutCompleteMain action = onMain "layoutcomplete" layoutCompleteDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onLayoutCompleteMain', but the handler also receives read-only access
-- to the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleLayout LayoutCompleteEvent Model DOMRef
--
-- view_ [ event (static (onLayoutCompleteMainWith HandleLayout)) ] [ "some view" ]
-- @
--
onLayoutCompleteMainWith :: (LayoutCompleteEvent -> model -> DOMRef -> action) -> EventHandler model action
onLayoutCompleteMainWith action = onMain "layoutcomplete" layoutCompleteDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#snap
--
-- Callback when pagination scrolling is about to occur.
--
-- @
--
-- data Action = HandleSnap SnapEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = list_ defaultListOptions [ onSnap HandleSnap ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleSnap SnapEvent {..}) =
--   io_ (consoleLog "Received SnapEvent")
--
-- @
--
onSnap :: (SnapEvent -> action) -> Attribute model action
onSnap action = on "snap" snapDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onSnap', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleSnap SnapEvent
--
-- view_ [ event (static (onSnapMain HandleSnap)) ] [ "some view" ]
-- @
--
onSnapMain :: (SnapEvent -> action) -> EventHandler model action
onSnapMain action = onMain "snap" snapDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onSnapMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleSnap SnapEvent Model DOMRef
--
-- view_ [ event (static (onSnapMainWith HandleSnap)) ] [ "some view" ]
-- @
--
onSnapMainWith :: (SnapEvent -> model -> DOMRef -> action) -> EventHandler model action
onSnapMainWith action = onMain "snap" snapDecoder action
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Like 'onScroll', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onScrollWith :: (ScrollEvent -> DOMRef -> action) -> Attribute model action
onScrollWith action = on "scroll" scrollDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onScrollToUpper', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onScrollToUpperWith :: (ScrollEvent -> DOMRef -> action) -> Attribute model action
onScrollToUpperWith action = on "scrolltoupper" scrollDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onScrollToLower', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onScrollToLowerWith :: (ScrollEvent -> DOMRef -> action) -> Attribute model action
onScrollToLowerWith action = on "scrolltolower" scrollDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onScrollStateChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onScrollStateChangeWith :: (ScrollStateChange -> DOMRef -> action) -> Attribute model action
onScrollStateChangeWith action = on "scrollstatechange" scrollStateDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onLayoutComplete', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onLayoutCompleteWith :: (LayoutCompleteEvent -> DOMRef -> action) -> Attribute model action
onLayoutCompleteWith action = on "layoutcomplete" layoutCompleteDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onSnap', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onSnapWith :: (SnapEvent -> DOMRef -> action) -> Attribute model action
onSnapWith action = on "snap" snapDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
