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
----------------------------------------------------------------------------
module Miso.Native.Element.List.Event
  ( -- *** Event
    onScroll
  , onScrollToUpper
  , onScrollToLower
  , onScrollStateChange
  , onLayoutComplete
  , onSnap
  -- *** Types
  , ScrollEvent (..)
  , SnapEvent (..)
  , LayoutCompleteEvent (..)
  , DiffResult (..)
  , ListEventSource (..)
  , Cell (..)
  , ScrollStateChange (..)
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
import           Miso.Types (Attribute)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
listEvents :: Events
listEvents
  = M.fromList
  [ ("scroll", BUBBLE)
  , ("scrolltoupper", BUBBLE)
  , ("scrolltolower", BUBBLE)
  , ("scrollend", BUBBLE)
  , ("contentsizechanged", BUBBLE)
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
      <*> o .: "listEventSource"
      <*> o .: "attachedCells"
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
  { cellId :: Double
  -- ^ Node id
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
  deriving (Show, Eq, Enum)
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
-- Enable 'needLayoutCompleteInfo' to use.
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
-- view :: Model -> View Model Action
-- view model = list_ defaultListOptions [ onScroll HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScroll :: (ScrollEvent -> action) -> Attribute action
onScroll action = on "scroll" scrollDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scrolltoupper
--
-- Callback triggered when scrolling to the top of \<list\>. The trigger
-- position of this callback can be controlled by 'upperThresholdItemCount'.
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: Model -> View Model Action
-- view model = list_ defaultListOptions [ onScrollToUpper HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScrollToUpper :: (ScrollEvent -> action) -> Attribute action
onScrollToUpper action = on "scrolltoupper" scrollDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scrolltolower
--
-- Callback triggered when scrolling to the bottom of \<list\>. The trigger
-- position of this callback can be controlled by 'lowerThresholdItemCount_'
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: Model -> View Model Action
-- view model = list_ defaultListOptions [ onScrollToLower HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScrollToLower :: (ScrollEvent -> action) -> Attribute action
onScrollToLower action = on "scrolltolower" scrollDecoder (\x _ -> action x)
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
-- view :: Model -> View Model Action
-- view model = list_ defaultListOptions [ onScrollStateChange HandleScrollState ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll Stationary) =
--   io_ (consoleLog "Received Stationary scroll state change")
-- update _ = pure ()
--
-- @
--
onScrollStateChange :: (ScrollStateChange -> action) -> Attribute action
onScrollStateChange action = on "scrollstatechange" scrollStateDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#layoutcomplete
--
-- Callback triggered after \<list\> layout is complete.
--
-- @
--
-- data Action = HandleLayout LayoutCompleteEvent
--
-- view :: Model -> View Model Action
-- view model = list_ defaultListOptions [ onLayoutComplete HandleLayout ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleLayout LayoutCompleteEvent {..}) =
--   io_ (consoleLog "Received LayoutCompleteEvent")
--
-- @
--
onLayoutComplete :: (LayoutCompleteEvent -> action) -> Attribute action
onLayoutComplete action = on "layoutcomplete" layoutCompleteDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#snap
--
-- Callback when pagination scrolling is about to occur.
--
-- @
--
-- data Action = HandleSnap SnapEvent
--
-- view :: Model -> View Model Action
-- view model = list_ defaultListOptions [ onSnap HandleSnap ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleSnap SnapEvent {..}) =
--   io_ (consoleLog "Received SnapEvent")
--
-- @
--
onSnap :: (SnapEvent -> action) -> Attribute action
onSnap action = on "snap" snapDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
