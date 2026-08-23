-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Text.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.Element.Text.Event
  ( -- *** Events
    onLayout
  , onLayoutWith
  , onLayoutMain
  , onLayoutMainWith
  , onSelectionChange
  , onSelectionChangeWith
  , onSelectionChangeMain
  , onSelectionChangeMainWith
    -- *** Types
  , LayoutEvent          (..)
  , LineInfo             (..)
  , Size                 (..)
  , SelectionChangeEvent (..)
  , Direction            (..)
    -- *** Decoders
  , layoutDecoder
  , selectionChangeDecoder
    -- *** Event Map
  , textEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
import           Miso.Event
import           Miso.JSON
----------------------------------------------------------------------------
import           Miso.Types (Attribute, EventHandler, DOMRef)
----------------------------------------------------------------------------
-- | The 'Events' map for the Lynx @<text>@ element.
--
-- Combine with other element maps using @<>@ and pass the result to
-- 'Miso.Native.native', so the delegator listens for these events.
--
-- @since 1.13.0.0
textEvents :: Events
textEvents
  = M.fromList
  [ ("layout", BUBBLE)
  , ("selectionchange", BUBBLE)
  ]
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#layout
--
-- The layout event returns the result information after text layout,
-- including the number of lines of the current text, and the start and
-- end positions of the text in each line relative to the entire text.
--
-- @
--
-- data Action = HandleLayout LayoutEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = text_ [ onLayout HandleLayout ] [ text "hi" ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleLayout LayoutEvent {..}) = io_ (consoleLog "layout event received")
--
-- @
--
onLayout :: (LayoutEvent -> action) -> Attribute model action
onLayout action = on "layout" layoutDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onLayout', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleLayout LayoutEvent
--
-- view_ [ event (static (onLayoutMain HandleLayout)) ] [ "some view" ]
-- @
--
onLayoutMain :: (LayoutEvent -> action) -> EventHandler model action
onLayoutMain action = onMain "layout" layoutDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onLayoutMain', but the handler also receives read-only access to the
-- @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleLayout LayoutEvent Model DOMRef
--
-- view_ [ event (static (onLayoutMainWith HandleLayout)) ] [ "some view" ]
-- @
--
onLayoutMainWith :: (LayoutEvent -> model -> DOMRef -> action) -> EventHandler model action
onLayoutMainWith action = onMain "layout" layoutDecoder action
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#selectionchange
--
-- This event is triggered whenever the selected text range changes.
--
-- @
--
-- data Action = HandleSelectionChange SelectionChangeEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = text_ [ onSelectionChange HandleSelectionChange ] [ text "hi" ]
--
-- update :: Action -> Effect context props Model Action
-- update (HandleSelectionChange SelectionChangeEvent {..}) =
--   io_ (consoleLog "selection change event received")
--
-- @
--
onSelectionChange :: (SelectionChangeEvent -> action) -> Attribute model action
onSelectionChange action = on "selectionchange" selectionChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onSelectionChange', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleSelectionChange SelectionChangeEvent
--
-- view_ [ event (static (onSelectionChangeMain HandleSelectionChange)) ] [ "some view" ]
-- @
--
onSelectionChangeMain :: (SelectionChangeEvent -> action) -> EventHandler model action
onSelectionChangeMain action = onMain "selectionchange" selectionChangeDecoder (\e _ _ -> action e)
-----------------------------------------------------------------------------
-- | Like 'onSelectionChangeMain', but the handler also receives read-only access
-- to the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleSelectionChange SelectionChangeEvent Model DOMRef
--
-- view_ [ event (static (onSelectionChangeMainWith HandleSelectionChange)) ] [ "some view" ]
-- @
--
onSelectionChangeMainWith :: (SelectionChangeEvent -> model -> DOMRef -> action) -> EventHandler model action
onSelectionChangeMainWith action = onMain "selectionchange" selectionChangeDecoder action
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'SelectionChangeEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
selectionChangeDecoder :: Decoder SelectionChangeEvent
selectionChangeDecoder = ["detail"] `at` parser
  where
    parser :: Value -> Parser SelectionChangeEvent
    parser = withObject "SelectionChangeEvent" $ \o -> do
      SelectionChangeEvent
        <$> o .: "start"
        <*> o .: "end"
        <*> o .: "direction"
-----------------------------------------------------------------------------
-- | Payload of a @<text>@ selection-change event: the @start@ and @end@
-- offsets of the new selection and the direction it was extended in.
-- 
-- @since 1.13.0.0
data SelectionChangeEvent
  = SelectionChangeEvent
  { start, end :: Double
  , direction :: Direction
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | The direction a text selection was extended in.
-- 
-- @since 1.13.0.0
data Direction = Forward | Backward
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON Direction where
  parseJSON = withText "Direction" $ \case
    "forward" -> pure Forward
    "backward" -> pure Backward
    x -> typeMismatch "Direction" (toJSON x)
-----------------------------------------------------------------------------
-- | Payload of a @<text>@ layout event: how many lines were laid out,
-- per-line detail, and the resulting size.
-- 
-- @since 1.13.0.0
data LayoutEvent
  = LayoutEvent
  { lineInfoLineCount     :: Double
  , lineInfoLines         :: [LineInfo]
  , lineInfoSize          :: Size
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | t'Decoder' producing a t'LayoutEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
layoutDecoder :: Decoder LayoutEvent
layoutDecoder = ["detail"] `at` do
  withObject "LayoutEvent" $ \o ->
    LayoutEvent
      <$> o .: "lineCount"
      <*> o .: "lines"
      <*> do
        s <- o .: "size"
        Size <$> s .: "width" <*> s .: "height"
-----------------------------------------------------------------------------
instance FromJSON LineInfo where
  parseJSON = withObject "lineInfo" $ \o ->
    LineInfo
      <$> o .: "start"
      <*> o .: "end"
      <*> o .: "ellipsisCount"
-----------------------------------------------------------------------------
-- | Per-line detail from a @<text>@ layout event: the character range the
-- line covers and how many characters were ellipsized.
-- 
-- @since 1.13.0.0
data LineInfo
  = LineInfo
  { lineInfoStart, lineInfoEnd, lineInfoEllipsisCount :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | The measured width and height of laid-out @<text>@ content, in px.
-- 
-- @since 1.13.0.0
data Size
  = Size
  { sizeWidth, sizeHeight :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Like 'onLayout', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onLayoutWith :: (LayoutEvent -> DOMRef -> action) -> Attribute model action
onLayoutWith action = on "layout" layoutDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
-- | Like 'onSelectionChange', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onSelectionChangeWith :: (SelectionChangeEvent -> DOMRef -> action) -> Attribute model action
onSelectionChangeWith action = on "selectionchange" selectionChangeDecoder $ \v _ domRef -> action v domRef
-----------------------------------------------------------------------------
