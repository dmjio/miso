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
----------------------------------------------------------------------------
module Miso.Native.Element.Text.Event
  ( -- *** Events
    onLayout
  , onSelectionChange
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
import           Miso.Event
import           Miso.JSON
----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Miso.Types (Attribute)
----------------------------------------------------------------------------
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
-- view :: Model -> View Model Action
-- view model = text_ [ onLayout HandleLayout ] [ text "hi" ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleLayout LayoutEvent {..}) = io_ (consoleLog "layout event received")
--
-- @
--
onLayout :: (LayoutEvent -> action) -> Attribute action
onLayout action = on "layout" layoutDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#selectionchange
--
-- This event is triggered whenever the selected text range changes.
--
-- @
--
-- data Action = HandleSelectionChange SelectionChangeEvent
--
-- view :: Model -> View Model Action
-- view model = text_ [ onSelectionChange HandleSelectionChange ] [ text "hi" ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleSelectionChange SelectionChangeEvent {..}) =
--   io_ (consoleLog "selection change event received")
--
-- @
--
onSelectionChange :: (SelectionChangeEvent -> action) -> Attribute action
onSelectionChange action = on "selectionchange" selectionChangeDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
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
data SelectionChangeEvent
  = SelectionChangeEvent
  { start, end :: Double
  , direction :: Direction
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Direction = Forward | Backward
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON Direction where
  parseJSON = withText "Direction" $ \case
    "forward" -> pure Forward
    "backward" -> pure Backward
    x -> typeMismatch "Direction" (String x)
-----------------------------------------------------------------------------
data LayoutEvent
  = LayoutEvent
  { lineInfoLineCount     :: Double
  , lineInfoLines         :: [LineInfo]
  , lineInfoSize          :: Size
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
layoutDecoder :: Decoder LayoutEvent
layoutDecoder = [] `at` do
  withObject "LayoutEvent" $ \o ->
    LayoutEvent
      <$> o .: "lineCount"
      <*> o .: "lineInfo"
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
data LineInfo
  = LineInfo
  { lineInfoStart, lineInfoEnd, lineInfoEllipsisCount :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Size
  = Size
  { sizeWidth, sizeHeight :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
