-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Text.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Text.Method
  ( -- *** Methods
    setTextSelection
  , getTextBoundingRect
  , getSelectedText
  -- *** Types
  , SetTextSelection (..)
  -- *** Smart constructors
  , defaultGetTextBoundingRect
  ) where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
data SetTextSelection
  = SetTextSelection
  { startX, startY :: Double
  -- ^ X/Y-coordinate of the selection start relative to the element
  , endX, endY :: Double
  -- ^ X/Y-coordinate of the selection end relative to the element
  , showStartHandle, showEndHandle :: Bool
  -- ^ Whether to show or hide the start/end handle
  }
-----------------------------------------------------------------------------
instance ToJSVal SetTextSelection where
  toJSVal SetTextSelection {..} = do
    o <- create
    set "startX" startX o
    set "startY" startY o
    set "endX" endX o
    set "endY" endY o
    set "showStartHandle" showStartHandle o
    set "showEndHandle" showEndHandle o
    toJSVal o
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/text.html#setTextSelection>
--
-- This method sets the selected text based on start and end positions and controls the visibility of selection handles. The response res contains:
--
-- @
--
-- data Action = SetText | TextSet | SetTextError MisoString
--
-- update :: Action -> Effect props model Action
-- update SetText = setTextSelection "someImageId" SetText SetTextError
-- update TextSet = io_ (consoleLog "text was set")
-- update (SetTextError e) = io_ (consoleLog e)
--
-- @
--
setTextSelection
  :: MisoString
  -> SetTextSelection
  -> action
  -> (MisoString -> action)
  -> Effect context props model action
setTextSelection selector params action =
  invokeExec "setTextSelection" selector params (\() -> action)
-----------------------------------------------------------------------------
data GetTextBoundingRect
  = GetTextBoundingRect
  { start, end :: Double
  -- ^ X/Y-coordinate of the selection start relative to the element
  }
-----------------------------------------------------------------------------
instance ToJSVal GetTextBoundingRect where
  toJSVal GetTextBoundingRect {..} = do
    o <- create
    set "start" start o
    set "end" end o
    toJSVal o
-----------------------------------------------------------------------------
defaultGetTextBoundingRect :: GetTextBoundingRect
defaultGetTextBoundingRect = GetTextBoundingRect 0 0
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#gettextboundingrect
--
-- This method retrieves the bounding box of a specific range of text.
--
-- @
--
-- data Action = RectReceived Rect | GetRect | GotError MisoString
--
-- update :: Action -> Effect props model Action
-- update GetRect = getTextBoundingRect "#box" defaultGetTextBoundingRect RectReceived GotError
-- update (RectReceived rect) = io_ $ consoleLog ("got rect")
-- update (GotError errMsg) = io_ (consoleLog errMsg)
--
-- @
--
getTextBoundingRect
  :: MisoString
  -> GetTextBoundingRect
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
getTextBoundingRect = invokeExec "getTextBoundingRect"
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/text.html#getselectedtext
--
-- This method retrieves the string content of the currently selected text.
--
-- @
--
-- data Action = TextReceived MisoString | GetText | GotError MisoString
--
-- update :: Action -> Effect props model Action
-- update GetText = getSelectedText "#box" TextReceived GotError
-- update (TextReceived txt) = io_ (consoleLog ("got text: " <> txt))
-- update (GotError errMsg) = io_ (consoleLog errMsg)
--
-- @
--
getSelectedText
  :: MisoString
  -> (MisoString -> action)
  -> (MisoString -> action)
  -> Effect context props model action
getSelectedText selector = invokeExec "getSelectedText" selector ()
-----------------------------------------------------------------------------
