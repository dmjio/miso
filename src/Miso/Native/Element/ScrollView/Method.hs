-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.ScrollView.Method
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.ScrollView.Method
  ( -- *** Methods
    scrollTo
  , autoScroll
  , scrollIntoView
  , scrollBy
  , getScrollInfo
  -- *** Types
  , ScrollTo (..)
  , AutoScroll (..)
  , ScrollIntoView (..)
  , ScrollBy (..)
  , ScrollInfo (..)
  -- *** Smart constructors
  , defaultScrollTo
  , defaultAutoScroll
  , defaultScrollIntoView
  , defaultScrollBy
  ) where
-----------------------------------------------------------------------------
import Miso hiding (scrollIntoView, inline)
import Miso.Native.FFI
-----------------------------------------------------------------------------
data ScrollTo
  = ScrollTo
  { offset :: Double
  , index :: Double
  , smooth :: Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal ScrollTo where
  toJSVal ScrollTo {..} = do
    object <- create
    set "offset" offset object
    set "index" index object
    set "smooth" smooth object
    toJSVal object 
-----------------------------------------------------------------------------
defaultScrollTo :: ScrollTo
defaultScrollTo = ScrollTo 0 1 True
-----------------------------------------------------------------------------
scrollTo
  :: MisoString
  -> ScrollTo
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
scrollTo = invokeExec "scrollTo"
-----------------------------------------------------------------------------
data AutoScroll
  = AutoScroll
  { rate :: Double
  , start :: Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal AutoScroll where
  toJSVal AutoScroll {..} = do
    object <- create
    set "rate" rate object
    set "start" start object
    toJSVal object 
-----------------------------------------------------------------------------
defaultAutoScroll :: AutoScroll
defaultAutoScroll = AutoScroll 120 True
-----------------------------------------------------------------------------
autoScroll
  :: MisoString
  -> AutoScroll
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
autoScroll = invokeExec "autoScroll"
-----------------------------------------------------------------------------
data ScrollIntoView
  = ScrollIntoView
  { block :: MisoString
    -- ^ Vertical alignment options: "start" aligns top | "center" centers | "end" aligns bottom
  , inline :: MisoString
    -- ^ Horizontal alignment options: "start" aligns left | "center" centers | "end" aligns right
  , behavior :: MisoString
    -- ^ "smooth" | "none" whether to animate scrolling
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal ScrollIntoView where
  toJSVal ScrollIntoView {..} = do
    object <- create
    set "block" block object
    set "inline" inline object
    set "behavior" behavior object
    scrollIntoViewOptions <- create
    set "scrollIntoViewOptions" object scrollIntoViewOptions
    toJSVal scrollIntoViewOptions
-----------------------------------------------------------------------------
defaultScrollIntoView :: ScrollIntoView
defaultScrollIntoView
  = ScrollIntoView
  { block = "center"
  , inline = "start"
  , behavior = "smooth"
  }
-----------------------------------------------------------------------------
scrollIntoView
  :: MisoString
  -> ScrollIntoView
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
scrollIntoView = invokeExec "scrollIntoView"
-----------------------------------------------------------------------------
newtype ScrollBy
  = ScrollBy
  { scrollByOffset :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal ScrollBy where
  toJSVal ScrollBy {..} = do
    object <- create
    set "offset" scrollByOffset object
    toJSVal object
-----------------------------------------------------------------------------
defaultScrollBy :: ScrollBy
defaultScrollBy = ScrollBy
  { scrollByOffset = 0
  }
-----------------------------------------------------------------------------
scrollBy
  :: MisoString
  -> ScrollBy
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
scrollBy = invokeExec "scrollBy"
-----------------------------------------------------------------------------
-- | Result of calling 'getScrollInfo'
data ScrollInfo
  = ScrollInfo
  { scrollRange :: Double
    -- ^ Total scrollable range along the orientation, in PX
  , scrollX :: Double
    -- ^ Content offset on the X-axis, in PX
  , scrollY :: Double
    -- ^ Content offset on the Y-axis, in PX
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal ScrollInfo where
  fromJSVal = \o -> do
    let readProp = \name ->
          fromJSValUnchecked =<< o ! (name :: MisoString)
    scrollRange <- readProp "scrollRange"
    scrollX     <- readProp "scrollX"
    scrollY     <- readProp "scrollY"
    pure $ Just ScrollInfo {..}
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#getscrollinfo
--
-- Retrieves the current scroll information of the \<scroll-view\>.
--
-- @
--
-- data Action = GetInfo | InfoReceived ScrollInfo | GotError MisoString
--
-- update :: Action -> Effect props model Action
-- update = \\case
--   GetInfo -> getScrollInfo "#box" InfoReceived GotError
--   InfoReceived ScrollInfo {..} -> io_ (consoleLog "got scroll info")
--   GotError errMsg -> io_ (consoleLog errMsg)
--
-- @
--
getScrollInfo
  :: MisoString
  -> (ScrollInfo -> action)
  -> (MisoString -> action)
  -> Effect context props model action
getScrollInfo selector = invokeExec "getScrollInfo" selector ()
-----------------------------------------------------------------------------
