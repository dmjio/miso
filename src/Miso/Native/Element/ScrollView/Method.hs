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
--
-- @since 1.13.0.0
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
-- | Parameters for @scrollTo@: the target @index@, an extra @offset@ to continue past it,
-- and whether the movement is animated.
--
-- @since 1.13.0.0
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
-- | A t'ScrollTo' with sensible defaults (first index, no offset, animated).
--
-- Override only the fields you need.
--
-- @since 1.13.0.0
defaultScrollTo :: ScrollTo
defaultScrollTo = ScrollTo 0 1 True
-----------------------------------------------------------------------------
-- | Invokes the Lynx @scrollTo@ method on a @<scroll-view>@ element.
--
-- Takes a selector, a t'ScrollTo' of parameters, a success continuation and
-- an error continuation.
--
-- @since 1.13.0.0
scrollTo
  :: MisoString
  -> ScrollTo
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
scrollTo = invokeExec "scrollTo"
-----------------------------------------------------------------------------
-- | Parameters for 'autoScroll': the scroll @rate@, whether to @start@ or stop, and
-- whether to stop automatically at the end.
--
-- @since 1.13.0.0
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
-- | A t'AutoScroll' with sensible defaults (stopped).
--
-- Override only the fields you need.
--
-- @since 1.13.0.0
defaultAutoScroll :: AutoScroll
defaultAutoScroll = AutoScroll 120 False
-----------------------------------------------------------------------------
-- | Invokes the Lynx @autoScroll@ method on a @<scroll-view>@ element.
--
-- Takes a selector, a t'AutoScroll' of parameters, a success continuation and
-- an error continuation.
--
-- @since 1.13.0.0
autoScroll
  :: MisoString
  -> AutoScroll
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
autoScroll = invokeExec "autoScroll"
-----------------------------------------------------------------------------
-- | Parameters for 'scrollIntoView': where the element should come to rest within the
-- viewport.
--
-- @since 1.13.0.0
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
-- | A t'ScrollIntoView' with sensible defaults (nearest alignment).
--
-- Override only the fields you need.
--
-- @since 1.13.0.0
defaultScrollIntoView :: ScrollIntoView
defaultScrollIntoView
  = ScrollIntoView
  { block = "center"
  , inline = "start"
  , behavior = "smooth"
  }
-----------------------------------------------------------------------------
-- | Invokes the Lynx @scrollIntoView@ method on a @<scroll-view>@ element.
--
-- Takes a selector, a t'ScrollIntoView' of parameters, a success continuation and
-- an error continuation.
--
-- @since 1.13.0.0
scrollIntoView
  :: MisoString
  -> ScrollIntoView
  -> (JSVal -> action)
  -> (MisoString -> action)
  -> Effect context props model action
scrollIntoView = invokeExec "scrollIntoView"
-----------------------------------------------------------------------------
-- | Parameters for 'scrollBy': the distance to scroll, relative to the current position.
--
-- @since 1.13.0.0
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
-- | A t'ScrollBy' with sensible defaults (zero offset).
--
-- Override only the fields you need.
--
-- @since 1.13.0.0
defaultScrollBy :: ScrollBy
defaultScrollBy = ScrollBy
  { scrollByOffset = 0
  }
-----------------------------------------------------------------------------
-- | Invokes the Lynx @scrollBy@ method on a @<scroll-view>@ element.
--
-- Takes a selector, a t'ScrollBy' of parameters, a success continuation and
-- an error continuation.
--
-- @since 1.13.0.0
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
