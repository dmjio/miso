-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.ScrollView.Event
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.ScrollView.Event
  ( -- *** Event
    onScroll
  , onScrollToUpper
  , onScrollToLower
  , onScrollEnd
  , onContentSizeChanged
  -- *** Decoders
  , scrollDecoder
  -- *** Types
  , ScrollEvent (..)
  -- *** Event Map
  , scrollViewEvents
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map as M
-----------------------------------------------------------------------------
import           Miso.Types (Attribute)
import           Miso.Event
import           Miso.JSON (withObject, (.:), (.:?), (.!=))
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
scrollViewEvents :: Events
scrollViewEvents
  = M.fromList
  [ ("scroll", BUBBLE)
  , ("scrolltoupper", BUBBLE)
  , ("scrolltolower", BUBBLE)
  , ("scrollend", BUBBLE)
  , ("contentsizechanged", BUBBLE)
  ]
-----------------------------------------------------------------------------
scrollDecoder :: Decoder ScrollEvent
scrollDecoder = ["detail"] `at` do
  withObject "ScrollEvent" $ \o ->
    ScrollEvent
      <$> o .: "type"
      <*> o .:? "deltaX" .!= 0
      <*> o .:? "deltaY" .!= 0
      <*> o .:? "scrollLeft" .!= 0
      <*> o .:? "scrollTop" .!= 0
      <*> o .:? "scrollHeight" .!= 0
      <*> o .:? "scrollWidth" .!= 0
-----------------------------------------------------------------------------
data ScrollEvent
  = ScrollEvent
  { scrollType :: MisoString
  , deltaX, deltaY :: Double
  , scrollLeft, scrollTop, scrollHeight, scrollWidth :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#scroll
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: Model -> View Model Action
-- view model = scrollView_ [ onScroll HandleScroll ] [ ]
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
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#scrolltoupper
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: Model -> View Model Action
-- view model = scrollView_ [ onScrollToUpper HnadleScroll ] [ ]
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
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#scrolltolower
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: Model -> View Model Action
-- view model = scrollView_ [ onScrollToLower HandleScroll ] [ ]
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
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#scrollend
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: Model -> View Model Action
-- view model = scrollView_ [ onScrollToLower HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScrollEnd :: (ScrollEvent -> action) -> Attribute action
onScrollEnd action = on "scrollend" scrollDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#contentsizechanged
--
-- Triggered when the content area comprised of direct child nodes changes in width
-- or height. This event triggers after the \<scroll-view\> content completes layout.
-- If updating \<scroll-view\> child nodes, call updated scrolling methods like
-- `scrollTo` in this event.
--
-- @
--
-- data Action = HandleContentSizeChanged ScrollEvent
--
-- view :: Model -> View Model Action
-- view model = scrollView_ [ onContentSizeChanged HandleContentSizeChanged ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleContentSizeChanged ScrollEvent {..}) =
--   io_ (consoleLog "handled content size changed event")
--
-- @
--
onContentSizeChanged :: (ScrollEvent -> action) -> Attribute action
onContentSizeChanged action = on "contentsizechanged" scrollDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
