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
  , onScrollWith
  , onScrollToUpper
  , onScrollToUpperWith
  , onScrollToLower
  , onScrollToLowerWith
  , onScrollEnd
  , onScrollEndWith
  , onContentSizeChanged
  , onContentSizeChangedWith
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
import           Miso.Types (EventHandler, DOMRef)
import           Miso.Event
import           Miso.JSON (withObject, (.:), (.:?), (.!=))
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
scrollViewEvents :: Events
scrollViewEvents
  = backgroundEvents
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
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = scrollView_ [ onScroll HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScroll :: (ScrollEvent -> action) -> EventHandler action
onScroll action = on "scroll" scrollDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#scrolltoupper
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = scrollView_ [ onScrollToUpper HnadleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScrollToUpper :: (ScrollEvent -> action) -> EventHandler action
onScrollToUpper action = on "scrolltoupper" scrollDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#scrolltolower
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = scrollView_ [ onScrollToLower HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScrollToLower :: (ScrollEvent -> action) -> EventHandler action
onScrollToLower action = on "scrolltolower" scrollDecoder (\x _ -> action x)
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/scroll-view.html#scrollend
--
-- @
--
-- data Action = HandleScroll ScrollEvent
--
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = scrollView_ [ onScrollToLower HandleScroll ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleScroll ScrollEvent {..}) =
--   io_ (consoleLog "handled scroll event")
--
-- @
--
onScrollEnd :: (ScrollEvent -> action) -> EventHandler action
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
-- view :: context -> props -> Model -> View context Action
-- view _ _ model = scrollView_ [ onContentSizeChanged HandleContentSizeChanged ] [ ]
--
-- update :: Action -> Effect props Model Action
-- update (HandleContentSizeChanged ScrollEvent {..}) =
--   io_ (consoleLog "handled content size changed event")
--
-- @
--
onContentSizeChanged :: (ScrollEvent -> action) -> EventHandler action
onContentSizeChanged action = on "contentsizechanged" scrollDecoder (\x _ -> action x)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Like 'onScroll', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onScrollWith :: (ScrollEvent -> DOMRef -> action) -> EventHandler action
onScrollWith action = on "scroll" scrollDecoder action
-----------------------------------------------------------------------------
-- | Like 'onScrollToUpper', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onScrollToUpperWith :: (ScrollEvent -> DOMRef -> action) -> EventHandler action
onScrollToUpperWith action = on "scrolltoupper" scrollDecoder action
-----------------------------------------------------------------------------
-- | Like 'onScrollToLower', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onScrollToLowerWith :: (ScrollEvent -> DOMRef -> action) -> EventHandler action
onScrollToLowerWith action = on "scrolltolower" scrollDecoder action
-----------------------------------------------------------------------------
-- | Like 'onScrollEnd', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onScrollEndWith :: (ScrollEvent -> DOMRef -> action) -> EventHandler action
onScrollEndWith action = on "scrollend" scrollDecoder action
-----------------------------------------------------------------------------
-- | Like 'onContentSizeChanged', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread ('MTS') handlers that imperatively mutate the element.
onContentSizeChangedWith :: (ScrollEvent -> DOMRef -> action) -> EventHandler action
onContentSizeChangedWith action = on "contentsizechanged" scrollDecoder action
-----------------------------------------------------------------------------
