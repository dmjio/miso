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
--
-- @since 1.13.0.0
----------------------------------------------------------------------------
module Miso.Native.Element.ScrollView.Event
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
  , onScrollEnd
  , onScrollEndWith
  , onScrollEndMain
  , onScrollEndMainWith
  , onContentSizeChanged
  , onContentSizeChangedWith
  , onContentSizeChangedMain
  , onContentSizeChangedMainWith
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
import           Miso.Types (Attribute, EventHandler, DOMRef)
import           Miso.Event
import           Miso.JSON (withObject, (.:?), (.!=))
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | The 'Events' map for the Lynx @<scrollview>@ element.
--
-- Combine with other element maps using @<>@ and pass the result to
-- 'Miso.Native.native', so the delegator listens for these events.
--
-- @since 1.13.0.0
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
-- | t'Decoder' producing a t'ScrollEvent' from the raw Lynx event payload.
--
-- Pass it to 'Miso.Event.on' \/ 'Miso.Event.onMain' when writing a handler by
-- hand; the @on*@ helpers in this module already use it.
--
-- @since 1.13.0.0
scrollDecoder :: Decoder ScrollEvent
scrollDecoder = ["detail"] `at` do
  withObject "ScrollEvent" $ \o ->
    ScrollEvent
      -- @type@ is present on native scroll events but omitted by the web
      -- (LynxDevTool) preview; keep it optional so decoding succeeds in both.
      <$> o .:? "type" .!= ""
      <*> o .:? "deltaX" .!= 0
      <*> o .:? "deltaY" .!= 0
      <*> o .:? "scrollLeft" .!= 0
      <*> o .:? "scrollTop" .!= 0
      <*> o .:? "scrollHeight" .!= 0
      <*> o .:? "scrollWidth" .!= 0
-----------------------------------------------------------------------------
-- | Payload of a @<scroll-view>@ scroll event: the delta since the last
-- event, the current offsets, and the full scrollable extent.
-- 
-- @since 1.13.0.0
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
onScrollEnd :: (ScrollEvent -> action) -> Attribute model action
onScrollEnd action = on "scrollend" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollEnd', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleScroll ScrollEvent
--
-- view_ [ event (static (onScrollEndMain HandleScroll)) ] [ "some view" ]
-- @
--
onScrollEndMain :: (ScrollEvent -> action) -> EventHandler model action
onScrollEndMain action = onMain "scrollend" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onScrollEndMain', but the handler also receives read-only access to
-- the @model@ and the target element's 'DOMRef' (for imperative MTS mutation).
--
-- @
-- data Action = HandleScroll ScrollEvent Model DOMRef
--
-- view_ [ event (static (onScrollEndMainWith HandleScroll)) ] [ "some view" ]
-- @
--
onScrollEndMainWith :: (ScrollEvent -> model -> DOMRef -> action) -> EventHandler model action
onScrollEndMainWith action = onMain "scrollend" scrollDecoder action
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
onContentSizeChanged :: (ScrollEvent -> action) -> Attribute model action
onContentSizeChanged action = on "contentsizechanged" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onContentSizeChanged', but dispatched on the Lynx __main thread__ (@MTS@).
--
-- Runs imperatively on the MTS (no VDOM diff). Meant to be used with
-- @-XStaticPointers@.
--
-- @
-- data Action = HandleContentSizeChanged ScrollEvent
--
-- view_ [ event (static (onContentSizeChangedMain HandleContentSizeChanged)) ] [ "some view" ]
-- @
--
onContentSizeChangedMain :: (ScrollEvent -> action) -> EventHandler model action
onContentSizeChangedMain action = onMain "contentsizechanged" scrollDecoder (\x _ _ -> action x)
-----------------------------------------------------------------------------
-- | Like 'onContentSizeChangedMain', but the handler also receives read-only
-- access to the @model@ and the target element's 'DOMRef' (for imperative MTS
-- mutation).
--
-- @
-- data Action = HandleContentSizeChanged ScrollEvent Model DOMRef
--
-- view_ [ event (static (onContentSizeChangedMainWith HandleContentSizeChanged)) ] [ "some view" ]
-- @
--
onContentSizeChangedMainWith :: (ScrollEvent -> model -> DOMRef -> action) -> EventHandler model action
onContentSizeChangedMainWith action = onMain "contentsizechanged" scrollDecoder action
-----------------------------------------------------------------------------
-- | Like 'onScroll', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onScrollWith :: (ScrollEvent -> DOMRef -> action) -> Attribute model action
onScrollWith action = on "scroll" scrollDecoder $ \se _ domRef -> action se domRef
-----------------------------------------------------------------------------
-- | Like 'onScrollToUpper', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onScrollToUpperWith :: (ScrollEvent -> DOMRef -> action) -> Attribute model action
onScrollToUpperWith action = on "scrolltoupper" scrollDecoder $ \se _ domRef -> action se domRef
-----------------------------------------------------------------------------
-- | Like 'onScrollToLower', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onScrollToLowerWith :: (ScrollEvent -> DOMRef -> action) -> Attribute model action
onScrollToLowerWith action = on "scrolltolower" scrollDecoder $ \se _ domRef -> action se domRef
-----------------------------------------------------------------------------
-- | Like 'onScrollEnd', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onScrollEndWith :: (ScrollEvent -> DOMRef -> action) -> Attribute model action
onScrollEndWith action = on "scrollend" scrollDecoder $ \se _ domRef -> action se domRef
-----------------------------------------------------------------------------
-- | Like 'onContentSizeChanged', but the handler also receives the target element's 'DOMRef'.
-- Use for main-thread (@MTS@) handlers that imperatively mutate the element.
onContentSizeChangedWith :: (ScrollEvent -> DOMRef -> action) -> Attribute model action
onContentSizeChangedWith action = on "contentsizechanged" scrollDecoder $ \se _ domRef -> action se domRef
-----------------------------------------------------------------------------
