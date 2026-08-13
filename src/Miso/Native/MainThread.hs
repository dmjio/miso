-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.MainThread
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Main-thread (MTS) imperative element manipulation
--
-- Helpers for /main-thread events/ on the Lynx dual-thread runtime. A handler
-- registered for a 'Miso.Event.Types.MTS' event (see
-- 'Miso.Event.Types.mainThreadEvents') runs synchronously on the main thread and
-- receives the target 'DOMRef' via a @*With@ combinator
-- (e.g. 'Miso.Native.Element.View.Event.onTapWith'). Such a handler must be
-- __imperative__: it mutates the element directly with the functions below.
--
-- It does __not__ go through the VDOM diff — no re-render, no patches, no
-- background-thread round-trip. This is the low-latency path for gestures and
-- scroll-linked animation.
--
-- @
-- -- move an element with the finger, entirely on the main thread:
-- view _ _ _ = view_ [ onTouchMoveWith Drag ] []
--
-- update (Drag touch domRef) = io_ $
--   setStyleProperty domRef \"transform\"
--     (\"translateY(\" <> ms (touchY touch) <> \"px)\")
-- @
--
-- __Conflict caveat.__ A property you drive imperatively here must /not/ also be
-- set declaratively by the background-thread @view@ for the same element: both
-- threads write the shared element tree through the same PAPI, with no
-- arbitration, so the next background re-render would clobber it (and vice
-- versa). Keep a single owner per @(element, property)@ — typically compositor
-- properties like @transform@ / @opacity@ that the @view@ leaves alone. This is
-- the same discipline Lynx itself requires; it is not enforced.
--
-- These call Lynx element PAPI globals and are only meaningful on the native
-- runtime's main thread.
----------------------------------------------------------------------------
module Miso.Native.MainThread
  ( -- *** Imperative element mutation (main thread only)
    setStyleProperty
  , setStyleProperties
  , setStylePropertyTransform
  , setAttribute
  , getAttribute
  , flushElementTree
    -- *** Element-tree navigation (main thread only)
  , firstElementChild
  , nextElementSibling
  , parentElement
    -- *** Frame-driven animation (main thread only)
  , eachFrame
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void, forM_)
import           Data.IORef (newIORef, readIORef, writeIORef)
-----------------------------------------------------------------------------
import           Miso.CSS (transforms, TransformFn)
import           Miso.DSL
  ( jsg0, jsg1, jsg2, jsg3, fromJSValUnchecked
  , requestAnimationFrame, syncCallback1, freeFunction, Function(..), jsNull )
import           Miso.Effect (DOMRef)
import           Miso.JSON (ToJSON(..), FromJSON(..), Value(Null))
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Lets a target 'DOMRef' ride inside a @*With@ handler's action. Native
-- component actions must be @ToJSON@\/@FromJSON@, but a raw 'DOMRef' (a
-- @JSVal@) has no meaningful serialization — and main-thread actions never
-- cross the thread boundary anyway, so these are __inert placeholders__:
-- 'toJSON' is @Null@ and 'parseJSON' fails. Only import "Miso.Native.MainThread"
-- where you actually dispatch main-thread events.
--
-- ⚠ These are global orphan instances for @JSVal@; do not rely on round-tripping
-- a 'DOMRef' through JSON anywhere.
instance ToJSON DOMRef where
  toJSON _ = Null
instance FromJSON DOMRef where
  parseJSON _ = fail "DOMRef: main-thread-only, never deserialized"
-----------------------------------------------------------------------------
-- | Set a single inline style property on the element, then flush.
--
-- > setStyleProperty domRef "transform" "translateX(20px)"
setStyleProperty :: DOMRef -> MisoString -> MisoString -> IO ()
setStyleProperty node name value = do
  void (jsg3 "__AddInlineStyle" node name value)
  flushElementTree
-----------------------------------------------------------------------------
-- | Set several inline style properties, then flush once.
setStyleProperties :: DOMRef -> [(MisoString, MisoString)] -> IO ()
setStyleProperties node styles = do
  forM_ styles $ \(name, value) ->
    void (jsg3 "__AddInlineStyle" node name value)
  flushElementTree
-----------------------------------------------------------------------------
-- | Set the element's @transform@ from a list of typed t'Miso.CSS.TransformFn's
-- (from "Miso.CSS"), then flush — a typed alternative to writing the
-- @transform@ string by hand.
--
-- > setStylePropertyTransform ref [ CSS.translateX (CSS.px 20) ]
setStylePropertyTransform :: DOMRef -> [TransformFn] -> IO ()
setStylePropertyTransform node fns = setStyleProperties node [ transforms fns ]
-----------------------------------------------------------------------------
-- | Set an attribute on the element, then flush.
setAttribute :: DOMRef -> MisoString -> MisoString -> IO ()
setAttribute node key value = do
  void (jsg3 "__SetAttribute" node key value)
  flushElementTree
-----------------------------------------------------------------------------
-- | Read an attribute's current value from the element.
getAttribute :: DOMRef -> MisoString -> IO MisoString
getAttribute node key =
  fromJSValUnchecked =<< jsg2 "__GetAttributeByName" node key
-----------------------------------------------------------------------------
-- | Commit pending element-tree mutations to the screen. The @set*@ helpers
-- above already flush; call this directly only when batching lower-level calls.
flushElementTree :: IO ()
flushElementTree = void (jsg0 "__FlushElementTree")
-----------------------------------------------------------------------------
-- | First element child of a node (Lynx @__FirstElement@). Lets a main-thread
-- handler reach a /different/ element than the event target by walking the
-- tree — e.g. from a scroll handler's list ref to a sibling scrollbar thumb.
firstElementChild :: DOMRef -> IO DOMRef
firstElementChild = jsg1 "__FirstElement"
-----------------------------------------------------------------------------
-- | Next element sibling of a node (Lynx @__NextElement@).
nextElementSibling :: DOMRef -> IO DOMRef
nextElementSibling = jsg1 "__NextElement"
-----------------------------------------------------------------------------
-- | Parent element of a node (Lynx @__GetParent@).
parentElement :: DOMRef -> IO DOMRef
parentElement = jsg1 "__GetParent"
-----------------------------------------------------------------------------
-- | Drive @step@ once per animation frame until it returns 'False', then release
-- the underlying callback. @step@ receives the frame timestamp in milliseconds.
--
-- This is the vsync-coalesced loop primitive for main-thread, scroll-linked
-- animation: read the latest gesture state, imperatively paint at most once per
-- frame (via 'setStyleProperty' \/ 'setStylePropertyTransform'), and stop by
-- returning 'False' when the gesture ends.
--
-- @
-- startFollow ref = 'eachFrame' $ \\_ts -> do
--   d <- readDrag
--   if not (active d) then pure False else do
--     setStylePropertyTransform ref [ CSS.translateX (CSS.px (offset d)) ]
--     pure True
-- @
eachFrame :: (Double -> IO Bool) -> IO ()
eachFrame step = do
  cbRef <- newIORef jsNull
  let frame tsVal = do
        keep <- step =<< fromJSValUnchecked tsVal
        cb   <- readIORef cbRef
        if keep
          then void (requestAnimationFrame cb)
          else freeFunction (Function cb)
  cb <- syncCallback1 frame
  writeIORef cbRef cb
  void (requestAnimationFrame cb)
-----------------------------------------------------------------------------
