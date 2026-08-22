-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- registered for a t'Miso.Event.Types.MTS' event (see
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
--
-- @since 1.13.0.0
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
    -- *** Platform info (main thread only)
  , SystemInfo(..)
  , getSystemInfo
    -- *** Main-thread-local mutable state
  , MainThreadRef
  , mainThreadRef
  , readMainThreadRef
  , writeMainThreadRef
  , modifyMainThreadRef
  , modifyMainThreadRef_
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void, forM_)
import           Control.Monad.State (State, execState)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import           System.IO.Unsafe (unsafePerformIO)
-----------------------------------------------------------------------------
import           Miso.CSS (transforms, TransformFn)
import           Miso.DSL
  ( jsg, jsg0, jsg1, jsg2, jsg3, (!), isUndefined, FromJSVal(..)
  , requestAnimationFrame, syncCallback1, freeFunction, Function(..), jsNull )
import           GHC.Generics (Generic)
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
-- | Drive @step@ once per animation frame until it returns @False@, then release
-- the underlying callback. @step@ receives the frame timestamp in milliseconds.
--
-- This is the vsync-coalesced loop primitive for main-thread, scroll-linked
-- animation: read the latest gesture state, imperatively paint at most once per
-- frame (via 'setStyleProperty' \/ 'setStylePropertyTransform'), and stop by
-- returning @False@ when the gesture ends.
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
-- | Lynx's @lynx.SystemInfo@: device pixel geometry and platform metadata. The
-- field names match the Lynx @SystemInfo@ object, so it decodes directly. Fields
-- that Lynx omits on some realms are 'Maybe' — notably 'runtimeType', which is
-- unavailable in the lepus (main-thread) runtime.
data SystemInfo = SystemInfo
  { pixelWidth     :: Double
    -- ^ Physical pixel width of the device.
  , pixelHeight    :: Double
    -- ^ Physical pixel height of the device.
  , pixelRatio     :: Double
    -- ^ Physical pixel ratio (device pixels per logical pixel).
  , osVersion      :: MisoString
    -- ^ Operating-system version.
  , platform       :: MisoString
    -- ^ Device platform, e.g. @\"Android\"@, @\"iOS\"@, @\"macOS\"@.
  , lynxSdkVersion :: Maybe MisoString
    -- ^ Lynx SDK version (deprecated upstream; may be absent).
  , engineVersion  :: Maybe MisoString
    -- ^ Lynx Engine version (absent on older engines).
  , runtimeType    :: Maybe MisoString
    -- ^ JS engine (@\"v8\"@ \/ @\"jsc\"@ \/ @\"quickjs\"@); not available in lepus.
  , theme          :: Maybe Value
    -- ^ Opaque theme object, when present.
  } deriving (Show, Eq, Generic)

instance FromJSVal SystemInfo

-- | Read Lynx's @lynx.SystemInfo@, decoded into t'SystemInfo'. This global is
-- main-thread-only: present on the MTS realm and absent on the BTS realm, so
-- this returns 'Just' on the main thread and 'Nothing' on the background thread.
-- The @undefined@ guard makes the background-thread read a safe 'Nothing' rather
-- than a throw; a decode failure (e.g. a required field missing) is also
-- 'Nothing'.
getSystemInfo :: IO (Maybe SystemInfo)
getSystemInfo = do
  si <- jsg "lynx" >>= (! "SystemInfo")
  u  <- isUndefined si
  if u then pure Nothing else fromJSVal si
-----------------------------------------------------------------------------
-- | A thin wrapper over 'IORef' for state that lives __only__ on the main
-- thread and must never reach the background thread's shared @model@ (which the
-- BTS solely owns — see "Miso.Runtime"). Use it for transient, main-thread-local
-- gesture\/animation state: the current drag offset, a fling velocity, whether a
-- follow loop is active, etc.
--
-- Reads and writes are ordinary 'IORef' operations, safe here because the MTS is
-- single-threaded; no atomics are needed.
newtype MainThreadRef a = MainThreadRef (IORef a)
-----------------------------------------------------------------------------
-- | Create a top-level t'MainThreadRef' with an initial value.
--
-- This uses 'unsafePerformIO' to allocate the underlying 'IORef' as a CAF, so
-- the ref is shared across all uses of the binding. __You must give every
-- top-level t'MainThreadRef' binding a @{-\# NOINLINE \#-}@ pragma__ — otherwise
-- GHC may inline the CAF and allocate a fresh, independent 'IORef' at each use
-- site, silently splitting your state into multiple copies.
--
-- @
-- dragRef :: t'MainThreadRef' Double
-- dragRef = 'mainThreadRef' 0
-- {-\# NOINLINE dragRef \#-}
-- @
mainThreadRef :: a -> MainThreadRef a
mainThreadRef x = MainThreadRef (unsafePerformIO (newIORef x))
{-# NOINLINE mainThreadRef #-}
-----------------------------------------------------------------------------
-- | Read the current value of a t'MainThreadRef'.
readMainThreadRef :: MainThreadRef a -> IO a
readMainThreadRef (MainThreadRef ref) = readIORef ref
-----------------------------------------------------------------------------
-- | Overwrite the value of a t'MainThreadRef'.
writeMainThreadRef :: MainThreadRef a -> a -> IO ()
writeMainThreadRef (MainThreadRef ref) = writeIORef ref
-----------------------------------------------------------------------------
-- | Strictly modify the value of a t'MainThreadRef'.
modifyMainThreadRef :: MainThreadRef a -> (a -> a) -> IO ()
modifyMainThreadRef (MainThreadRef ref) = modifyIORef' ref
-----------------------------------------------------------------------------
-- | Strictly modify a t'MainThreadRef' with a @'State' a ()@ computation, letting
-- you drive the update with the "Miso.Lens" operators (@.=@, @%=@, @+=@, …).
--
-- @
-- modifyMainThreadRef_ dragRef $ do
--   offset @.=@ newX
--   active @.=@ True
-- @
modifyMainThreadRef_ :: MainThreadRef a -> State a () -> IO ()
modifyMainThreadRef_ ref go = modifyMainThreadRef ref (execState go)
-----------------------------------------------------------------------------
