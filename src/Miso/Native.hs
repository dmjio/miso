-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = miso native 📱
--
-- "Miso.Native" targets __native mobile devices__ by driving the
-- [Lynx](https://lynxjs.org) runtime instead of the browser DOM. The same
-- [MVU](https://elm-lang.org) programming model, 'Component' API, event
-- delegation and virtual-DOM diffing you use on the web ("Miso") carry over
-- unchanged — only the element vocabulary differs ('Miso.Native.Element.view_',
-- 'Miso.Native.Element.text_', … instead of 'Miso.Html.Element.div_' \/
-- 'Miso.Html.Element.span_') and rendering is performed by Lynx's
-- [element PAPI](https://lynxjs.org/api/engine/element-api) rather than by
-- mutating a browser DOM.
--
-- This module is the native analog of the 'Miso.miso' \/ 'Miso.startApp'
-- entrypoints: 'native' (and 'nativeWithContext') boot a root 'Component' onto
-- the Lynx runtime.
--
-- == Enabling native
--
-- The native backend is gated behind the @native@ /cabal flag/. It must be
-- enabled to bring "Miso.Native" and the @Miso.Native.*@ element \/ event \/ FFI
-- modules into scope (build with @-fnative@). Web \/ WASM builds are unaffected —
-- all cross-thread machinery lives behind the @NATIVE@ CPP guard.
--
-- = The dual-thread architecture
--
-- Lynx runs your application across __two threads__, and miso maps onto both:
--
-- * __BTS__ — the /background thread/ (\"background thread script\"). This is
--   where your application /logic/ lives. Everything runs here __by default__:
--   the 'Miso.Types.update' function, event handling, 'Effect' scheduling and
--   /all/ virtual-DOM diffing.
--
-- * __MTS__ — the /main thread/ (\"main thread script\"). This thread owns the
--   actual element tree and /rendering/. It is where the pixels land. It is also
--   available as a low-latency escape hatch for performance-critical event
--   handling (see [Main-thread events](#g:mainthread) below).
--
-- The __same Haskell bundle runs on both threads__; the native runtime
-- (@ts\/miso-native.ts@) selects the BTS or MTS drawing context per-thread from a
-- global flag, so there is no renderer to register — 'native' starts the app
-- directly.
--
-- The guiding principle: __everything originates on the BTS__. The MTS is a
-- rendering surface that the BTS drives across the thread boundary.
--
-- == Knowing which thread you are on
--
-- Lynx builds the bundle with [rspeedy](https://lynxjs.org) — its Rust-based
-- tooling — which compiles the sources /twice/, once per thread, inlining a
-- __compile-time constant__ (@__BACKGROUND__@) that distinguishes the two. That
-- constant surfaces in Haskell as three top-level 'Bool's in "Miso.Runtime":
--
-- * @mts@ — 'True' when this execution context is the Lynx /main/ thread.
-- * @bts@ — 'True' when this context is the Lynx /background/ thread.
-- * @web@ — 'True' for a plain web \/ WASM build (neither Lynx thread).
--
-- Exactly one is 'True', and the value is invariant for the lifetime of a JS
-- context, so the runtime computes it once and caches it. Runtime code branches
-- on @mts@ \/ @bts@ to decide where work runs (e.g. the scheduler suppresses the
-- paint step on the MTS, which only hydrates props \/ context read-only).
--
-- == What crosses the thread boundary, and how
--
-- Because logic (BTS) and rendering (MTS) live on different threads, miso
-- synchronizes them by shipping messages across the boundary. This is largely
-- invisible, but understanding it explains the API constraints below.
--
-- * __Initial draw__ — The very first 'Draw' happens __on the MTS itself__, and
--   it does __not__ rely on the BTS diffing a tree and transferring patches
--   across the boundary. The root 'Component' is booted from a 'StaticPtr' (via
--   'native' \/ 'nativeWithContext'), so the MTS reconstructs it from the
--   pointer's 'GHC.StaticPtr.StaticKey' alone and renders the first frame
--   locally (Lynx's instant first frame). Only /after/ this initial draw does the
--   cross-thread patch protocol take over: __every subsequent diff runs on the
--   BTS and ships patches to the MTS__ to apply.
--
-- * __Subsequent component mounts__ — When the BTS 'view' mounts a child
--   'Component', that mount is synchronized to the MTS __asynchronously__ using
--   /static mounting/: the child is wrapped in a @static@ pointer
--   (@-XStaticPointers@) so only its 'GHC.StaticPtr.StaticKey' — not a closure —
--   needs to cross the boundary. The MTS dereferences the key to rebuild the
--   component locally. See 'Miso.Types.vcomp' \/ 'Miso.Types.mountStatic_'.
--
-- * __Props \/ context diffing__ — Likewise, @props@ passed to a child and the
--   global @context@ are diffed on the BTS and the resulting values are
--   JSON-serialized across to the MTS (hence the @ToJSON@ \/ @FromJSON@
--   constraints on native mounting combinators). The @static@ pointer carries the
--   /constructor/; the runtime @props@ \/ @context@ payload is shipped separately,
--   so it may depend on the parent @model@.
--
-- * __Events__ — Events raised on the MTS are, by default, forwarded to the BTS
--   where 'update' runs (see below). Cross-thread handlers are carried as an
--   'Miso.Types.EventHandler', embedded with 'Miso.Types.event' @. static (…)@ so
--   the peer thread can rebuild the handler from its 'GHC.StaticPtr.StaticKey'.
--
-- = Static mounting
--
-- Because component constructors, event handlers and effects may need to be
-- reconstructed on the /other/ thread, native miso threads them across the
-- boundary as @static@ pointers rather than closures. This requires the
-- @-XStaticPointers@ language extension.
--
-- The root component is mounted with 'Miso.Types.mountStatic_' wrapped in
-- @static@:
--
-- @
-- {-# LANGUAGE StaticPointers #-}
-- -----------------------------------------------------------------------------
-- module Main where
-- -----------------------------------------------------------------------------
-- import "Miso"
-- import "Miso.Native"
-- -----------------------------------------------------------------------------
-- main :: 'IO' ()
-- main = 'native' 'nativeEvents' (static ('Miso.Types.mountStatic_' app))
-- @
--
-- Child components are embedded in a 'view' the same way, with 'Miso.Types.vcomp':
--
-- @
-- view _ _ _ = view_ [] [ 'Miso.Types.vcomp' () (static ('Miso.Types.mountStatic_' childComponent)) ]
-- @
--
-- __Static-pointer limitation.__ A @static@ form may only close over
-- /top-level, closed/ bindings — it cannot capture local variables. This is why
-- component constructors and main-thread handlers are supplied as references to
-- top-level definitions, with any runtime data (props, decoded event payloads)
-- shipped separately as serialized values rather than captured in a closure.
--
-- = Effects: choosing a thread
--
-- Since 'update' runs on the BTS, the 'IO' it schedules runs on the BTS too. Two
-- combinators let an 'Effect' pin its 'IO' to a specific thread regardless of
-- where the action was dispatched:
--
-- * 'Miso.Effect.runOnBG' — run the 'IO' on the __background__ thread (BTS).
-- * 'Miso.Effect.runOnMain' — run the 'IO' on the __main__ thread (MTS).
--
-- A cross-thread schedule does not execute its 'IO' locally: it forwards the
-- /originating action/ across the boundary; the peer sinks it, re-runs 'update'
-- there, and the same effect — now same-thread — runs its 'IO' locally. So
-- 'Miso.Effect.runOnMain''s 'IO' always ends up on the MTS and
-- 'Miso.Effect.runOnBG''s on the BTS, independent of the dispatching thread.
--
-- = Main-thread events #mainthread#
--
-- __Thread affinity is per-handler, not per-event-name.__ Any given event can be
-- handled on /either/ thread; the choice is made at each handler, so the same
-- event (say @tap@) may run on the BTS for one element and the MTS for another.
-- The __default is the BTS__ — a plain 'Miso.Native.Element.View.Event.onTap'
-- handler runs on the background thread. Opting a handler into the MTS is
-- explicit (the @*Main@ variants below); nothing runs on the main thread unless
-- you ask for it.
--
-- By default an event handler runs on the __BTS__: the event is forwarded from
-- the MTS, 'update' runs on the BTS, the model changes, and the resulting diff is
-- shipped back to the MTS to paint. That round-trip is fine for most
-- interactions but adds latency for gesture- and scroll-linked animation.
--
-- For those cases, handlers have __@*Main@-suffixed variants__ (e.g.
-- 'Miso.Native.Element.View.Event.onTapMain',
-- 'Miso.Native.Element.View.Event.onTouchMoveMain') that run __synchronously on
-- the MTS__ — no VDOM diff, no patches, no BTS round-trip. Such a handler is
-- /imperative/: it mutates the target element directly through the helpers in
-- "Miso.Native.MainThread" (e.g. 'Miso.Native.MainThread.setStyleProperty'). The
-- @*MainWith@ variants additionally hand the handler the current @model@ and the
-- target 'Miso.Types.DOMRef' (@\\event model domRef -> action@).
--
-- Because a main-thread handler must be reconstructed on the MTS, it is an
-- 'Miso.Types.EventHandler' embedded with 'Miso.Types.event' @. static@ — so
-- __main-thread event handlers require @-XStaticPointers@__ (the @static@ keyword
-- is how the handler crosses to the MTS by 'GHC.StaticPtr.StaticKey'):
--
-- @
-- {-# LANGUAGE StaticPointers #-}
--
-- view _ _ _ =
--   view_ [ 'Miso.Types.event' (static ('Miso.Native.Element.View.Event.onTapMain' HandleTap)) ] []
-- @
--
-- The same @static@ capture limitation applies: an @onTapMain@ handler refers to
-- a top-level action \/ function; runtime data reaches the handler via the
-- decoded event payload, not a captured closure.
--
-- __Reaching the @model@ (and why it is passed, not captured).__ A static
-- main-thread handler /cannot/ close over the @model@, @props@ or @context@ from
-- the enclosing 'view' — those are local bindings, which @static@ forbids. So
-- rather than capture them, the @*MainWith@ variants __pass the @model@ as an
-- argument__ to the handler, giving imperative MTS code the state it needs to
-- integrate without a BTS round-trip. Note this is the __main-thread's own copy__
-- of the model: it is populated on the MTS __eventually consistently__ from the
-- BTS (the authoritative model still lives on the background thread), so a
-- handler may observe a value slightly behind the latest BTS state.
--
-- __Ownership caveat.__ A property you drive imperatively from the MTS must not
-- /also/ be written declaratively by the BTS @view@ for the same element: both
-- threads write the shared element tree through the same PAPI with no
-- arbitration, so one will clobber the other. Keep a single owner per
-- @(element, property)@ — typically compositor properties like @transform@ \/
-- @opacity@ that the @view@ leaves alone.
--
-- = A minimal native component
--
-- @
-- {-# LANGUAGE StaticPointers #-}
-- -----------------------------------------------------------------------------
-- import "Miso"
-- import "Miso.Native"
-- -----------------------------------------------------------------------------
-- view :: context -> props -> Model -> View context Action
-- view _ _ m =
--   vfrag_
--   [ view_ [ onTap Increment ] [ text_ [] [ \"+\" ] ]
--   , text_ [] [ text $ ms (show m) ]
--   , view_ [ onTap Decrement ] [ text_ [] [ \"-\" ] ]
--   ]
-- @
--
-- More information on how to use miso is available on GitHub
--
-- <http://github.com/dmjio/miso>
--
----------------------------------------------------------------------------
module Miso.Native
   ( -- * Entrypoint
     native
   , nativeWithContext
     -- * Element
   , module Miso.Native.Element
     -- * FFI
   , module Miso.Native.FFI
     -- * Event
   , module Miso.Native.Event
   ) where
-----------------------------------------------------------------------------
import Miso.Runtime (initComponent)
import Miso.Types (Events, SomeStaticComponent(..), SomeComponent(..), Hydrate(..))
import Miso.JSON (ToJSON, FromJSON)
-----------------------------------------------------------------------------
import Miso.Native.Element
import Miso.Native.FFI
import Miso.Native.Event
-----------------------------------------------------------------------------
import GHC.StaticPtr (StaticPtr, deRefStaticPtr, staticKey)
-----------------------------------------------------------------------------
-- | The native drawing context is already selected per-thread by the runtime
-- (@ts\/miso-native.ts@ picks @bts@ or @mts@ from @__BACKGROUND__@), so there
-- is no renderer to register — we start the app directly.
--
-- @
-- {-# LANGUAGE StaticPointers #-}
--
-- import Miso
-- import Miso.Native
--
-- main :: IO ()
-- main = native nativeEvents (static (mountStatic_ app))
-- @
--
native
  :: Events
  -> StaticPtr (SomeStaticComponent () ())
  -> IO ()
native events ptr =
  case deRefStaticPtr ptr of
    SomeStaticComponent mk -> case mk () of
      SomeComponent key props_ vcomp_ ->
        initComponent events Draw False () vcomp_
          key props_ (Just (staticKey ptr))
-----------------------------------------------------------------------------
-- | Like 'native', but the user can specify a global 'context' object.
--
-- @
-- {-# LANGUAGE StaticPointers #-}
--
-- import Miso
-- import Miso.Native
--
-- main :: IO ()
-- main = nativeWithContext nativeEvents () (static (mountStatic_ app))
-- @
--
nativeWithContext
  :: (ToJSON context, FromJSON context, Eq context)
  => Events
  -> context
  -> StaticPtr (SomeStaticComponent () context)
  -> IO ()
nativeWithContext events context ptr =
  case deRefStaticPtr ptr of
    SomeStaticComponent mk -> case mk () of
      SomeComponent key props_ vcomp_ ->
        initComponent events Draw False context vcomp_
          key props_ (Just (staticKey ptr))
-----------------------------------------------------------------------------
