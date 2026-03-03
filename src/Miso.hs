-----------------------------------------------------------------------------
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- miso is a project for building user interfaces in the browser using Haskell.
--
-- It provides a React-like programming experience for a simple
-- Haskell dialect that emphasizes @performance@, @purity@, @simplicity@ and
-- @extensibility@.
--
-- miso supports common areas that arise in web development:
--
-- * __DOM manipulation__: @miso@ uses a virtual DOM diffing algorithm that is
--   responsible for all DOM modifications and component mounting lifecycle events.
--
-- * __Event delegation__: All event listeners are attached to a top-level element
--   (typically @<body>@). When raised, events are routed through the virtual DOM
--   to Haskell event handlers which cause application state changes. Internally @miso@ virtualizes both the 'capture' and 'bubble' phases of the
--   browser when it performs event routing.
--
-- * __Prerendering__: This is an optional feature. Prerendering copies the DOM
--   into the virtual DOM to avoid page redraws on initial page load and to
--   enhance search engine optimization. @miso@ provides its own HTML rendering (@Miso.Html.Render@) to facilitate
--   this on the server and the 'miso' function exists on the client to \"hydrate\"
--   the virtual DOM with the physical DOM.
--
-- * __Isolated components__: A 'Component' is an abstract concept that allows
--   users to create reusable pieces of application functionality. A 'Component'
--   contains user-defined state, logic for updating this state, and a function
--   for creating UI templates from this user-defined state.
--
-- * __Custom renderers__: The underlying DOM-operations are able to be abstracted over
-- to allow a custom rendering engine to be used. This is seen in the [miso-lynx](https://github.com/haskell-miso/miso-lynx) project
-- (which allows miso to target mobile phone devices).
--
-- * __Lifecycle hooks__: As 'Component' are mounted and unmounted, and virtual DOM nodes created and destroyed, it is possible to listen
-- for these lifecycle events and handle them in your application. This is commonly used for 'Component' communication and for third-party integration
-- with JavaScript libraries.
--
-- = The Model-Update-View pattern
--
-- The core type of the project is 'Component'. A 'Component' adheres to the [Elm](https://elm-lang.org)
-- MUV (model-update-view) pattern.
--
-- * __model__: This can be any user-defined type in Haskell. An 'Eq' constraint
--   is required. We recommend using the default derived 'Eq' instance.
--
-- * __update__: @'update' :: action -> 'Effect' parent model action@
--   The 'update' function handles how the 'model' evolves over time in response
--   to events that are raised by the browser. This function takes any 'action',
--   optionally updating the 'model' and introducing 'IO' into the system.
--
-- * __view__: @'view' :: model -> 'View' model action@
--   This is the templating function that is used to construct a new virtual DOM
--   (or HTML if rendering on the server).
--
-- = Your First t'Component'
--
-- To define a 'Component' the 'component' smart constructor can be used.
-- Below is an example of a counter component.
--
-- @
-- -----------------------------------------------------------------------------
-- module Main where
-- -----------------------------------------------------------------------------
-- import Miso
-- import Miso.Lens
-- import qualified Miso.Html.Element as H
-- import qualified Miso.Html.Event as HE
-- import qualified Miso.Html.Property as HP
-- -----------------------------------------------------------------------------
--                        * => The type of the 'parent' Component 'model'
--                        |     * => The type of the current Component's 'model'
--                        |     |    * => The type of the 'action' that updates the 'model'
--                        |     |    |
-- counter :: Component parent Int Action
-- counter = component m u v
--   where
--     m :: Int
--     m = 0
--
--     u :: Action -> Effect parent Int Action
--     u = \\case
--       Add -> this += 1
--       Subtract -> this -= 1
--
--     v :: Int -> View Int Action
--     v x = H.div_
--       [ H.button_ [ HE.onClick Add ] [ "+" ]
--       , text (ms x)
--       , H.button_ [ H.onClick Subtract ] [ "-" ]
--       ]
-- -----------------------------------------------------------------------------
-- main :: IO ()
-- main = startApp defaultEvents counter
-- -----------------------------------------------------------------------------
-- data Action
--   = Add
--   | Subtract
--   deriving (Eq, Show)
-- -----------------------------------------------------------------------------
-- @
--
-- = Running your first t'Component'
--
-- To run the counter 'Component' defined above the 'startApp' or 'miso' functions be used.
--
-- @
-- main :: IO ()
-- main = startApp defaultEvents counter
-- @
--
-- The 'startApp' function is what we recommend using first. It takes a list of events (which are globally delegated) and
-- a 'Component', and begins drawing on the screen and remains responsive to browser events defined in 'defaultEvents'.
--
-- The 'startApp' function assumes that <body> is empty (or contains @<script>@) that don't affect the application, and it will
-- begin drawing the 'Component' 'View' from @<body>@.
--
-- The 'miso' function (and also the 'prerender' function) assume that @<body>@ has already been populated by the results of the 'view' function.
-- Instead of drawing, 'miso' will traverse the DOM and Virtual DOM, copying the DOM references into the Virtual DOM, performing a structural equality check.
-- If the structures do not match, miso will fallback to drawing the page from scratch (clearing the contents of @<body>@ first).
-- This process is done to avoid unnecessary page drawing (to avoid page blinks).
--
-- We recommend using 'startApp' first, and then adding support for 'miso' later as an optimization.
--
-- It is possible to execute an initial action when a t'Component' is first mounted. See the 'mount' (and similarly 'unmount') hooks.
--
-- @
--
-- data Action = Init
--
-- main :: IO ()
-- main = startApp defaultEvents counter { mount = Just Init }
--
-- update :: App model Action
-- update = \\case
--   Init -> io_ (consoleLog "hello world!")
-- @
--
-- = 'Component' composition
--
-- As of @1.9@, @miso@ 'Component's can contain other 'Component's. This is
-- accomplished through the component mounting combinator @('+>')@. This combinator
-- is responsible for encoding a typed 'Component' hierarchy, allowing components
-- type-safe read-only access to their parent's model state.
--
-- This combinator unifies the parent @model@ with the child @parent@, and
-- subsequently the grandchild @parent@ unifies with the child @model@. This
-- gives us a correct-by-construction 'Component' hierarchy.
--
-- @
-- (+>)
--   :: forall child model action a . Eq child
--   => MisoString
--   -> Component model child action
--   -> View model a
-- key +> vcomp = VComp [ Property "key" (toJSON key) ] (SomeComponent vcomp)
-- @
--
-- Practically, using this combinator looks like:
--
-- @
-- view :: Int -> View Int action
-- view x = div_ [ id_ "container" ] [ "counter" +> counter ]
-- @
--
-- You'll notice the @\"counter\"@ string was specified. This is a unique key
-- to identify a 'Component' at runtime. These keys are very important when
-- diffing two components together.
--
-- It is possible to mount a component using the 'mount_' function, which avoids specifying a 'key_', but this should only be used
-- when the user is certain they will not be diffing their 'Component' with another 'Component'. When in doubt, use the @('+>')@ combinator
-- and key your 'Component'.
--
-- Lastly, note also the signature of 'startApp'.
--
-- @
-- startApp :: Eq model => Events -> App model action -> IO ()
-- @
--
-- The 'App' type signature is a synonym for 'Component ROOT'
--
-- @
-- type App model action = Component ROOT model action
-- @
--
-- 'ROOT' is a type tag that encodes a 'Component' as top-level. Which means it has no @parent@, hence we mark it as 'ROOT'.
--
-- @
-- data ROOT
-- @
--
-- 'startApp' and 'miso' will always infer @parent@ as 'ROOT'.
--
-- = 'Component' lifecycle hooks
--
-- Components are mounted on the fly during diffing. All t'Component` are equipped with a `mount` and `unmount` function that can define a custom action that will be invoked when a lifecycle event occurs (Component creation / Component destruction).
--
-- * 'mount'
-- * 'unmount'
--
-- = 'Node' lifecycle hooks
--
-- Similar to t'Component' lifecycle hooks, all virtual DOM nodes expose. There are also `-with` variants that pass along the underlying 'DOMRef'. This is convenient for initializing third-party libraries (as seen below with [highlight.js](https://highlightjs.org/))
--
-- * 'onBeforeCreated'
-- * 'onCreated' / 'onCreatedWith'
-- * 'onBeforeDestroyed' / 'onBeforeDestroyedWith'
-- * 'onDestroyed'
--
-- @
-- {-# LANGUAGE QuasiQuotes -#}
-- {-# LANGUAGE MultilineStrings -#}
--
-- import Miso
--
-- data Action = Highlight DOMRef
--
-- update :: Action -> Effect parent model Action
-- update = \case
--    Highlight domRef -> io_ $ do
--       [js| hljs.highlight({domRef}) |]
--
-- view :: model -> View model Action
-- view x =
--   code_
--   [ onCreatedWith Highlight
--   ]
--   [ """
--     function addOne (x) { return x + 1; }
--     """
--   ]
-- @
--
-- == Keys
--
-- Virtual DOM nodes can be \"keyed\". Keys have multiple purposes in @miso@ (and React).
--
-- * 1) Keys are used to optimize child node list diffing.
--
-- When two lists of elements are being diffed, as long as they all have unique keys, diffing large child lists will be much faster. This optimization automatically occurs when all the elements in a 'VNode' child list contain unique keys. Unless all 'View' nodes in a child list are keyed, this optimization will not fire.
--
-- * 2) Keys are used to compare two identical nodes. If two `VNode` are being compared (or two `VComp`), and their keys differ, the old node will be destroyed and a new one created. Otherwise, the underlying DOM node won't be removed, but its properties will be diffed. In the case of diffing two t'Component' (the t'VComp' case), if the keys differ, the `unmount` phase will be triggered.
--
-- See the 'key_' property for usage (and smart constructors like 'textKey_' and @('+>')@ as well).
--
-- @
--   ul_
--   []
--   [ li_ [ key_ "key-1" ] [ "a" ]
--   , li_ [ key_ "key-2" ] [ "b" ]
--   , "key-3" +> counter
--   , textKey "key-4" "text here"
--   ]
-- @
--
-- = The t'View' templating DSL
--
-- The 'View' type is the core type for templating a web page. It is similar to
-- a [Rose tree](https://en.wikipedia.org/wiki/Rose_tree) data structure. It is
-- mutually recursive with the 'Component' type, which allows us to embed
-- 'Component's inside one another. This is how the Virtual DOM is constructed in Haskell.
--
-- @
-- data View model action
--   = VNode Namespace MisoString [Attribute action] [View model action]
--   | VText (Maybe Key) MisoString
--   | VComp [Attribute action] (SomeComponent model)
-- @
--
-- The exisential type of 'SomeComponent' is defined recursively in terms of 'View' and is what allows
-- us to embed 'Component'.
--
-- @
-- data SomeComponent parent
--   = forall model action . Eq model
--   => SomeComponent (Component parent model action)
-- @
--
-- 'VNode' and 'VText' have a one-to-one mapping from the virtual DOM to the physical DOM. The 'VComp' constructor is abstract and does not contain a reference to the physical DOM.
--
-- The smart constructors 'node', 'text', 'component' and @('+>')@ are used to
-- build 'VNode', 'VText' and 'VComp' respectively.
--
-- = Events
--
-- * Event Delegation (bubble / capture)
--
-- All events are delegated through @<body>@. Miso supports both `capture` and `bubble` phases of browser events.
-- Users can handle both phases in their applications.
--
-- * Using events
--
-- Miso exposes the 'defaultEvents' map, these events are listened for on @<body>@ and get routed through the 'View'. There
-- are other default 'Event' maps that are exposed as conveniences. All events required by all 'Component' must
-- union'd together for use when running your application.
--
-- @
-- touchEvents :: Events
-- touchEvents = M.fromList
--   [ ("touchstart", BUBBLE)
--   , ("touchcancel", BUBBLE)
--   , ("touchmove", BUBBLE)
--   , ("touchend", BUBBLE)
--   ]
-- @
--
-- * Defining events
--
-- Users can define their own events using the 'Miso.Event.on' combinator. By default this will define an event in the 'Miso.Event.Types.BUBBLE' phase. See 'Miso.Event.onCapture' for handling events during the capture phase. See the 'Miso.Html.Event' for
-- many predefined events.
--
-- @
-- -----------------------------------------------------------------------------
-- -- | https://developer.mozilla.org/en-US/docs/Web/Events/change
-- onChangeWith :: (MisoString -> DOMRef -> action) -> Attribute action
-- onChangeWith = on "change" valueDecoder
-- @
--
-- * 'Decoder'
--
-- After an event has been raised, a user is able to extract information from the event for use in their application. This is accomplished through a 'Decoder'.
--
-- Many common decoders are available for use in 'Miso.Event.Decoder'.
--
-- @
-- data Decoder a
--   = Decoder
--   { decoder :: Value -> Parser a
--     -- ^ FromJSON-based Event decoder
--   , decodeAt :: DecodeTarget
--     -- ^ Location in DOM of where to decode
--   }
-- @
-- -----------------------------------------------------------------------------
-- -- | Retrieves "value" field in t'Decoder'
-- valueDecoder :: Decoder MisoString
-- valueDecoder = Decoder {..}
--   where
--     decodeAt = DecodeTarget ["target"]
--     decoder = withObject "target" $ \o -> o .: "value"
-- @
--
-- = Attributes / Properties
--
-- The 'Attribute' type allows us to define web handlers to map browser events to
-- Haskell data types, along with specifying properties on DOM elements
-- (like @class@ and @id@). See 'Miso.Html' for more information.
--
-- @
-- div_ [ id_ "some-id", className "some-class" ] [ ]
-- @
--
-- = Effect
--
-- The core type of 'Effect' is a Reader-Writer-State 'Control.Monad.RWS.RWS'.
-- This is how the 'model' can be mutated over time in response to 'action's.
-- This allows 'IO' to be scheduled for evaluation by the @miso@ scheduler.
--
-- @
-- type Effect parent model action = RWS (ComponentInfo parent) [Schedule action] model ()
-- @
--
-- The core primitives for working inside of 'Effect' are:
--
-- * 'withSink': The core function (from which all other combinators are defined)
--   that gives users access to the underlying event sink. This also allows us to
--   introduce 'IO' into the system. The @miso@ scheduler attaches exception
--   handlers to all 'IO' actions.
--
-- * 'io': Allows users to schedule asynchronous 'IO' operations that will be
--   evaluated in their own thread.
--
-- * 'sync': Forces the scheduler to evaluate 'IO' synchronously. It is
--   recommended to use the 'io' function since this will block the scheduler.
--
-- == Component Communication
--
-- Components are able to communicate to each asynchronously via a message-passing system.
-- The miso runtime exposes a few primitives to allow t'Component' communication.
--
-- * broadcast
--
-- All t'Component' a mailbox that can receive messages (as t'Value') from other t'Component'.
--
-- This is meant to be used with the 'checkMail' function.
--
-- * mail
--
-- The 'mail' function allows a 'Component' to send a specific message (as 'Value') to another t'Component' via its t'ComponentId'.
-- The 'ComponentId' can be found in the 'Effect' monad. Using 'ask' will return a t'ComponentInfo'. The 'Component' receiving
-- the message will find it in its 'mailbox'.
--
-- * PubSub / Topic
--
-- We have added support for a publisher / subscriber model. See 'Miso.PubSub' for more information.
--
-- = Subscriptions
--
-- A t'Subscription' is any long-running operation that is external to a Component, but that a
-- Component would like to be notified of any events that occur.
--
-- * subs
--
-- The 'subs' field of 'Component' contains 'Subscription' that exist for the lifetime of that 'Component'.
-- When a 'Component' unmounts, these 'Subscription' will be stopped.
--
-- * createSub
--
-- 'createSub' is a helper function for creating a 'Subscription' using the 'Control.Exception.bracket' pattern.
-- This ensures that event listeners can be unregistered when a 'Component' unmounts. For example usage
-- please see the 'Miso.Subscription' submodules. 'createSub' is only meant to be used in scenarios where
-- custom event listeners are used.
--
-- @
-- onLineSub :: (Bool -> action) -> Sub action
-- onLineSub f sink = createSub acquire release sink
--   where
--     release (cb1, cb2) = do
--       FFI.windowRemoveEventListener "online" cb1
--       FFI.windowRemoveEventListener "offline" cb2
--     acquire = do
--       cb1 <- FFI.windowAddEventListener "online" (const $ sink (f True))
--       cb2 <- FFI.windowAddEventListener "offline" (const $ sink (f False))
--       pure (cb1, cb2)
-- @
--
-- * 'startSub' / 'stopSub'
--
-- At times its necessary to dynamically generate a 'Subscription' in reponse to an event. The 'startSub'
-- and 'stopSub' functions facilitate dynamic 'Subscription' creation / removal. E.g. starting a 'Miso.WebSocket' connection
-- when a user logs in.
--
-- @
--   update = \\case
--     StartTimer -> startSub ("timer" :: MisoString) timerSub
--     StopTimer -> stopSub "timer"
--     Log -> io_ (consoleLog "log")
--       where
--         timerSub sink = forever $ (threadDelay 100000) >> sink Log
-- @
--
-- = (2D/3D) Canvas support
--
--  Miso has full 2D and 3D canvas support. See the 'Miso.Canvas' module and the [miso-canvas](https://github.com/haskell-miso/miso-canvas) example.
--
-- = State management (Lens)
--
--  A simple Lens implementation is included with miso, this was done for convenience and to minimize dependencies. See @Miso.Lens@. This is a simple lens formulation that exposes many common 'MonadState' lenses (e.g. @('+=')@) that work in the 'Effect' monad.
--
-- = HTML
--
-- Miso's 'View' can also be repurposed to render HTML. See the 'Miso.Html.Render' module for the 'Miso.Html.Render.ToHtml' class.
--
-- = JavaScript EDSL
--
-- Miso provides a Javascript DSL (inspired by [jsaddle](https://hackage.haskell.org/packages/jsaddle)) via 'Miso.DSL'.
-- See the t'ToJSVal' / t'FromJSVal' typeclasses when marshaling to and from Haskell to JavaScript. See also the 'Miso.DSL.jsg'
-- function for accessing JavaScript objects that exist in the global scope.
--
-- = QuasiQuotation
--
-- Along with Miso.DSL, a JavaScript QuasiQuoter is now included (See @Miso.FFI.QQ@). This makes it easy to
-- integrate miso with any third-party JavaScript library.
--
-- @
--
-- {-# LANGUAGE QuasiQuotes #-}
--
-- import Miso.FFI.QQ (js)
-- update msg = \case
--   Log -> io_ [js| console.log(${msg}) |]
--
-- @
--
-- = Routing
--
-- miso has its own 'Miso.Router', that uses the browser's [History](https://developer.mozilla.org/en-US/docs/Web/API/History) API.
-- See 'Miso.Router' for more information. The router is inspired by both servant and the web-routes package.
--
-- = MisoString
--
-- miso includes its own string type named t'MisoString'. This is the preferred string type to use
-- in order to maximize application performance. Since strings are ubiquitous in applications we
-- want to minimize copying strings between the JS and Haskell heaps. t'MisoString' accomplishes this.
--
-- t'MisoString' is a synonym for t'JSString' when using the JS / WASM backends. When using vanilla GHC
-- it is t'Text'.
--
-- Along with its own string type, miso uses MisoString to implement its own lexer and parser modules 'Miso.Util.Lexer' / 'Miso.Util.Parser' etc.
--
-- = JSON
--
-- 'Miso.JSON' is a [microaeson](https://hackage.haskell.org/packages/microaeson)
-- implementation that also uses t'MisoString'. This is done for performance reasons and to minimize the dependency burden.
--
-- = Styles
--
-- Miso prescribes no exact style usage. It is up to the user's discretion on how best to handle styles in their application. Inline styles, external stylesheets and the 'Miso.CSS' 'Miso.CSS.StyleSheet' DSL can all be used. See also [miso-ui](https://ui.haskell-miso.org) for an example of what is possible.
--
-- = Development
--
-- When developing miso applications interactively it is possible to append styles and scripts to the @<head>@ portion of
-- the page. This is a convenience not meant to be used in production. We recommend guarding the usage behind a flag.
--
-- @
-- main :: IO ()
-- main = startApp defaultEvents counter
--  where
--    app = counter
-- #ifdef INTERACTIVE
--      { scripts = [ Src "https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js" ]
--      , styles = [ Href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" ]
--      }
-- #endif
-- @
--
-- See the [miso-sampler](https://github.com/haskell-miso/miso-sampler) repository for more information on how to setup the CPP flag.
--
-- = Debugging
--
-- Sometimes things can go wrong. Common errors like using `onClick` but not listening for the the 'click' event are common.
-- These are errors that cannot be caught statically. These can be detected by enabling 'DebugAll'. Currently debugging event delegation
-- and page hydration is supported.
--
-- * 'DebugHydrate'
-- * 'DebugEvents'
--
-- @
-- counter { logLevel = DebugAll }
-- @
--
-- = Internals
--
-- Internally miso uses a global event queue and a scheduler to process all events raised by all 'Component' throughout the lifetime of
-- an application. Events are processed in FIFO order, batched by the 'Component' that raised them.
--
-----------------------------------------------------------------------------
module Miso
  ( -- * API
    -- ** Miso
    miso
  , prerender
  , (🍜)
    -- ** App
  , App
  , startApp
  , renderApp
    -- ** Component
  , Component (..)
  , component
  , (+>)
  , mount_
    -- ** Sink
  , withSink
  , Sink
    -- ** Mail
  , mail
  , checkMail
  , parent
  , mailParent
  , broadcast
    -- ** Subscriptions
  , startSub
  , stopSub
  , Sub
    -- ** Effect
  , issue
  , batch
  , io
  , io_
  , sync
  , sync_
  , for
  -- ** JS file embedding
#ifdef WASM
  , evalFile
#endif
  , withJS
    -- * Reactivity (Data bindings)
    -- | Primitives for synchronizing parent and child models.
  , module Miso.Binding
    -- * DSL
    -- | A JavaScript DSL for easy FFI interoperability
  , module Miso.DSL
    -- * Effect
    -- | 'Effect', 'Sub', and 'Sink' types for defining update functions and subscriptions.
  , module Miso.Effect
    -- * Event
    -- | Functions for specifying component lifecycle events and event handlers.
  , module Miso.Event
    -- * Fetch
    -- | Interface to the Fetch API for making HTTP requests.
  , module Miso.Fetch
    -- * PubSub
    -- | Publish / Subscribe primitives for communication between components.
  , module Miso.PubSub
    -- * Property
    -- | Construct custom properties on DOM elements.
  , module Miso.Property
    -- * Reload
    -- | Support for clearing the page during live-reloading w/ WASM browser mode.
  , module Miso.Reload
    -- * Subscriptions
    -- | Subscriptions for external events (mouse, keyboard, window, history, etc.).
  , module Miso.Subscription
    -- * Storage
    -- | Web Storage API (Local and Session storage) interface.
  , module Miso.Storage
    -- * Types
    -- | Core types for Miso applications.
  , module Miso.Types
    -- * Util
    -- | Utility functions for views, parsing, and general purpose combinators.
  , module Miso.Util
    -- * FFI
    -- | Foreign Function Interface (FFI) utilities for interacting with JavaScript.
  , module Miso.FFI
    -- * State management
    -- | State management for Miso applications.
  , module Miso.State
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void)
-----------------------------------------------------------------------------
import           Miso.Binding
import           Miso.Diff
import           Miso.DSL
import           Miso.Effect
import           Miso.Event
import           Miso.Fetch
import           Miso.FFI
import qualified Miso.FFI.Internal as FFI
import           Miso.Property
import           Miso.PubSub
import           Miso.Reload
import           Miso.Router
import           Miso.Runtime
import           Miso.State
import           Miso.Storage
import           Miso.Subscription
import           Miso.Types
import           Miso.Util
----------------------------------------------------------------------------
-- | Runs an isomorphic @miso@ application.
--
-- Assumes the pre-rendered DOM is already present.
-- Always mounts to \<body\>. Copies page into the virtual DOM.
--
-- @
-- main :: IO ()
-- main = miso defaultEvents (\\uri -> app uri))
-- @
miso
  :: Eq model
  => Events
  -- ^ Globally delegated Events
  -> (URI -> App model action)
  -- ^ The Component application, with the current URI as an argument
  -> IO ()
miso events f = withJS $ do
  vcomp <- f <$> getURI
  initComponent events Hydrate vcomp { mountPoint = Nothing }
----------------------------------------------------------------------------
-- | Like 'miso', except discards the 'URI' argument.
--
-- Use this function if you'd like to prerender, but not use navigation.
--
-- @
-- main :: IO ()
-- main = prerender defaultEvents app
-- @
prerender
  :: Eq model
  => Events
  -- ^ Globally delegated Events
  -> App model action
  -- ^ Component application
  -> IO ()
prerender events vcomp = initComponent events Hydrate vcomp { mountPoint = Nothing }
-----------------------------------------------------------------------------
-- | Like 'miso', except it does not perform page hydration.
--
-- This function draws your application on an empty <body>
--
-- You will most likely want to use this function for your application
-- unless you are using prerendering.
--
-- @
-- main :: IO ()
-- main = startApp defaultEvents app
-- @
--
startApp
  :: Eq model
  => Events
  -- ^ Globally delegated Events
  -> App model action
  -- ^ Component application
  -> IO ()
startApp events = initComponent events Draw
-----------------------------------------------------------------------------
-- | Alias for 'Miso.miso'.
(🍜)
  :: Eq model
  => Events
  -- ^ Globally delegated Events
  -> (URI -> App model action)
  -- ^ Component application, with the current URI as an argument
  -> IO ()
(🍜) = miso
----------------------------------------------------------------------------
-- | Runs a 'miso' application, but with a custom rendering engine.
--
-- The 'MisoString' specified here is the variable name of a globally-scoped
-- JS object that implements the context interface per @ts\/miso\/context\/dom.ts@
-- This is necessary for native support.
--
-- It is expected to be run on an empty @<body>@
--
-- @
-- main :: IO ()
-- main = renderApp defaultEvents "my-context" app
-- @
renderApp
  :: Eq model
  => Events
  -- ^q Globally delegated Events
  -> MisoString
  -- ^ Name of the JS object that contains the drawing context
  -> App model action
  -- ^ Component application
  -> IO ()
renderApp events renderer vcomp = do
  FFI.setDrawingContext renderer
  initComponent events Draw vcomp
----------------------------------------------------------------------------
-- | Top-level t'Miso.Types.Component' initialization helper for 'renderApp'.
initComponent
  :: (Eq parent, Eq model)
  => Events
  -> Hydrate
  -> Component parent model action
  -> IO ()
initComponent events hydrate vcomp@Component {..} = withJS $ do
  root <- mountElement (getMountPoint mountPoint)
  void $ initialize events rootComponentId hydrate isRoot vcomp (pure root)
----------------------------------------------------------------------------
isRoot :: Bool
isRoot = True
----------------------------------------------------------------------------
-- | Load miso's javascript.
--
-- You don't need to use this function if you're compiling w/ WASM and using `miso` or `startApp`.
-- It's already invoked for you. This is a no-op w/ the JS backend.
--
-- If you need access to `Miso.FFI` to call functions from `miso.js`, but you're not
-- using `startApp` or `miso`, you'll need to call this function (w/ WASM only).
--
#ifdef PRODUCTION
#define MISO_JS_PATH "js/miso.prod.js"
#else
#define MISO_JS_PATH "js/miso.js"
#endif
withJS :: IO a -> IO a
withJS action = do
#ifdef WASM
  $(evalFile MISO_JS_PATH)
#endif
  action
-----------------------------------------------------------------------------
