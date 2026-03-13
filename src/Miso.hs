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
-- Copyright   :  (C) 2016-2026 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Miso 🍜
--
-- @miso@ is a library for building web and native user interface applications in Haskell. See the [GitHub group](https://github.com/haskell-miso).
--
-- It provides a [React](https://react.dev)-like programming experience for a simple [Haskell](https://haskell.org) dialect that emphasizes
--
-- * performance
-- * purity
-- * simplicity
-- * extensibility
-- * composability
--
-- miso supports common areas that arise naturally in web development:
--
-- * __DOM manipulation__: @miso@ uses a [Virtual DOM](https://en.wikipedia.org/wiki/Virtual_DOM) with diffing algorithm that is
--   responsible for all DOM modification and 'Component' lifecycle hooks.
--
-- * __Event delegation__: All event listeners are attached to a top-level element
--   (typically @\<body\>@). When raised, events are routed through the virtual DOM
--   to Haskell event handlers which cause application state changes. Internally @miso@
--   virtualizes both the @capture@ and @bubble@ phases of the browser when it performs event routing.
--
-- * __Prerendering__: Prerendering is a process where the server delivers HTML
--   to the client before the JavaScript (or Web Assembly) application bootstraps.
--   Instead of performing an initial draw, the application will create and populate the virtual DOM from the actual DOM.
--   This is a process known as \"hydration\". This avoids unnecessary page draws on initial page
--   load and enhances search engine optimization. @miso@ provides its own HTML rendering
--   ("Miso.Html.Render") to render HTML on the server and the 'miso' function exists on the client to \"hydrate\"
--   the virtual DOM with the DOM.
--
-- * __Components__: A 'Component' can be considered an instance of a @miso@ application. A 'Component'
--   contains user-defined state, logic for updating this state, and a function
--   for creating UI templates from this user-defined state. 'Component' can nest other 'Component' because @miso@
--   is defined recursively.
--
-- * __Custom renderers__: The underlying DOM operations are able to be abstracted.
-- This allows a custom rendering engine to be used. This is seen in the [miso-lynx](https://github.com/haskell-miso/miso-lynx) project
-- (which allows miso to target mobile phone devices).
--
-- * __Lifecycle hooks__: 'Component' expose 'Miso.Types.mount' and 'Miso.Types.unmount' lifecycle hooks. This allow users to define custom logic that will
-- execute when a 'Component' mounts or unmounts. 'Miso.Event.onCreated' and 'Miso.Event.onDestroyed' are 'VNode' specific lifecycle hooks.
-- These hooks are commonly used for 'Component' communication and for third-party integration with JavaScript libraries.
--
-- * __State management__: 'Component' @model@ state can be manipulated using "Miso.Lens" or "Miso.State" in response to application events.
--
-- = The Model-View-Update pattern
--
-- The core type of miso is 'Component'. The 'Component' API adheres to the [Elm](https://elm-lang.org)
-- MVU (model-view-update) interface. This is similar to a left-fold, where the 'Component' @model@
-- will be updated via a list of @action@ given a specific 'Miso.Types.update' function, and rendered via 'Miso.Types.view'.
--
-- * __model__: This can be any user-defined type in Haskell. An 'Eq' constraint
--   is required. We recommend using the default derived 'Eq' instance.
--
-- * __view__: @'view' :: model -> 'View' model action@
--   This is the templating function that is used to construct a new virtual DOM
--   (or HTML if rendering on the server).
--
-- * __update__: @'update' :: action -> 'Effect' parent model action@
--   The 'update' function handles how the 'model' evolves over time in response
--   to events that are raised by the application. This function takes any @action@,
--   updating the @model@ and optionally introducing 'IO' into the system.
--
-- = Your first t'Component'
--
-- To define a 'Component' the 'component' smart constructor can be used.
-- Below is an example of a simple counter 'Component'.
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
--                        * - The type of the parent Component 'model'
--                        |     * - The type of the current Component's 'model'
--                        |     |    * - The type of the action that updates the 'model'
--                        |     |    |
-- counter :: 'Component' parent Int Action
-- counter = 'component' m u v
--   where
--     m :: Int
--     m = 0
--
--     u :: Action -> 'Effect' parent Int Action
--     u = \\case
--       Add -> 'this' += 1
--       Subtract -> 'this' -= 1
--
--     v :: Int -> 'View' Int Action
--     v x = H.div_
--       [ H.button_ [ HE.onClick Add, HP.id_ "add" ] [ "+" ]
--       , text (ms x)
--       , H.button_ [ HE.onClick Subtract, HP.id_ "subtract" ] [ "-" ]
--       ]
-- -----------------------------------------------------------------------------
-- main :: IO ()
-- main = 'startApp' 'defaultEvents' counter
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
-- The 'startApp' (or 'miso') functions are used to run the above t'Component'.
--
-- @
-- main :: IO ()
-- main = 'startApp' 'defaultEvents' counter
-- @
--
-- The 'startApp' function is what we recommend using first. It sets up event listeners and performs the initial page draw.
-- The 'startApp' function assumes that @\<body\>@ is empty, and it will begin drawing the 'Component' 'View' from @\<body\>@.
--
-- The 'miso' function (and also the 'prerender' function) assume that @\<body\>@ has already been populated by the results of the 'view' function.
-- Instead of drawing, 'miso' will perform hydration.
-- If the structures do not match 'miso' will fallback to drawing the page from scratch (clearing the contents of @\<body\>@ first).
--
-- It is possible to execute an initial action when a t'Component' is first mounted. See the 'mount' (and similarly 'unmount') hooks.
--
-- @
--
-- data Action = Init
--
-- main :: IO ()
-- main = 'startApp' 'defaultEvents' counter { 'mount' = Just Init }
--
-- update :: 'App' model Action
-- update = \\case
--   Init -> 'io_' ('consoleLog' "hello world!")
-- @
--
-- = 'Component' composition
--
-- @miso@ 'Component' can contain other 'Component'. This is
-- accomplished through the 'Component' mounting combinator ('+>'). This combinator
-- is responsible for encoding a typed 'Component' hierarchy, allowing 'Component'
-- type-safe read-only access to their @parent@ model state.
--
-- This combinator unifies the parent @model@ with the child @parent@, and
-- subsequently the grandchild @parent@ unifies with the child @model@. This
-- gives us a correct-by-construction 'Component' hierarchy.
--
-- @
-- ('+>')
--   :: forall child model action a . Eq child
--   => 'MisoString'
--   -> 'Component' model child action
--   -> 'View' model a
-- key '+>' vcomp = 'VComp' [ 'Property' "key" ('toJSON' key) ] ('SomeComponent' vcomp)
-- @
--
-- Practically, using this combinator looks like:
--
-- @
-- view :: Int -> 'View' Int action
-- view x = 'div_' [ 'id_' "container" ] [ "counter" '+>' counter ]
-- @
--
-- You'll notice the @\"counter\"@ string was specified. This is a unique 'Key'
-- to identify a 'Component' at runtime. These keys are very important when
-- diffing two 'Component' together. When intentionally replacing 'Component' it is important
-- to specify a new 'Key', otherwise the 'Component' will not be unmounted.
--
-- It is possible to mount a component using the 'mount_' function, which avoids specifying a 'key_', but this should only be used
-- when the user is certain they will not be diffing their 'Component' with another 'Component'. When in doubt, use the ('+>') combinator
-- and 'key_' your 'Component'.
--
-- Lastly, note also the signature of 'startApp'.
--
-- @
-- 'startApp' :: 'Eq' model => 'Events' -> 'App' model action -> IO ()
-- @
--
-- The 'App' type signature is a synonym for 'Component' 'ROOT'
--
-- @
-- type 'App' model action = 'Component' 'ROOT' model action
-- @
--
-- 'ROOT' is a type tag that encodes a 'Component' as top-level. Which means it has no @parent@, hence we mark @parent@ as 'ROOT'.
--
-- @
-- data 'ROOT'
-- @
--
-- 'startApp' and 'miso' will always infer @parent@ as 'ROOT'.
--
-- = 'VComp' lifecycle hooks
--
-- 'Component' are mounted on the fly during diffing. All t'Component` are equipped with `mount` and `unmount` hooks. This allows the defining of custom actions that will be processed in response to lifecycle events.
--
-- * 'Miso.Types.mount'
-- * 'Miso.Types.unmount'
--
-- = 'VNode' lifecycle hooks
--
-- Similar to t'Component' lifecycle hooks, all 'Miso.Types.VNode' also expose lifecycle hooks.
--
-- * 'Miso.Event.onBeforeCreated'
-- * 'Miso.Event.onCreated' / 'Miso.Event.onCreatedWith'
-- * 'Miso.Event.onBeforeDestroyed' / 'Miso.Event.onBeforeDestroyedWith'
-- * 'Miso.Event.onDestroyed'
--
-- This is convenient for initializing and deinitializing third-party libraries (as seen below with [highlight.js](https://highlightjs.org/))
--
-- @
-- {-# LANGUAGE QuasiQuotes -#}
-- {-# LANGUAGE MultilineStrings -#}
--
-- import Miso
-- import Miso.FFI.QQ (js)
--
-- data Action = Highlight DOMRef
--
-- update :: Action -> 'Effect' parent model Action
-- update = \\case
--   Highlight domRef -> 'io_' $ do
--     ['js'| hljs.highlight(${domRef}) |]
--
-- view :: model -> 'View' model Action
-- view x =
--   'code_'
--   [ 'onCreatedWith' Highlight
--   ]
--   [ """
--     function addOne (x) { return x + 1; }
--     """
--   ]
-- @
--
-- = 'Key'
--
-- A 'Key' is a unique identifier used to optimize diffing.
--
-- Virtual DOM nodes can be \"keyed\" (See 'key_'). Keys have multiple meanings in @miso@ (and react).
--
-- * Keys are used to optimize child node list diffing.
--
-- When two lists of elements are being diffed, as long as they all have unique keys, diffing large child lists will be much faster. This optimization automatically occurs when all the elements in a 'VNode' child list contain unique keys. Unless all 'View' nodes in a child list are keyed, this optimization will not fire.
--
-- * Keys are used to compare two identical nodes.
--
-- If two `VNode` are being compared (or two `VComp`) and their keys differ, the old node will be destroyed and a new one created. Otherwise, the underlying DOM node won't be removed, but its properties will be diffed. In the case of diffing two t'Component' (the 'VComp' case), if the keys differ, the 'unmount' phase will be triggered for the old 'VComp' and the 'mount' phase will be triggered for the new 'Component'. The underlying DOM reference will be replaced.
--
-- See the 'key_' property for usage (and smart constructors like 'textKey_' and ('+>') as well).
--
-- @
-- 'ul_'
--   []
--   [ 'li_' [ 'key_' "key-1" ] [ "a" ]
--   , 'li_' [ 'key_' "key-2" ] [ "b" ]
--   , "key-3" '+>' counter
--   , 'textKey' "key-4" "text here"
--   ]
-- @
--
-- = t'View' DSL
--
-- The 'View' type is the core type for templating a web page. It is similar to
-- a [Rose tree](https://en.wikipedia.org/wiki/Rose_tree) data structure. This is how the Virtual DOM is constructed. It is
-- mutually recursive with the 'Component' type (via the 'view' function), which allows us to embed
-- 'Component' inside other 'Component'.
--
-- @
-- data 'View' model action
--   = 'VNode' 'Namespace' 'Tag' ['Attribute' action] ['View' model action]
--   | 'VText' (Maybe 'Key') 'MisoString'
--   | 'VComp' ['Attribute' action] ('SomeComponent' model)
-- @
--
-- 'VNode' and 'VText' have a one-to-one mapping from the virtual DOM to the physical DOM. The 'VComp' constructor is abstract and does not contain a reference to the physical DOM. The existential type of 'SomeComponent' is defined recursively in terms of 'View' and is what allows us to embed other polymorphic 'Component'.
--
-- @
-- data 'SomeComponent' parent
--   = forall model action . Eq model
--   => 'SomeComponent' ('Component' parent model action)
-- @
--
-- The smart constructors:
--
-- * 'node'
-- * 'text'
-- * 'component'
-- * ('+>')
--
-- are used to build 'VNode', 'VText' and 'VComp' respectively. A list of all the smart constructors defined in terms of 'node' (e.g. 'Miso.Html.Element.div_') can be found in "Miso.Html.Element".
--
-- = 'Events'
--
-- * Event Delegation
--
-- By default all events are delegated through @\<body\>@. Miso supports both @capture@ and @bubble@ phases of browser events.
-- Users can handle both phases in their applications.
--
-- * Using events
--
-- Miso exposes a 'defaultEvents' for convenience, these events are commonly used events and listened for on @\<body\>@. They get routed through the 'View' to the virtual DOM node that raised the event. Other 'Events' are exposed as conveniences (e.g. 'touchEvents'). All events required by all 'Component' must be combined together for use when running your application (e.g. @keyboardEvents <> touchEvents@).
--
-- @
-- 'touchEvents' :: 'Events'
-- 'touchEvents' = M.fromList
--   [ ("touchstart", 'BUBBLE')
--   , ("touchcancel", 'BUBBLE')
--   , ("touchmove", 'BUBBLE')
--   , ("touchend", 'BUBBLE')
--   ]
-- @
--
-- * Defining event handlers
--
-- Users can define their own event handlers using the 'Miso.Event.on' combinator. By default this will define an event in the 'Miso.Event.Types.BUBBLE' phase. See 'Miso.Event.onCapture' for handling events during the 'Miso.Event.Types.CAPTURE' phase. See the the module "Miso.Html.Event" for many predefined events.
--
-- @
-- 'onChangeWith' :: ('MisoString' -> 'DOMRef' -> action) -> 'Attribute' action
-- 'onChangeWith' = 'on' "change" 'valueDecoder'
-- @
--
-- * Decoding events
--
-- After an event has been raised, one can extract information from the event for use in their application. This is accomplished through a 'Decoder'. Many common decoders are available for use in "Miso.Event.Decoder".
--
-- @
-- data 'Decoder' a
--   = 'Decoder'
--   { 'decoder' :: 'Value' -> 'Parser' a
--   , 'decodeAt' :: 'DecodeTarget'
--   }
--
-- -- | Example of a custom 'Decoder' for the @value@ property of an event target.
-- 'valueDecoder' :: 'Decoder' 'MisoString'
-- 'valueDecoder' = Decoder {..}
--   where
--     decodeAt = 'DecodeTarget' ["target"]
--     decoder = 'withObject' "target" $ \\o -> o .: "value"
-- @
--
-- = Attributes / Properties
--
-- The 'Attribute' type allows us to define web handlers that map browser events to
-- Haskell data types (e.g. 'Miso.Html.Event.onClick'), along with specifying properties on DOM elements
-- (like 'Miso.Html.Property.className' and 'Miso.Html.Property.id_'). See "Miso.Property" and "Miso.Html.Property" for more information.
--
-- @
-- 'div_' [ 'id_' "some-id", 'className' "some-class" ] [ ]
-- @
--
-- = 'Effect'
--
-- The 'Effect' type is used to mutate the @model@ over time in response to @action@.
-- This allows 'IO' to be scheduled for evaluation by the @miso@ scheduler.
--
-- Note: 'IO' is never evaluated inside of 'Effect', it is only scheduled.
-- There is no 'MonadIO' instance for 'Effect'.
--
-- The 'Effect' type is defined as a 'RWS'.
--
-- @
-- type 'Effect' parent model action = 'RWS' ('ComponentInfo' parent) ['Schedule' action] model ()
-- @
--
-- 'IO' can be performed either synchronoulsy or asynchronously. By default all 'IO' is asynchronous
--
-- == Asynchronous 'IO'
--
-- * 'io': Used to introduce asynchronous 'io' into the system, see also the 'io_' variant.
--
-- * 'withSink': The core function (from which most other combinators are defined)
--   that gives users access to the underlying event 'Sink'. This also allows us to
--   introduce 'IO' into the system. The @miso@ scheduler attaches exception
--   handlers to all 'IO' actions.
--
-- * For maximum flexibility, the 'MonadWriter' instance ('tell') can be used to schedule 'IO' (see the 'withSink' implementation).
--
-- == Synchronous 'IO'
--
-- * 'sync': Forces the scheduler to evaluate 'IO' synchronously. It is
--   recommended to use the 'io' function by default, 'sync' *will* block the scheduler.
--
-- == 'Sink'
--
-- @
-- type 'Sink' a => a -> 'IO' ()
-- @
--
-- The 'Sink' function allows one to write any @action@ to the global event queue.
--
-- == Managing 'model' state.
--
-- Any 'MonadState' function is allowed for use when manipulating @model@, 'Miso.State.get', 'Miso.State.put', etc. See "Miso.State".
--
-- The 'MonadReader' instances allows the retrieval of 'ComponentInfo' within 'Effect'.
-- 'ComponentInfo' provides the current 'ComponentId' the @parent@ 'ComponentId', and the 'DOMRef' ('_componentDOMRef') that the 'Component' is mounted on.
--
-- = 'Component' communication
--
-- == Asynchronous communication
--
-- 'Component' are able to communicate asynchronously via a message-passing system.
-- The miso runtime exposes a few primitives to allow t'Component' communication.
--
-- * 'broadcast'
-- * 'mail'
-- * 'mailParent'
--
-- All t'Component' have a 'mailbox' that can receive messages (as t'Miso.JSON.Value') from other t'Component'.
-- This is meant to be used with the 'checkMail' function. The 'mail' function allows a 'Component' to send a specific message (as 'Value') to another t'Component' via its t'ComponentId'.
-- The 'ComponentId' can be found in the 'Effect' monad. Using 'ask' will return a t'ComponentInfo'. The 'Component' receiving
-- the message will find it in its 'mailbox'.
--
-- * "Miso.PubSub"
--
-- miso has support for the publisher / subscriber concurrency pattern. See the "Miso.PubSub" module for more information.
--
-- == Synchronous communication
--
-- * "Miso.Binding"
--
-- Experimental support for data bindings (where 'Component' model can synchronize fields via a 'Miso.Lens.Lens' in response to model differences along the parent-child relationship). See the "Miso.Binding" module for more information, and the [miso-reactive](https://github.com/haskell-miso/miso-reactive) example. *Warning*: This is still considered experimental.
--
-- * 'parent'
--
-- While not direct communication, a 'Component' can receive read-only access to its @parent@ state via the 'parent' function.
--
-- = Subscriptions
--
-- A t'Sub' is any long-running operation that is external to a 'Component', but that can write
-- to a 'Component' 'Sink'. 'Sub' come in two flavors, a dynamic 'Sub' (via 'startSub' / 'stopSub') and 'subs'.
--
-- * 'subs'
--
-- @
-- main :: IO ()
-- main = 'startApp' 'defaultEvents' app { 'subs' = [ timerSub ] }
--
-- timerSub :: 'Sub' Action
-- timerSub sink = 'forever' $ ('threadDelay' 100000) >> sink Log
--
-- data Action = Log
-- @
--
-- The 'subs' field of 'Component' contains 'Sub' that exist for the lifetime of that 'Component'.
-- When a 'Component' unmounts, these 'Sub' will be stopped, and their resources finalized.
--
-- @
-- 'onLineSub' :: (Bool -> action) -> 'Sub' action
-- 'onLineSub' f sink = 'createSub' acquire release sink
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
-- At times its necessary to dynamically generate a 'Sub' in reponse to an event (e.g. starting a "Miso.WebSocket" connection
-- when a user logs in). The 'startSub' and 'stopSub' functions facilitate dynamic 'Sub' creation / removal.
--
-- @
--   update = \\case
--     StartTimer -> 'startSub' ("timer" :: MisoString) timerSub
--     StopTimer -> 'stopSub' "timer"
--     Log -> 'io_' ('consoleLog' "log")
--       where
--         timerSub :: 'Sub' Action
--         timerSub sink = 'forever' $ ('threadDelay' 100000) >> sink Log
--
--   data Action = Log
-- @
--
-- * 'Miso.Subscription.Util.createSub'
--
-- 'Miso.Subscription.Util.createSub' is a helper function for creating a 'Sub' using the 'Control.Exception.bracket' pattern.
-- This ensures that event listeners can be unregistered when a 'Component' unmounts. For example usage
-- please see the "Miso.Subscription" sub modules. 'createSub' is only meant to be used in scenarios where
-- custom event listeners are required (shown below).
--
-- = (2D/3D) Canvas support
--
--  Miso has full 2D and 3D canvas support. See the "Miso.Canvas" module, the [miso-canvas](https://github.com/haskell-miso/miso-canvas2d) example, along with the [three-miso](https://github.com/haskell-miso/three-miso) package.
--
-- = 'State' management ('Lens')
--
--  A simple 'Miso.Lens.Lens' implementation is included with miso, this was done for convenience, to minimize dependencies, reduce payload size, and provide a simpler interface. See "Miso.Lens". This is a simple lens formulation that exposes many common 'MonadState' lenses (e.g. @'+='@) that work in the 'Effect' monad. "Miso.Lens" is not required for use, any lens library will also work with miso.
--
-- = HTML
--
-- Miso's virtual DOM DSL ('Miso.Types.View') type can be repurposed to render HTML. See the "Miso.Html.Render" module for more information. This uses the 'Miso.Html.Render.ToHtml' class.
--
-- = JavaScript EDSL
--
-- Miso provides a Javascript DSL (inspired by [jsaddle](https://hackage.haskell.org/package/jsaddle)) via "Miso.DSL".
-- See the 'Miso.DSL.ToJSVal' / 'Miso.DSL.FromJSVal' typeclasses when marshaling to and from Haskell to JavaScript. See also the 'Miso.DSL.jsg'
-- function for accessing JavaScript objects that exist in the global scope.
--
-- @
-- document :: 'JSVal' <- 'jsg' "document" :: IO 'JSVal'
-- len :: 'Int' <- 'fromJSValUnchecked' =<< (document ! "body" ! "children" ! "length")
-- @
--
-- = QuasiQuotation (@inline-js@)
--
-- Along with "Miso.DSL", a JavaScript QuasiQuoter is now included (See "Miso.FFI.QQ"). This makes it easy to
-- integrate miso with any third-party JavaScript library. This bindings in scope can be used inside the QuasiQuoter, which
-- will utilize their 'Miso.DSL.ToJSVal' instances. When returning values from the QuasiQuoter, the 'Miso.DSL.FromJSVal' instance will
-- be used Haskell.
--
-- @
--
-- {-# LANGUAGE QuasiQuotes #-}
--
-- import Miso.FFI.QQ ('js')
--
-- update :: Action -> 'Effect' parent model Action
-- update = \\case
--   Log msg -> io_ [js| console.log(${msg}) |]
--
-- data Action = Log MisoString
-- @
--
-- = Routing
--
-- miso exposes its own internal router. See "Miso.Router" for more information. The router is inspired by both the [servant](https://hackage.haskell.org/package/servant) and the [web-routes](https://hackage.haskell.org/package/web-routes) package. The router has its own 'Sub' called 'Miso.Router.routerSub' meant for easy integration with the History API.
--
-- = 'MisoString'
--
-- miso includes its own string type named t'MisoString'. This is the preferred string type to use
-- in order to maximize application performance. Since strings are ubiquitous in applications we
-- want to minimize the copying of these strings between the JS and Haskell heaps. t'MisoString' accomplishes this.
-- t'MisoString' is a synonym for t'JSString' when using the JS / WASM backends. When using vanilla GHC
-- it is t'Data.Text'. See "Miso.String" for more information.
--
-- For string conversions see the 'ms', 'fromMisoString' functions and 'ToMisoString' / 'FromMisoString' classes.
--
-- t'MisoString' is also used in the "Miso.Util.Lexer" and "Miso.Util.Parser" modules.
--
-- = JSON
--
-- "Miso.JSON" is a [microaeson](https://hackage.haskell.org/package/microaeson)
-- implementation that uses t'MisoString'. This is done for performance reasons and to minimize the dependency burden. "Miso.JSON" is used
-- in "Miso.Event.Decoder", "Miso.Fetch", "Miso.WebSocket" modules respectively.
--
-- = Styles
--
-- Miso prescribes no exact CSS style usage. It is up to the user's discretion on how best to handle styles in their application. Inline styles, external stylesheets and the "Miso.CSS" DSL can all be used. See also [miso-ui](https://ui.haskell-miso.org) for an example of what is possible.
--
-- = Development
--
-- When developing miso applications interactively it is possible to append 'styles' and 'scripts' to the @\<head\>@ portion of
-- the page when the 'Component' mounts. This is a convenience only meant to be used in development. We recommend guarding the usage behind a flag.
--
-- @
-- main :: 'IO' ()
-- main = 'startApp' 'defaultEvents' counter
--  where
--    app = counter
-- #ifdef INTERACTIVE
--      { 'scripts' = [ 'Src' "https://ajax.googleapis.com/ajax/libs/jquery/3.7.1/jquery.min.js" ]
--      , 'styles' = [ 'Href' "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" ]
--      }
-- #endif
-- @
--
-- See the [miso-sampler](https://github.com/haskell-miso/miso-sampler) repository for more information.
--
-- = Debugging
--
-- Sometimes things can go wrong. Common errors like using `onClick` but not listening for the the 'click' event are common.
-- These are errors that cannot be caught statically. These can be detected by enabling 'DebugAll'. Currently, debugging event delegation
-- and page hydration is supported.
--
-- * 'DebugHydrate'
-- * 'DebugEvents'
--
-- @
-- counter { 'logLevel' = 'DebugAll' }
-- @
--
-- = Internals
--
-- Internally miso uses a global event queue and a scheduler to process all events raised by 'Component' throughout the lifetime of
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
    -- * Bindings
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
import           Miso.Binding
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
-- | Runs an @miso@ application.
--
-- Assumes the pre-rendered DOM is already present.
-- Always mounts to \<body\>. Copies page into the virtual DOM.
--
-- @
-- main :: 'IO' ()
-- main = 'miso' 'defaultEvents' (\\uri -> app uri))
-- @
miso
  :: Eq model
  => Events
  -- ^ Globally delegated Events
  -> (URI -> App model action)
  -- ^ The Component application, with the current URI as an argument
  -> IO ()
miso events f = do
  vcomp <- f <$> getURI
  initComponent events Hydrate vcomp { mountPoint = Nothing }
----------------------------------------------------------------------------
-- | Like 'miso', except discards the 'URI' argument.
--
-- Use this function if you'd like to prerender, but not use navigation.
--
-- @
-- main :: 'IO' ()
-- main = 'prerender' 'defaultEvents' app
-- @
prerender
  :: Eq model
  => Events
  -- ^ Globally delegated 'Events'
  -> App model action
  -- ^ 'Component' application
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
-- main :: 'IO' ()
-- main = 'startApp' 'defaultEvents' app
-- @
--
startApp
  :: Eq model
  => Events
  -- ^ Globally delegated 'Events'
  -> App model action
  -- ^ 'Component' application
  -> IO ()
startApp events = initComponent events Draw
-----------------------------------------------------------------------------
-- | Alias for 'Miso.miso'.
(🍜)
  :: Eq model
  => Events
  -- ^ Globally delegated 'Events'
  -> (URI -> App model action)
  -- ^ 'Component' application, with the current URI as an argument
  -> IO ()
(🍜) = miso
----------------------------------------------------------------------------
-- | Runs a 'miso' application, but with a custom rendering engine.
--
-- The 'MisoString' specified here is the variable name of a globally-scoped
-- JS object that implements the context interface per @ts\/miso\/context\/dom.ts@
-- This is necessary for native support.
--
-- It is expected to be run on an empty @\<body\>@
--
-- @
-- main :: IO ()
-- main = 'renderApp' 'defaultEvents' "my-context" app
-- @
renderApp
  :: Eq model
  => Events
  -- ^ Globally delegated 'Events'
  -> MisoString
  -- ^ Name of the JS object that contains the drawing context
  -> App model action
  -- ^ 'Component' application
  -> IO ()
renderApp events renderer vcomp = do
  FFI.setDrawingContext renderer
  initComponent events Draw vcomp
----------------------------------------------------------------------------
