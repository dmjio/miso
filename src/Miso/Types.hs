-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Types
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Core types for Miso applications.
--
-- == Model-View-Update
--
-- Miso follows the __Model-View-Update (MVU)__ pattern popularised by Elm.
-- The central record is t'Component', which bundles together:
--
--   * @model@ — the application state (must derive 'Eq' so the runtime can
--     detect changes and skip unnecessary re-renders).
--   * @view@ — a pure function from @model@ to t'View', which describes the
--     desired DOM tree.
--   * @update@ — an action handler that produces an t'Effect', allowing the
--     model to be updated and\/or 'IO' to be scheduled.
--   * @subs@ — a list of long-running t'Sub' computations (timers, WebSockets,
--     etc.) that push actions into the component's queue.
--
-- Use 'component' (or its alias 'vcomp') to build a t'Component' with sensible
-- defaults, and 'Miso.startApp' to start a top-level t'App'.
--
-- == Virtual DOM
--
-- The t'View' type is the virtual DOM tree used for diffing and patching.
-- Leaf constructors are 'VText' (text nodes) and 'VComp' (nested components).
-- Branch constructors are 'VNode' (element nodes) and 'VFrag' (fragments).
-- You normally build t'View' values using the HTML combinators from
-- "Miso.Html" rather than these constructors directly.
--
-- == Component Composition
--
-- Components are nested using the '+>' or 'mount_' combinators.  Each child
-- component is fully encapsulated — it owns its own model, update loop, and
-- subscriptions.  The phantom @parent@ type parameter ensures that only the
-- correct parent can communicate with a given child.
----------------------------------------------------------------------------
module Miso.Types
  ( -- ** Types
    App
  , Component     (..)
  , ComponentId
  , SomeComponent (..)
  , View          (..)
  , Key           (..)
  , Attribute     (..)
  , Namespace     (..)
  , CSS           (..)
  , JS            (..)
  , LogLevel      (..)
  , VTree         (..)
  , VTreeType     (..)
  , Tag
  , CacheBust
  , MountPoint
  , DOMRef
  , ROOT
  , Events
  , Phase         (..)
  , URI           (..)
  -- ** Classes
  , ToKey         (..)
  -- ** Data Bindings
  , Binding       (..)
  -- ** Smart Constructors
  , emptyURI
  , component
  , vcomp
  , (-->)
  , (<--)
  , (<-->)
  , (<--->)
  , (--->)
  , (<---)
  -- ** Component mounting
  , (+>)
  , mount_
  , mountWithProps_
  , mountWithProps
  -- ** Key combinators
  , keyed
  -- ** Fragment combinators
  , fragment
  , fragment_
  , vfrag
  , vfrag_
  -- ** Utils
  , getMountPoint
  , optionalAttrs
  , optionalVoidAttrs
  , optionalChildren
  , prettyURI
  , prettyQueryString
  -- *** Combinators
  , node
  , vnode
  , text
  , vtext
  , text_
  , textRaw
  , textKey
  , textKey_
  , htmlEncode
  -- *** MisoString
  , MisoString
  , toMisoString
  , fromMisoString
  , ms
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           Data.String (IsString, fromString)
import qualified Data.Text as T
import           GHC.Generics
import           Prelude
-----------------------------------------------------------------------------
import           Miso.Binding ((<--), (-->), (<-->), (<---), (--->), (<--->), Binding(..))
import           Miso.DSL
import           Miso.Effect (Effect, Sub, Sink, DOMRef, ComponentId)
import           Miso.Event.Types
import           Miso.JSON (Value, ToJSON(..), encode)
import qualified Miso.String as MS
import           Miso.String (ToMisoString, MisoString, toMisoString, ms, fromMisoString)
import           Miso.CSS.Types (StyleSheet)
-----------------------------------------------------------------------------
-- | The central record of a Miso application, following the
-- __Model-View-Update__ pattern.
--
-- A t'Component' ties together an initial @model@, an @update@ function for
-- handling @action@s, a @view@ function that renders the model to a t'View',
-- and a set of long-running t'Sub' subscriptions.
--
-- The four type parameters are:
--
--   * @parent@ — phantom type that identifies the parent component (use 'ROOT'
--     for the top-level application component, which has no parent).
--   * @props@ — read-only data supplied by the parent on each render pass.
--   * @model@ — the component's internal state; must derive 'Eq'.
--   * @action@ — the sum type of messages the component can receive.
--
-- Use 'component' or 'vcomp' rather than the 'Component' constructor directly,
-- so you only need to supply the three required arguments and rely on sensible
-- defaults for everything else.
--
-- @
-- data Model  = Model { counter :: Int } deriving Eq
-- data Action = Increment | Decrement
--
-- myApp :: App Model Action
-- myApp = component (Model 0) update view
--   where
--     update Increment = modify (\m -> m { counter = counter m + 1 })
--     update Decrement = modify (\m -> m { counter = counter m - 1 })
--     view _ m = div_ [] [ text (ms (counter m)) ]
-- @
data Component parent props model action
  = Component
  { model :: model
  -- ^ Initial model used on first mount.
#ifdef SSR
  , hydrateModel :: Maybe (IO model)
#else
  , hydrateModel :: Maybe (IO model)
#endif
  -- ^ Optional 'IO' action that loads the component @model@ from the page
  --   during server-side rendering hydration.  The resulting @model@ is used
  --   only on the initial hydration pass and is ignored on subsequent remounts.
  , update :: action -> Effect parent props model action
  -- ^ Pure state-transition function.  Given an @action@, returns an t'Effect'
  --   that may update the @model@ and\/or schedule 'IO' side effects.
  , view :: props -> model -> View model action
  -- ^ Renders the current @model@ (and the @props@ supplied by the parent)
  --   to a virtual DOM t'View'.  This function must be pure.
  , subs :: [ Sub action ]
  -- ^ Long-running subscriptions started when the component mounts and
  --   cancelled when it unmounts (e.g. timers, WebSocket listeners).
  , styles :: [CSS]
  -- ^ CSS stylesheets to inject into \<head\> before the first render.
  --   Accepts external URLs ('Href') or inline CSS text ('Style').
  --
  -- @since 1.9.0.0
  , scripts :: [JS]
  -- ^ JavaScript resources to inject into \<head\> before the first render.
  --   Accepts external URLs ('Src'), inline scripts ('Script'), ES modules
  --   ('Module'), or import maps ('ImportMap').
  --
  -- @since 1.9.0.0
  , mountPoint :: Maybe MountPoint
  -- ^ CSS selector (e.g. @\"#app\"@) or tag name of the DOM element to mount
  --   the component on.  'Nothing' defaults to @\<body\>@.
  , logLevel :: LogLevel
  -- ^ Controls verbosity of Miso's internal debug logging.
  --   Use 'Off' in production and 'DebugAll' during development.
  , mailbox :: Value -> Maybe action
  -- ^ Decodes an incoming JSON message from another component (via
  --   'Miso.PubSub') into an @action@.  Return 'Nothing' to discard the
  --   message.
  --
  -- @since 1.9.0.0
  , bindings :: [ Binding parent model ]
  -- ^ Declarative data-binding rules that synchronise parts of the @model@
  --   with fields in the parent component's model.  See "Miso.Binding".
  --
  -- @since 1.9.0.0
  , eventPropagation :: Bool
  -- ^ When 'True', DOM events that are not handled by this component bubble
  --   up through the component boundary to the parent component's event
  --   delegator.  Defaults to 'False'.
  --
  -- @since 1.9.0.0
  , mount :: Maybe action
  -- ^ Action dispatched to 'update' immediately after the component is
  --   mounted in the DOM.  Useful for triggering initial data fetches.
  --
  -- @since 1.9.0.0
  , unmount :: Maybe action
  -- ^ Action dispatched to 'update' just before the component is removed
  --   from the DOM.  Useful for cleanup tasks.
  --
  -- @since 1.9.0.0
  , onPropsChanged :: Maybe (props -> props -> action)
  -- ^ Called by the runtime whenever the parent re-renders and supplies new
  --   @props@.  Receives the previous @props@ and the new @props@; the
  --   returned @action@ is dispatched to 'update'.
  --
  -- @since 1.11.0.0
  }
-----------------------------------------------------------------------------
-- | The CSS selector or tag name that identifies the DOM element a
-- t'Component' is mounted on.
--
-- Examples: @\"body\"@, @\"#app\"@, @\".container\"@
type MountPoint = MisoString
-----------------------------------------------------------------------------
-- | Describes a CSS resource to be injected into the document \<head\> before
-- the first render of a t'Component'.
--
-- The three constructors cover the most common use-cases:
--
-- @
-- -- Link to an external stylesheet (with cache-busting timestamp):
-- Href \"https://cdn.example.com/styles.css\" True
--
-- -- Inline CSS text:
-- Style \"body { margin: 0; padding: 0; }\"
--
-- -- Structured CSS built with the 'Miso.CSS' DSL:
-- Sheet myStyleSheet
-- @
--
-- @since 1.9.0.0
data CSS
  = Href MisoString CacheBust
  -- ^ External CSS URL.  When 'CacheBust' is 'True', a timestamp query
  --   parameter is appended to force cache invalidation.
  | Style MisoString
  -- ^ Raw CSS text, injected inside a @\<style\>@ tag.
  | Sheet StyleSheet
  -- ^ Structured CSS built with the 'Miso.CSS' DSL; serialised to a
  --   @\<style\>@ tag at runtime.
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | When 'True', Miso appends a Unix-millisecond timestamp as a query
-- parameter to external resource URLs ('Href', 'Src'), forcing the browser to
-- bypass its cache and re-fetch the asset.
--
-- Set to 'False' in production to allow normal HTTP caching.
type CacheBust = Bool
-----------------------------------------------------------------------------
-- | Allow users to express JS and append it to <head> before the first draw
--
-- This is meant to be useful in development only.
--
-- @
-- Src \"http:\/\/example.com\/script.js\" (False :: CacheBust)
-- Script "alert(\"hi\");"
-- ImportMap [ "key" =: "value" ]
-- Module "console.log(\"hi\");"
-- @
--
-- @since 1.9.0.0
data JS
  = Src MisoString CacheBust
  -- ^ URL linking to hosted JS
  | Script MisoString
  -- ^ Raw JS content that you would enter in a \<script\> tag
  | Module MisoString
  -- ^ Raw JS module content that you would enter in a \<script type="module"\> tag.
  -- See [script type](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/script/type)
  | ImportMap [(MisoString,MisoString)]
  -- ^ Import map content in a \<script type="importmap"\> tag.
  -- See [importmap](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/script/type/importmap)
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Unwrap a 'Maybe' 'MountPoint', defaulting to @\"body\"@ when 'Nothing' is
-- supplied.
getMountPoint :: Maybe MisoString -> MisoString
getMountPoint = fromMaybe "body"
-----------------------------------------------------------------------------
-- | Smart constructor for t'Component' with sensible defaults.
--
-- All optional fields ('subs', 'styles', 'scripts', 'mountPoint', 'logLevel',
-- 'mailbox', 'bindings', 'eventPropagation', 'mount', 'unmount',
-- 'onPropsChanged') are set to their zero\/off values.  Override individual
-- fields using Haskell record-update syntax:
--
-- @
-- myComp :: App Model Action
-- myComp = (component initModel update view)
--   { mountPoint = Just \"#app\"
--   , logLevel   = DebugHydrate
--   , subs       = [ timerSub ]
--   }
-- @
component
  :: model
  -- ^ Initial model
  -> (action -> Effect parent props model action)
  -- ^ Action handler (the @update@ function)
  -> (props -> model -> View model action)
  -- ^ Render function (the @view@ function)
  -> Component parent props model action
component m u v = Component
  { model = m
  , hydrateModel = Nothing
  , update = u
  , view = v
  , subs = []
  , styles = []
  , scripts = []
  , mountPoint = Nothing
  , logLevel = Off
  , mailbox = const Nothing
  , bindings = []
  , eventPropagation = False
  , mount = Nothing
  , unmount = Nothing
  , onPropsChanged = Nothing
  }
-----------------------------------------------------------------------------
-- | Alias for 'component'.
--
-- Provided for symmetry with the @vnode@ \/ @vtext@ \/ @vfrag@ naming
-- convention.  Prefer 'component' in new code unless the @vcomp@ name
-- reads more naturally in context.
vcomp
  :: model
  -- ^ Initial model
  -> (action -> Effect parent props model action)
  -- ^ Action handler (the @update@ function)
  -> (props -> model -> View model action)
  -- ^ Render function (the @view@ function)
  -> Component parent props model action
vcomp = component
-----------------------------------------------------------------------------
-- | Phantom type used as the @parent@ parameter of a top-level t'Component'.
--
-- 'ROOT' is an uninhabited type (analogous to 'Data.Void.Void') that signals
-- to the type checker that a component sits at the root of the component
-- tree and therefore has no parent to communicate with.
--
-- You rarely mention 'ROOT' directly; instead use the t'App' type alias:
--
-- @
-- type App model action = Component ROOT () model action
-- @
data ROOT
-----------------------------------------------------------------------------
-- | Trivial 'Eq' instance for the uninhabited t'ROOT' type.
instance Eq ROOT where _ == _ = True
-----------------------------------------------------------------------------
-- | Convenience alias for a top-level Miso application.
--
-- An t'App' is simply a t'Component' whose @parent@ is fixed to 'ROOT'
-- (no parent) and whose @props@ are @()@ (no external configuration).
-- Pass an t'App' to 'Miso.startApp' to start your application.
--
-- @
-- myApp :: App Model Action
-- myApp = component initModel update view
-- @
type App model action = Component ROOT () model action
-----------------------------------------------------------------------------
-- | Controls the verbosity of Miso's internal debug logging.
--
-- Set the 'logLevel' field of a t'Component' to enable different categories
-- of warnings.  In production builds, use 'Off'.
data LogLevel
  = Off
  -- ^ No debug output (default).
  | DebugHydrate
  -- ^ Warns when the server-rendered DOM structure or property values differ
  --   from the virtual DOM during the initial hydration pass.  Use this to
  --   diagnose SSR mismatches.
  | DebugEvents
  -- ^ Warns when a DOM event cannot be routed to its Haskell handler, or
  --   when an event handler is registered but the event is not being listened
  --   for by the component's event delegator.
  | DebugAll
  -- ^ Enables all of the above logging categories.
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | HTML\/SVG element tag name, e.g. @\"div\"@, @\"p\"@, @\"svg\"@.
--
-- Passed as the first argument to @document.createElement@ (or
-- @document.createElementNS@ for namespaced elements) by the Miso runtime.
-- Application code rarely needs to use this type directly; the combinators in
-- "Miso.Html.Element" and "Miso.Svg.Element" supply it automatically.
type Tag = MisoString
-----------------------------------------------------------------------------
-- | The virtual DOM tree type.
--
-- You normally build t'View' values using the HTML combinators from
-- "Miso.Html" rather than these constructors directly.  The constructors are
-- exposed for library authors and advanced use-cases.
--
-- The four constructors correspond to the four node kinds in the virtual DOM:
--
-- @
-- -- An element node (most HTML\/SVG elements):
-- VNode HTML \"div\" [ id_ \"container\" ] [ VText Nothing \"hello\" ]
--
-- -- A plain text node:
-- VText Nothing \"Hello, World!\"
--
-- -- A keyed text node (faster diffing when position changes):
-- VText (Just (Key \"greeting\")) \"Hello!\"
--
-- -- A nested child component:
-- VComp (Just (Key \"child\")) (SomeComponent () myComp)
--
-- -- A key-less fragment (groups siblings without a wrapper element):
-- VFrag Nothing [ VText Nothing \"a\", VText Nothing \"b\" ]
-- @
data View model action
  = VNode Namespace Tag [Attribute action] [View model action]
  -- ^ An element node with a namespace, tag name, attributes, and children.
  | VText (Maybe Key) MisoString
  -- ^ A text node with an optional key for identity-stable diffing.
  | VComp (Maybe Key) (SomeComponent model)
  -- ^ A child component, wrapped in 'SomeComponent' to erase its type
  --   parameters.  The optional t'Key' helps the differ match components
  --   across renders.
  | VFrag (Maybe Key) [View model action]
  -- ^ A fragment — a group of sibling nodes without a wrapping DOM element.
  --   The optional t'Key' enables identity-stable diffing of fragment groups.
  deriving Functor
-----------------------------------------------------------------------------
-- | Existential wrapper that erases the @model@, @action@, and @props@ type
-- parameters of a t'Component', allowing heterogeneous child components to be
-- stored inside a t'View'.
--
-- The @parent@ phantom type is preserved so the runtime can route messages
-- correctly between parent and child.  The 'Eq' constraints on @model@ and
-- @props@ are required so the differ can skip re-rendering when nothing has
-- changed.
data SomeComponent parent
   = forall model action props . (Eq model, Eq props)
  => SomeComponent props (Component parent props model action)
-----------------------------------------------------------------------------
-- | Like '+>' but operates on any 'View', not just 'Component'.
--
-- This appends a 'Key' to any 'View'.
--
-- @
-- keyed "key" ("some text" :: View model action)
-- keyed "key" $ div_ [ id_ "container" ] [ "content" ]
-- keyed "key" (mount_ calendarComponent)
-- @
--
-- @since 1.10.0.0
keyed
  :: MisoString
  -> View model action
  -> View model action
keyed key = \case
    VText _ txt ->
      VText (Just (Key key)) txt
    VComp _ comp ->
      VComp (Just (Key key)) comp
    VFrag _ kids ->
      VFrag (Just (Key key)) kids
    VNode ns tag attrs kids ->
      VNode ns tag (Property "key" (toJSON key) : attrs) kids
-----------------------------------------------------------------------------
-- | Create a fragment (keyless).
--
-- A fragment groups multiple sibling 'View' nodes without introducing
-- an extra DOM element.
--
-- Synonym for `fragment'
--
-- @since 1.10.0.0
vfrag :: [View model action] -> View model action
vfrag = fragment
-----------------------------------------------------------------------------
-- | Create a fragment (keyless).
--
-- A fragment groups multiple sibling 'View' nodes without introducing
-- an extra DOM element.
--
-- @since 1.10.0.0
fragment :: [View model action] -> View model action
fragment = VFrag Nothing
-----------------------------------------------------------------------------
-- | Like 'fragment', but keyed for efficient diffing.
--
-- @since 1.10.0.0
vfrag_ :: MisoString -> [View model action] -> View model action
vfrag_ key = VFrag (Just (Key key))
-----------------------------------------------------------------------------
-- | Like 'fragment', but keyed for efficient diffing.
--
-- @since 1.10.0.0
fragment_ :: MisoString -> [View model action] -> View model action
fragment_ key = VFrag (Just (Key key))
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' mounting combinator
--
-- Used in the @view@ function to mount a t'Miso.Types.Component' on any 'VNode'.
--
-- @
-- "component-id" +> component model noop $ \\m ->
--   div_ [ id_ "foo" ] [ text (ms m) ]
-- @
--
-- @since 1.9.0.0
(+>)
  :: forall child childAction model action . Eq child
  => MisoString
  -- ^ 'VComp' 'key_'
  -> Component model () child childAction
  -- ^ 'Component'
  -> View model action
infixr 0 +>
key +> comp = VComp (Just (toKey key)) (SomeComponent () comp)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' mounting combinator.
--
-- Note: only use this if you're certain you won't be diffing two t'Miso.Types.Component'
-- against each other. Otherwise, you will need a key to distinguish between
-- the two t'Miso.Types.Component', to ensure unmounting and mounting occurs.
--
-- @
-- mountWithProps someProps $ component model noop $ \\m ->
--  div_ [ id_ "foo" ] [ text (ms m) ]
-- @
--
-- @since 1.11.0.0
mountWithProps
  :: (Eq child, Eq props)
  => props
  -- ^ 'props' to use
  -> Component parent props child action
  -- ^ 'Component' to mount
  -> View parent a
mountWithProps props comp  = VComp Nothing (SomeComponent props comp)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' mounting combinator.
--
-- @
-- mountWithProps_ "key" someProps $ component model noop $ \\m ->
--  div_ [ id_ "foo" ] [ text (ms m) ]
-- @
--
-- @since 1.11.0.0
mountWithProps_
  :: (Eq child, Eq props)
  => MisoString
  -- ^ 'key' to use
  -> props
  -- ^ 'props' to use
  -> Component parent props child action
  -- ^ 'Component' to mount
  -> View parent a
mountWithProps_ key props comp  = VComp (Just (Key key)) (SomeComponent props comp)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' mounting combinator.
--
-- Note: only use this if you're certain you won't be diffing two t'Miso.Types.Component'
-- against each other. Otherwise, you will need a key to distinguish between
-- the two t'Miso.Types.Component', to ensure unmounting and mounting occurs.
--
-- @
-- mount_ $ component model noop $ \\m ->
--  div_ [ id_ "foo" ] [ text (ms m) ]
-- @
--
-- @since 1.9.0.0
mount_
  :: Eq child
  => Component parent () child childAction
  -- ^ 'Component' to mount
  -> View parent action
mount_ comp = VComp Nothing (SomeComponent () comp)
-----------------------------------------------------------------------------
-- | XML namespace used when creating a DOM element.
--
-- The Miso HTML, SVG, and MathML combinators set the appropriate namespace
-- automatically; you only need this type when using 'node' \/ 'vnode' directly.
data Namespace
  = HTML
  -- ^ The HTML namespace (@http:\/\/www.w3.org\/1999\/xhtml@).
  --   Used for standard HTML elements.
  | SVG
  -- ^ The SVG namespace (@http:\/\/www.w3.org\/2000\/svg@).
  --   Used for scalable vector graphics elements.
  | MATHML
  -- ^ The MathML namespace (@http:\/\/www.w3.org\/1998\/Math\/MathML@).
  --   Used for mathematical markup elements.
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal Namespace where
  toJSVal = \case
    SVG -> toJSVal ("svg" :: MisoString)
    HTML -> toJSVal ("html" :: MisoString)
    MATHML -> toJSVal ("mathml" :: MisoString)
-----------------------------------------------------------------------------
-- | An opaque identity key attached to a virtual DOM node.
--
-- Keys help the virtual DOM differ match nodes across render cycles,
-- preserving DOM state (focus, scroll position, CSS transitions) for nodes
-- that move within a list rather than being destroyed and recreated.
--
-- __Invariant__: keys must be unique among the children of a single parent
-- node.  Duplicate keys produce undefined behavior at runtime.
--
-- Use the 'ToKey' class to convert common types ('Int', 'T.Text', etc.) into
-- a t'Key', or use 'keyed' to attach a key to any t'View'.
newtype Key = Key MisoString
  deriving newtype (Show, Eq, IsString, ToJSON, ToMisoString)
-----------------------------------------------------------------------------
-- | ToJSVal instance for t'Key'
instance ToJSVal Key where
  toJSVal (Key x) = toJSVal x
-----------------------------------------------------------------------------
-- | Class for types that can be used as virtual DOM t'Key's.
--
-- Instances are provided for the most common key types: 'Key', 'MisoString',
-- 'T.Text', 'String', 'Int', 'Double', 'Float', and 'Word'.
--
-- __Laws__:
--
--   * @toKey@ must be /injective/ — distinct inputs must produce distinct
--     t'Key' values.  Violating this rule can cause the differ to confuse
--     unrelated nodes, leading to subtle rendering bugs.
class ToKey key where
  -- | Convert a value of type @key@ into a t'Key'.
  toKey :: key -> Key
-----------------------------------------------------------------------------
-- | Identity instance
instance ToKey Key where toKey = id
-----------------------------------------------------------------------------
#ifndef VANILLA
-- | Convert 'MisoString' to t'Key'
instance ToKey MisoString where toKey = Key
#endif
-----------------------------------------------------------------------------
-- | Convert 'T.Text' to t'Key'
instance ToKey T.Text where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert 'String' to t'Key'
instance ToKey String where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert 'Int' to t'Key'
instance ToKey Int where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert 'Double' to t'Key'
instance ToKey Double where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert 'Float' to t'Key'
instance ToKey Float where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert 'Word' to t'Key'
instance ToKey Word where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | An attribute that can be attached to a virtual DOM node.
--
-- The four constructors cover all ways Miso represents node properties and
-- event listeners:
--
--   * 'Property' — a JSON-encoded DOM property (e.g. @id@, @href@, @value@).
--   * 'ClassList' — a list of CSS class names, merged efficiently by the differ.
--   * 'On' — a DOM event listener wired to the component's 'Sink'.
--   * 'Styles' — inline CSS style key\/value pairs.
--
-- You normally construct t'Attribute' values through the smart constructors in
-- "Miso.Html.Property", "Miso.Html.Event", and "Miso.CSS" rather than using
-- these data constructors directly.
data Attribute action
  = Property MisoString Value
  -- ^ A DOM property represented as a JSON 'Value'.  The 'MisoString' is the
  --   property name (e.g. @\"id\"@, @\"className\"@).
  | ClassList [MisoString]
  -- ^ A list of CSS class names to apply to the element.  The differ merges
  --   these intelligently rather than replacing the whole @class@ attribute.
  | On (Sink action -> VTree -> LogLevel -> Events -> IO ())
  -- ^ A DOM event listener.  The callback receives the component's t'Sink'
  --   (so it can dispatch actions back to 'update'), the current virtual DOM
  --   node, the log level, and the active event map.
  | Styles (M.Map MisoString MisoString)
  -- ^ Inline CSS style declarations as a map from property name to value
  --   (e.g. @\"color\" -> \"red\"@).
  deriving Functor
-----------------------------------------------------------------------------
instance Eq (Attribute action) where
  Property k1 v1 == Property k2 v2 = k1 == k2 && v1 == v2
  ClassList x == ClassList y = x == y
  Styles x == Styles y = x == y
  _ == _ = False
-----------------------------------------------------------------------------
instance Show (Attribute action) where
  show = \case
    Property key value ->
      MS.unpack key <> "=" <> MS.unpack (ms (encode value))
    ClassList classes ->
      MS.unpack (MS.intercalate " " classes)
    On _ ->
      "<event-handler>"
    Styles styles ->
      MS.unpack $ MS.concat
        [ k <> "=" <> v <> ";"
        | (k, v) <- M.toList styles
        ]
-----------------------------------------------------------------------------
-- | 'IsString' instance
instance IsString (View model action) where
  fromString = VText Nothing . fromString
-----------------------------------------------------------------------------
-- | A live virtual DOM tree serialised as a JavaScript 'Object'.
--
-- 'VTree' is the compiled form of a t'View' that is handed to the TypeScript
-- runtime for diffing, patching, and event delegation.  It is produced by
-- the rendering pipeline and is __not__ meant to be constructed by
-- application code — use t'View' instead.
newtype VTree = VTree { getTree :: Object }
  deriving newtype (ToObject, ToJSVal)
-----------------------------------------------------------------------------
-- | Create a virtual DOM element node.
--
-- @node ns tag attrs children@ produces a t'VNode' in namespace @ns@ with
-- tag @tag@, the given list of @attrs@, and the given @children@.
--
-- @
-- -- Equivalent to \<div id=\"foo\"\>hello\<\/div\>:
-- node HTML \"div\" [ id_ \"foo\" ] [ text \"hello\" ]
-- @
--
-- Prefer the pre-built combinators from "Miso.Html.Element" (e.g. @div_@,
-- @p_@) over calling 'node' directly.
node
  :: Namespace
  -- ^ Element namespace ('HTML', 'SVG', or 'MATHML')
  -> MisoString
  -- ^ Tag name (e.g. @\"div\"@, @\"span\"@)
  -> [Attribute action]
  -- ^ Attributes and event handlers
  -> [View model action]
  -- ^ Child nodes
  -> View model action
node = VNode
-----------------------------------------------------------------------------
-- | Alias for 'node'.
--
-- Follows the @v-@ naming convention used elsewhere in the virtual DOM API
-- (cf. 'vtext', 'vfrag', 'vcomp').
vnode
  :: Namespace
  -- ^ Element namespace ('HTML', 'SVG', or 'MATHML')
  -> MisoString
  -- ^ Tag name
  -> [Attribute action]
  -- ^ Attributes and event handlers
  -> [View model action]
  -- ^ Child nodes
  -> View model action
vnode = node
-----------------------------------------------------------------------------
-- | Create a t'VText' node containing the given string.
--
-- On the server (SSR builds) the string is HTML-escaped automatically via
-- 'htmlEncode'.  In the browser the string is set as the text content of a
-- DOM text node, so no escaping is applied.
--
-- @
-- text "Hello, World!"  -- renders as the text node Hello, World!
-- @
text :: MisoString -> View model action
#ifdef SSR
text = VText Nothing . htmlEncode
#else
text = VText Nothing
#endif
-----------------------------------------------------------------------------
-- | Alias for 'text'.
--
-- Follows the @v-@ naming convention (cf. 'vnode', 'vfrag').
vtext :: MisoString -> View model action
vtext = text
----------------------------------------------------------------------------
-- | Create a t'VText' node whose content is __never__ HTML-escaped.
--
-- Unlike 'text', 'textRaw' skips 'htmlEncode' even in SSR builds.  Use this
-- only when you are certain the string does not contain characters that need
-- escaping (e.g. it has already been sanitised or it is guaranteed to be
-- plain ASCII with no @\<@, @\>@, @&@, @\"@, or @\'@ characters).
--
-- __Warning__: passing untrusted user input to 'textRaw' is an XSS vulnerability.
textRaw :: MisoString -> View model action
textRaw = VText Nothing
----------------------------------------------------------------------------
-- |
-- HTML-encodes text.
--
-- Useful for escaping HTML when delivering on the server. Naive usage
-- of 'text' will ensure this as well.
--
-- >>> Data.Text.IO.putStrLn $ text "<a href=\"\">"
-- &lt;a href=&quot;&quot;&gt;
htmlEncode :: MisoString -> MisoString
htmlEncode = MS.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  x -> MS.singleton x
-----------------------------------------------------------------------------
-- | Create a new v'VText' containing concatenation of the given strings.
--
-- @
--   view :: View model action
--   view = div_
--     [ className "container" ]
--     [ text_
--       [ "foo"
--       , "bar"
--       ]
--     ]
-- @
--
-- Renders as @<div class="container">foo bar</div>@
--
-- A single additional space is added between elements.
--
text_ :: [MisoString] -> View model action
text_ = VText Nothing . MS.intercalate " "
-----------------------------------------------------------------------------
-- | Like 'text', but allow the node to be keyed for efficient diffing.
--
-- @
-- view :: model -> View model action
-- view = \x -> div_ [] [ textKey (1 :: Int) "text here" ]
-- @
--
-- @since 1.9.0.0
textKey :: ToKey key => key -> MisoString -> View model action
textKey k = VText (Just (toKey k))
-----------------------------------------------------------------------------
-- | Like 'text_', but allow the node to be keyed for efficient diffing.
--
-- @
-- view :: model -> View model action
-- view = \x -> div_ [] [ textKey_ (1 :: Int) [ "text", "goes", "here" ] ]
-- @
--
-- @since 1.9.0.0
textKey_ :: ToKey key => key -> [MisoString] -> View model action
textKey_ k xs = VText (Just (toKey k)) (MS.intercalate " " xs)
-----------------------------------------------------------------------------
-- | Utility function to make it easy to specify conditional attributes
--
-- @
-- view :: Bool -> View model action
-- view danger = optionalAttrs div_ [ id_ "some-div" ] danger [ class_ "danger" ] ["child"]
-- @
--
-- @since 1.9.0.0
optionalAttrs
  :: ([Attribute action] -> [View model action] -> View model action)
  -> [Attribute action] -- ^ Attributes to be added unconditionally
  -> Bool -- ^ A condition
  -> [Attribute action] -- ^ Additional attributes to add if the condition is True
  -> [View model action] -- ^ Children
  -> View model action
optionalAttrs element attrs condition opts kids =
  case element attrs kids of
    VNode ns name _ _ -> do
      let newAttrs = concat [ opts | condition ] ++ attrs
      VNode ns name newAttrs kids
    x -> x
-----------------------------------------------------------------------------
-- | Utility function to make it easy to specify conditional attributes for void elements.
--
-- @
-- view :: Bool -> View model action
-- view shouldClear = optionalVoidAttrs textarea_ [ value_ "" ] shouldClear [ id_ "text-area-id" ]
-- @
--
-- @since 1.9.0.0
optionalVoidAttrs
  :: ([Attribute action] -> View model action)
  -> [Attribute action] -- ^ Attributes to be added unconditionally
  -> Bool -- ^ A condition
  -> [Attribute action] -- ^ Additional attributes to add if the condition is True
  -> View model action
optionalVoidAttrs element attrs condition opts =
  case element attrs of
    VNode ns name _ kids -> do
      let newAttrs = concat [ opts | condition ] ++ attrs
      VNode ns name newAttrs kids
    x -> x
----------------------------------------------------------------------------
-- | Conditionally adds children.
--
-- @
-- view :: Bool -> View model action
-- view withChild = optionalChildren div_ [ id_ "txt" ] [] withChild [ "foo" ]
-- @
--
-- @since 1.9.0.0
optionalChildren
  :: ([Attribute action] -> [View model action] -> View model action)
  -> [Attribute action] -- ^ Attributes to be added unconditionally
  -> [View model action] -- ^ Children to be added unconditionally
  -> Bool -- ^ A condition
  -> [View model action] -- ^ Additional children to add if the condition is True
  -> View model action
optionalChildren element attrs kids condition opts =
  case element attrs kids of
    VNode ns name _ _ -> do
      let newKids = kids ++ concat [ opts | condition ]
      VNode ns name attrs newKids
    x -> x
----------------------------------------------------------------------------
-- | A parsed URI following [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986).
--
-- Miso represents URIs in a structured way to make client-side routing easier.
-- The three fields correspond to the most commonly used URI components:
--
-- @
-- -- https://example.com/blog/post?page=2&amp;draft#section-1
-- URI
--   { uriPath        = \"blog\/post\"
--   , uriQueryString = M.fromList [(\"page\", Just \"2\"), (\"draft\", Nothing)]
--   , uriFragment    = \"#section-1\"
--   }
-- @
--
-- Use 'prettyURI' to serialise back to a 'MisoString' and 'emptyURI' as a
-- zero value.  t'URI' values are produced by the history subscription in
-- "Miso.Subscription.History".
data URI
  = URI
  { uriPath :: MisoString
  -- ^ The path component, without a leading slash (e.g. @\"blog\/post\"@).
  , uriFragment :: MisoString
  -- ^ The fragment identifier, including the leading @#@ (e.g. @\"#section-1\"@),
  --   or the empty string when absent.
  , uriQueryString :: M.Map MisoString (Maybe MisoString)
  -- ^ Query parameters as a map from key to optional value.
  --   'Nothing' represents a flag parameter (@?key@ with no @=value@);
  --   'Just v' represents a key-value pair (@?key=value@).
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSVal, ToObject)
----------------------------------------------------------------------------
-- | A t'URI' with empty path, fragment, and query string.
--
-- Useful as a starting point when constructing URIs programmatically:
--
-- @
-- myURI = emptyURI { uriPath = \"about\" }
-- @
emptyURI :: URI
emptyURI = URI mempty mempty mempty
----------------------------------------------------------------------------
instance ToMisoString URI where
  toMisoString = prettyURI
----------------------------------------------------------------------------
instance ToJSON URI where
  toJSON = toJSON . toMisoString
----------------------------------------------------------------------------
-- | Serialise a t'URI' to its string representation.
--
-- The output always starts with a leading slash, followed by the path, the
-- query string (if any), and the fragment (if any).
--
-- @
-- prettyURI (URI \"blog\/post\" \"#sec\" (M.fromList [(\"page\", Just \"2\")]))
--   == \"\/blog\/post?page=2#sec\"
-- @
prettyURI :: URI -> MisoString
prettyURI uri@URI {..} = "/" <> uriPath <> prettyQueryString uri <> uriFragment
-----------------------------------------------------------------------------
-- | Serialise only the query-string portion of a t'URI'.
--
-- Returns the empty string when 'uriQueryString' is empty.  Key-value pairs
-- are rendered as @?key=value@ and flag-only parameters as @?key@.
prettyQueryString :: URI -> MisoString
prettyQueryString URI {..} = queries <> flags
  where
    queries =
      MS.concat
      [ "?" <>
        MS.intercalate "&"
        [ k <> "=" <> v
        | (k, Just v) <- M.toList uriQueryString
        ]
      | any isJust (M.elems uriQueryString)
      ]
    flags = mconcat
        [ "?" <> k
        | (k, Nothing) <- M.toList uriQueryString
        ]
-----------------------------------------------------------------------------
-- | Mirrors the TypeScript @VTreeType@ enum used by the Miso runtime to
-- identify the kind of each node in the serialised virtual DOM.
--
-- The integer encoding is:
--
-- @
-- VCompType = 0
-- VNodeType = 1
-- VTextType = 2
-- VFragType = 3
-- @
data VTreeType
  = VCompType
  -- ^ Component node (t'VComp')
  | VNodeType
  -- ^ Element node (t'VNode')
  | VTextType
  -- ^ Text node (t'VText')
  | VFragType
  -- ^ Fragment node (t'VFrag')
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal VTreeType where
  toJSVal = \case
    VCompType -> toJSVal (0 :: Int)
    VNodeType -> toJSVal (1 :: Int)
    VTextType -> toJSVal (2 :: Int)
    VFragType -> toJSVal (3 :: Int)
-----------------------------------------------------------------------------
