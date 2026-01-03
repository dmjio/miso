-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Types
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Core types for Miso applications.
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
  , NS            (..)
  , CSS           (..)
  , JS            (..)
  , LogLevel      (..)
  , VTree         (..)
  , VTreeType     (..)
  , MountPoint
  , DOMRef
  , ROOT
  , Transition
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
  , (-->)
  , (<--)
  , (<-->)
  , (<--->)
  , (--->)
  , (<---)
  -- ** Component mounting
  , (+>)
  , mount
  , mount_
  -- ** Utils
  , getMountPoint
  , optionalAttrs
  , optionalChildren
  , prettyURI
  , prettyQueryString
  -- *** Combinators
  , node
  , text
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
import           Data.Aeson (Value, ToJSON(..))
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust)
import           Data.String (IsString, fromString)
import qualified Data.Text as T
import           Miso.DSL
import           Prelude
-----------------------------------------------------------------------------
import           Miso.Binding ((<--), (-->), (<-->), (<---), (--->), (<--->), Binding(..))
import           Miso.Concurrent (Mail)
import           Miso.Effect (Effect, Sub, Sink, DOMRef, ComponentId)
import           Miso.Event.Types
import qualified Miso.String as MS
import           Miso.String (ToMisoString, MisoString, toMisoString, ms, fromMisoString)
import           Miso.CSS.Types (StyleSheet)
-----------------------------------------------------------------------------
-- | Application entry point
data Component parent model action
  = Component
  { model :: model
  -- ^ Initial model
#ifdef SSR
  , hydrateModel :: Maybe (IO model)
#else
  , hydrateModel :: Maybe (IO model)
#endif
  -- ^ Action to load component state, such as reading data from page.
  --   The resulting model is only used during initial hydration, not on remounts.
  , update :: action -> Effect parent model action
  -- ^ Updates model, optionally providing effects.
  , view :: model -> View model action
  -- ^ Draws 'View'
  , subs :: [ Sub action ]
  -- ^ Subscriptions to run during application lifetime
  , events :: Events
  -- ^ Delegated events that the body element will listen for.
  --   You can start with 'Miso.Event.Types.defaultEvents' and modify as needed.
  , styles :: [CSS]
  -- ^ CSS styles expressed as either a URL ('Href') or as 'Style' text.
  -- These styles are appended dynamically to the \<head\> section of your HTML page
  -- before the initial draw on \<body\> occurs.
  --
  -- @since 1.9.0.0
  , scripts :: [JS]
  -- ^ JavaScript scripts expressed as either a URL ('Src') or raw JS text.
  -- These scripts are appended dynamically to the \<head\> section of your HTML page
  -- before the initial draw on \<body\> occurs.
  --
  -- @since 1.9.0.0
  , initialAction :: Maybe action
  -- ^ Initial action run after the application has loaded, optional
  --
  -- @since 1.9.0.0
  , mountPoint :: Maybe MountPoint
  -- ^ ID of the root element for DOM diff.
  -- If 'Nothing' is provided, the entire document body is used as a mount point.
  , logLevel :: LogLevel
  -- ^ Debugging configuration for prerendering and event delegation
  , mailbox :: Mail -> Maybe action
  -- ^ Receives mail from other components
  --
  -- @since 1.9.0.0
  , bindings :: [ Binding parent model ]
  -- ^ Data bindings between parent and child t'Miso.Types.Component's
  --
  -- @since 1.9.0.0
  , eventPropagation :: Bool
  -- ^ Should events bubble up past the t'Miso.Types.Component' barrier.
  --
  -- Defaults to False
  --
  -- @since 1.9.0.0
  }
-----------------------------------------------------------------------------
-- | @mountPoint@ for t'Miso.Types.Component', e.g "body"
type MountPoint = MisoString
-----------------------------------------------------------------------------
-- | Allow users to express CSS and append it to \<head\> before the first draw
--
-- > Href "http://domain.com/style.css"
-- > Style "body { background-color: red; }"
--
data CSS
  = Href MisoString
  -- ^ URL linking to hosted CSS
  | Style MisoString
  -- ^ Raw CSS content in a 'Miso.Html.Element.style_' tag
  | Sheet StyleSheet
  -- ^ CSS built with 'Miso.CSS'
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Allow users to express JS and append it to <head> before the first draw
--
-- This is meant to be useful in development only.
--
-- @
--   Src \"http:\/\/example.com\/script.js\"
--   Script "alert(\"hi\");"
--   ImportMap [ "key" =: "value" ]
-- @
--
data JS
  = Src MisoString
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
-- | Convenience for extracting mount point
getMountPoint :: Maybe MisoString -> MisoString
getMountPoint = fromMaybe "body"
-----------------------------------------------------------------------------
-- | Smart constructor for t'Miso.Types.Component' with sane defaults.
component
  :: model
  -> (action -> Effect parent model action)
  -> (model -> View model action)
  -> Component parent model action
component m u v = Component
  { model = m
  , hydrateModel = Nothing
  , update = u
  , view = v
  , subs = []
  , events = defaultEvents
  , styles = []
  , scripts = []
  , mountPoint = Nothing
  , logLevel = Off
  , initialAction = Nothing
  , mailbox = const Nothing
  , bindings = []
  , eventPropagation = False
  }
-----------------------------------------------------------------------------
-- | A top-level t'Miso.Types.Component' can have no @parent@.
--
-- The 'ROOT' type is for disallowing a top-level mounted t'Miso.Types.Component' access
-- into its parent state. It has no inhabitants (spiritually 'Data.Void.Void')
--
data ROOT
-----------------------------------------------------------------------------
-- | 'Eq' instance for 'ROOT'
instance Eq ROOT where _ == _ = True
-----------------------------------------------------------------------------
-- | A miso application is a top-level t'Miso.Types.Component', which has no @parent@.
-- This is enforced by specializing the @parent@ type parameter to 'ROOT'.
--
type App model action = Component ROOT model action
-----------------------------------------------------------------------------
-- | A specialized version of 'Effect' that can be used in the type of application 'update' function,
-- when t'Miso.Types.Component's are not in use. Also for pre-1.9 'Miso.miso' applications.
type Transition model action = Effect ROOT model action
-----------------------------------------------------------------------------
-- | Logging configuration for debugging Miso internals (useful to see if prerendering is successful)
data LogLevel
  = Off
  -- ^ No debug logging, the default value used in 'component'
  | DebugHydrate
  -- ^ Will warn if the structure or properties of the
  -- DOM vs. Virtual DOM differ during prerendering.
  | DebugEvents
  -- ^ Will warn if an event cannot be routed to the Haskell event
  -- handler that raised it. Also will warn if an event handler is
  -- being used, yet it's not being listened for by the event
  -- delegator mount point.
  | DebugAll
  -- ^ Logs on all of the above
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Core type for constructing a virtual DOM in Haskell
data View model action
  = VNode NS MisoString [Attribute action] [View model action]
  | VText (Maybe Key) MisoString
  | VComp [Attribute action] (SomeComponent model)
  deriving Functor
-----------------------------------------------------------------------------
-- | Existential wrapper allowing nesting of t'Miso.Types.Component' in t'Miso.Types.Component'
data SomeComponent parent
   = forall model action . Eq model
  => SomeComponent (Component parent model action)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' mounting combinator
--
-- Used in the @view@ function to mount a t'Miso.Types.Component' on any 'VNode'.
--
-- @
--   "component-id" +> component model noop $ \\m ->
--     div_ [ id_ "foo" ] [ text (ms m) ]
-- @
--
-- @since 1.9.0.0
(+>)
  :: forall child model action a . Eq child
  => MisoString
  -> Component model child action
  -> View model a
infixr 0 +>
key +> vcomp = VComp [ Property "key" (toJSON key) ] (SomeComponent vcomp)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' mounting combinator. Takes '[Attribute a]' as arguments.
--
-- @
--   mount_ [ key_ "foo", onMounted Mounted ] $ component model noop $ \\m ->
--     div_ [ id_ "foo" ] [ text (ms m) ]
-- @
--
-- @since 1.9.0.0
mount_
  :: Eq m
  => [Attribute action]
  -> Component p m a
  -> View p action
mount_ attrs vcomp = VComp attrs (SomeComponent vcomp)
-----------------------------------------------------------------------------
-- | t'Miso.Types.Component' mounting combinator.
--
-- Note: only use this if you're certain you won't be diffing two t'Miso.Types.Component'
-- against each other. Otherwise, you will need a key to distinguish between
-- the two t'Miso.Types.Component', to ensure unmounting and mounting occurs.
--
-- @
--   mount $ component model noop $ \\m ->
--     div_ [ id_ "foo" ] [ text (ms m) ]
-- @
--
-- @since 1.9.0.0
mount
  :: Eq m
  => Component p m a
  -> View p action
mount = mount_ []
-----------------------------------------------------------------------------
-- | DOM element namespace.
data NS
  = HTML
  -- ^ HTML Namespace
  | SVG
  -- ^ SVG Namespace
  | MATHML
  -- ^ MATHML Namespace
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal NS where
  toJSVal = \case
    SVG -> toJSVal ("svg" :: MisoString)
    HTML -> toJSVal ("html" :: MisoString)
    MATHML -> toJSVal ("mathml" :: MisoString)
-----------------------------------------------------------------------------
-- | Unique key for a DOM node.
--
-- This key is only used to speed up diffing the children of a DOM
-- node, the actual content is not important. The keys of the children
-- of a given DOM node must be unique. Failure to satisfy this
-- invariant gives undefined behavior at runtime.
newtype Key = Key MisoString
  deriving (Show, Eq, IsString, ToJSON, ToMisoString)
-----------------------------------------------------------------------------
-- | ToJSVal instance for t'Key'
instance ToJSVal Key where
  toJSVal (Key x) = toJSVal x
-----------------------------------------------------------------------------
-- | Convert custom key types to t'Key'.
--
-- Instances of this class do not have to guarantee uniqueness of the
-- generated keys, it is up to the user to do so. @toKey@ must be an
-- injective function (different inputs must map to different outputs).
class ToKey key where
  -- | Converts any key into t'Key'
  toKey :: key -> Key
-----------------------------------------------------------------------------
-- | Identity instance
instance ToKey Key where toKey = id
-----------------------------------------------------------------------------
#ifndef VANILLA
-----------------------------------------------------------------------------
-- | Convert 'MisoString' to t'Key'
instance ToKey MisoString where toKey = Key . toMisoString
-----------------------------------------------------------------------------
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
-- | Attribute of a vnode in a t'View'.
--
data Attribute action
  = Property MisoString Value
  | ClassList [MisoString]
  | On (Sink action -> VTree -> LogLevel -> Events -> IO ())
  -- ^ The @Sink@ callback can be used to dispatch actions which are fed back to
  -- the @update@ function. This is especially useful for event handlers
  -- like the @onclick@ attribute. The second argument represents the
  -- vnode the attribute is attached to.
  | Styles (M.Map MisoString MisoString)
  deriving Functor
-----------------------------------------------------------------------------
-- | 'IsString' instance
instance IsString (View model action) where
  fromString = VText Nothing . fromString
-----------------------------------------------------------------------------
-- | Virtual DOM implemented as a JavaScript t'Object'.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see t'Miso.Types.View' instead.
newtype VTree = VTree { getTree :: Object }
  deriving (ToObject, ToJSVal)
-----------------------------------------------------------------------------
-- | Create a new 'Miso.Types.VNode'.
--
-- @node ns tag attrs children@ creates a new node with tag @tag@
-- in the namespace @ns@. All @attrs@ are called when
-- the node is created and its children are initialized to @children@.
node
  :: NS
  -> MisoString
  -> [Attribute action]
  -> [View model action]
  -> View model action
node = VNode
-----------------------------------------------------------------------------
-- | Create a new v'VText' with the given content.
text :: MisoString -> View model action
#ifdef SSR
text = VText Nothing . htmlEncode
#else
text = VText Nothing
#endif
----------------------------------------------------------------------------
-- | Create a new v'VText', not subject to HTML escaping.
--
-- Like 'text', except will not escape HTML when used on the server.
--
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
-- view danger = optionalAttrs textarea_ [ id_ "txt" ] danger [ class_ "danger" ] ["child"]
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
-- | URI type. See the official [specification](https://www.rfc-editor.org/rfc/rfc3986)
--
data URI
  = URI
  { uriPath, uriFragment :: MisoString
  , uriQueryString :: M.Map MisoString (Maybe MisoString)
  } deriving (Show, Eq)
----------------------------------------------------------------------------
-- | Empty t'URI'.
emptyURI :: URI
emptyURI = URI mempty mempty mempty
----------------------------------------------------------------------------
instance ToMisoString URI where
  toMisoString = prettyURI
----------------------------------------------------------------------------
-- | Pretty-prints a t'URI'.
prettyURI :: URI -> MisoString
prettyURI uri@URI {..} = "/" <> uriPath <> prettyQueryString uri <> uriFragment
-----------------------------------------------------------------------------
-- | Pretty-prints a t'URI' query string.
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
-- | VTreeType ADT for matching TypeScript enum
data VTreeType
  = VCompType
  | VNodeType
  | VTextType
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal VTreeType where
  toJSVal = \case 
    VCompType -> toJSVal (0 :: Int)
    VNodeType -> toJSVal (1 :: Int)
    VTextType -> toJSVal (2 :: Int)
-----------------------------------------------------------------------------
