-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Types
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Types
  ( -- ** Types
    Component        (..)
  , ComponentId
  , SomeComponent    (..)
  , View             (..)
  , Key              (..)
  , Attribute        (..)
  , NS               (..)
  , CSS              (..)
  , JS               (..)
  , LogLevel         (..)
  , VTree            (..)
  , MountPoint
  , DOMRef
  -- ** Classes
  , ToView           (..)
  , ToKey            (..)
  -- ** Smart Constructors
  , component
  -- ** Component
  , mount
  , (+>)
  -- ** Utils
  , getMountPoint
  -- *** Combinators
  , node
  , text
  , textRaw
  , rawHtml
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson (Value, ToJSON)
import           Data.JSString (JSString)
import           Data.Kind (Type)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.String (IsString, fromString)
import qualified Data.Text as T
import           Language.Javascript.JSaddle (ToJSVal(toJSVal), Object(..), JSM)
import           Prelude hiding (null)
import           Servant.API (HasLink(MkLink, toLink))
-----------------------------------------------------------------------------
import           Miso.Concurrent (Mail)
import           Miso.Effect (Effect, Sub, Sink, DOMRef)
import           Miso.Event.Types
import           Miso.String (MisoString, toMisoString)
import           Miso.Style.Types (StyleSheet)
-----------------------------------------------------------------------------
-- | Application entry point
data Component model action
  = Component
  { model :: model
  -- ^ initial model
  , update :: action -> Effect model action
  -- ^ Function to update model, optionally providing effects.
  , view :: model -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub action ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: Events
  -- ^ List of delegated events that the body element will listen for.
  --   You can start with 'Miso.Event.Types.defaultEvents' and modify as needed.
  , styles :: [CSS]
  -- ^ List of CSS styles expressed as either a URL ('Href') or as 'Style' text.
  -- These styles are appended dynamically to the <head> section of your HTML page
  -- before the initial draw on <body> occurs.
  --
  -- @since 1.9.0.0
  , scripts :: [JS]
  -- ^ List of JavaScript <scripts> expressed as either a URL ('Src') or raw JS text.
  -- These scripts are appended dynamically to the <head> section of your HTML page
  -- before the initial draw on <body> occurs.
  --
  -- @since 1.9.0.0
  , initialAction :: Maybe action
  -- ^ Initial action that is run after the application has loaded, optional
  --
  -- @since 1.9.0.0
  , mountPoint :: Maybe MountPoint
  -- ^ Id of the root element for DOM diff.
  -- If 'Nothing' is provided, the entire document body is used as a mount point.
  , logLevel :: LogLevel
  -- ^ Debugging for prerendering and event delegation
  , mailbox :: Mail -> Maybe action
  -- ^ Used to receive mail from other 'Component'
  --
  -- @since 1.9.0.0
  }
-----------------------------------------------------------------------------
-- | @mountPoint@ for @Component@, e.g "body"
type MountPoint = MisoString
-----------------------------------------------------------------------------
-- | ID for 'Component'
type ComponentId = Int
-----------------------------------------------------------------------------
-- | Allow users to express CSS and append it to <head> before the first draw
--
-- > Href "http://domain.com/style.css
--
data CSS
  = Href MisoString
  -- ^ 'Href' is a URL meant to link to hosted CSS
  | Style MisoString
  -- ^ 'Style' is meant to be raw CSS in a 'style_' tag
  | Sheet StyleSheet
  -- ^ 'Sheet' is meant to be CSS built with 'Miso.Style'
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Allow users to express JS and append it to <head> before the first draw
--
-- This is meant to be useful in development only.
--
-- @
--   Src "http://example.com/script.js
--   Script "http://example.com/script.js
-- @
--
data JS
  = Src MisoString
  -- ^ 'src' is a URL meant to link to hosted JS
  | Script MisoString
  -- ^ 'script' is meant to be raw JS in a 'script' tag
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Convenience for extracting mount point
getMountPoint :: Maybe MisoString -> MisoString
getMountPoint = fromMaybe "body"
-----------------------------------------------------------------------------
-- | Smart constructor for @Component@ with sane defaults.
component
  :: model
  -> (action -> Effect model action)
  -> (model -> View action)
  -> Component model action
component m u v = Component
  { model = m
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
  }
-----------------------------------------------------------------------------
-- | Optional Logging for debugging miso internals (useful to see if prerendering is successful)
data LogLevel
  = Off
  -- ^ No debug logging, the default value used in @component@
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
data View action
  = VNode NS MisoString [Attribute action] [View action]
  | VText MisoString
  | VTextRaw MisoString
  | VComp NS MisoString [Attribute action] SomeComponent
  deriving Functor
-----------------------------------------------------------------------------
-- | Existential wrapper used to allow the nesting of @Component@ in @Component@
data SomeComponent
   = forall model action . Eq model
  => SomeComponent (Component model action)
-----------------------------------------------------------------------------
-- | Used in the @view@ function to mount a 'Component' on any 'VNode'
--
-- @
--   mount_ (p_ [ key_ "component-1" ]) $ component $ \\m ->
--     div_ [ id_ "foo" ] [ text (ms m) ]
-- @
--
-- Warning this *is* a partial function. Do not attempt to mount on a
-- Text node. This function will ignore the children given and mount the
-- new 'Component' on top of them. Attempts to mount a 'Component' ontop of an
-- existing 'Component' always prioritize the component specified in the lowest
-- level.
--
-- See usage above. In general, it's wise to only mount on `VNode`.
--
-- @since 1.9.0.0
mount
  :: forall model action a . Eq model
  => ([Miso.Types.View action] -> Miso.Types.View a)
  -> Component model action
  -> View a
mount mkNode vcomp =
  case mkNode [] of
    VNode ns tag attrs _ ->
      VComp ns tag attrs
        (SomeComponent vcomp)
    VComp ns tag attrs vcomp_ ->
      VComp ns tag attrs vcomp_
    _ ->
      error "Cannot mount on a Text node"
-----------------------------------------------------------------------------
(+>)
  :: forall model action a . Eq model
  => ([Miso.Types.View action] -> Miso.Types.View a)
  -> Component model action
  -> View a
infixr 0 +>
(+>) = mount
-----------------------------------------------------------------------------
-- | For constructing type-safe links
instance HasLink (View a) where
  type MkLink (View a) b = b
  toLink x _ = x
-----------------------------------------------------------------------------
-- | Convenience class for using View
class ToView a where
  type ToViewAction a :: Type
  toView :: a -> View (ToViewAction a)
-----------------------------------------------------------------------------
instance ToView (View action) where
  type ToViewAction (View action) = action
  toView = id
-----------------------------------------------------------------------------
instance ToView (Component model action) where
  type ToViewAction (Component model action) = action
  toView Component {..} = toView (view model)
-----------------------------------------------------------------------------
-- | Namespace of DOM elements.
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  | MATHML  -- ^ MATHML Namespace
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSVal NS where
  toJSVal SVG  = toJSVal ("svg" :: JSString)
  toJSVal HTML = toJSVal ("html" :: JSString)
  toJSVal MATHML = toJSVal ("mathml" :: JSString)
-----------------------------------------------------------------------------
-- | A unique key for a dom node.
--
-- This key is only used to speed up diffing the children of a DOM
-- node, the actual content is not important. The keys of the children
-- of a given DOM node must be unique. Failure to satisfy this
-- invariant gives undefined behavior at runtime.
newtype Key = Key MisoString
  deriving (Show, Eq, IsString, ToJSON)
-----------------------------------------------------------------------------
-- | ToJSVal instance for Key
instance ToJSVal Key where
  toJSVal (Key x) = toJSVal x
-----------------------------------------------------------------------------
-- | Convert custom key types to @Key@.
--
-- Instances of this class do not have to guarantee uniqueness of the
-- generated keys, it is up to the user to do so. @toKey@ must be an
-- injective function.
class ToKey key where
  -- | Converts any key into @Key@
  toKey :: key -> Key
-----------------------------------------------------------------------------
-- | Identity instance
instance ToKey Key where toKey = id
-----------------------------------------------------------------------------
-- | Convert @MisoString@ to @Key@
instance ToKey JSString where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert @T.Text@ to @Key@
instance ToKey T.Text where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert @String@ to @Key@
instance ToKey String where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert @Int@ to @Key@
instance ToKey Int where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert @Double@ to @Key@
instance ToKey Double where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert @Float@ to @Key@
instance ToKey Float where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Convert @Word@ to @Key@
instance ToKey Word where toKey = Key . toMisoString
-----------------------------------------------------------------------------
-- | Attribute of a vnode in a @View@.
--
-- The @Sink@ callback can be used to dispatch actions which are fed back to
-- the @update@ function. This is especially useful for event handlers
-- like the @onclick@ attribute. The second argument represents the
-- vnode the attribute is attached to.
data Attribute action
  = Property MisoString Value
  | Event (Sink action -> VTree -> LogLevel -> Events -> JSM ())
  | Styles (M.Map MisoString MisoString)
  deriving Functor
-----------------------------------------------------------------------------
-- | @IsString@ instance
instance IsString (View a) where
  fromString = VText . fromString
-----------------------------------------------------------------------------
-- | Virtual DOM implemented as a JavaScript `Object`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree = VTree { getTree :: Object }
-----------------------------------------------------------------------------  
instance ToJSVal VTree where
  toJSVal (VTree (Object vtree)) = pure vtree
-----------------------------------------------------------------------------
-- | Create a new @Miso.Types.TextRaw@.
--
-- @expandable@
-- a 'rawHtml' node takes raw HTML and attempts to convert it to a 'VTree'
-- at runtime. This is a way to dynamically populate the virtual DOM from
-- HTML received at runtime. If rawHtml cannot parse the HTML it will not render.
rawHtml
  :: MisoString
  -> View action
rawHtml = VTextRaw
-----------------------------------------------------------------------------
-- | Create a new @Miso.Types.VNode@.
--
-- @node ns tag key attrs children@ creates a new node with tag @tag@
-- and 'Key' @key@ in the namespace @ns@. All @attrs@ are called when
-- the node is created and its children are initialized to @children@.
node :: NS
     -> MisoString
     -> [Attribute action]
     -> [View action]
     -> View action
node = VNode
-----------------------------------------------------------------------------
-- | Create a new @Text@ with the given content.
text :: MisoString -> View action
text = VText
-----------------------------------------------------------------------------
-- | `TextRaw` creation. Don't use directly
textRaw :: MisoString -> View action
textRaw = VTextRaw
-----------------------------------------------------------------------------
