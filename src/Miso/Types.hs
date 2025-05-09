{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
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
    App              (..)
  , Component        (..)
  , View             (..)
  , Key              (..)
  , Attribute        (..)
  , NS               (..)
  , CSS              (..)
  , LogLevel         (..)
  -- ** Classes
  , ToView           (..)
  , ToKey            (..)
  -- ** Functions
  , defaultApp
  , component
  , component_
  , componentWith
  , componentWith_
  , getMountPoint
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson (Value)
import           Data.JSString (JSString)
import           Data.Kind (Type)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.String (IsString, fromString)
import qualified Data.Text as T
import           Data.Proxy (Proxy(Proxy))
import           Language.Javascript.JSaddle (ToJSVal(toJSVal), Object, JSM)
import           Prelude hiding (null)
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import           Servant.API (HasLink(MkLink, toLink))
-----------------------------------------------------------------------------
import           Miso.Effect (Effect, Sub, Sink)
import           Miso.Event.Types
import           Miso.String (MisoString, toMisoString, ms)
-----------------------------------------------------------------------------
-- | Application entry point
data App (name :: Symbol) model action = App
  { model :: model
  -- ^ initial model
  , update :: action -> Effect model action
  -- ^ Function to update model, optionally providing effects.
  --   See the 'Transition' monad for succinctly expressing model transitions.
  , view :: model -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub action ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: M.Map MisoString Capture
  -- ^ List of delegated events that the body element will listen for.
  --   You can start with 'Miso.Event.Types.defaultEvents' and modify as needed.
  , styles :: [CSS]
  -- ^ List of CSS styles expressed as either a URL ('Href') or as 'Style' text.
  -- These styles are appended dynamically to the <head> section of your HTML page
  -- before the initial draw on <body> occurs.
  , initialAction :: Maybe action
  -- ^ Initial action that is run after the application has loaded, optional
  --
  -- @since 1.9.0.0
  , mountPoint :: Maybe MisoString
  -- ^ Id of the root element for DOM diff.
  -- If 'Nothing' is provided, the entire document body is used as a mount point.
  , logLevel :: LogLevel
  -- ^ Debugging for prerendering and event delegation
  }
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
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Convenience for extracting mount point
getMountPoint :: Maybe MisoString -> MisoString
getMountPoint = fromMaybe "body"
-----------------------------------------------------------------------------
-- | Smart constructor for @App@ with sane defaults.
defaultApp
  :: model
  -> (action -> Effect model action)
  -> (model -> View action)
  -> App name model action
defaultApp m u v = App
  { model = m
  , update = u
  , view = v
  , subs = []
  , events = defaultEvents
  , styles = []
  , mountPoint = Nothing
  , logLevel = Off
  , initialAction = Nothing
  }
-----------------------------------------------------------------------------
-- | Optional Logging for debugging miso internals (useful to see if prerendering is successful)
data LogLevel
  = Off
  | DebugPrerender
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
  = Node NS MisoString (Maybe Key) [Attribute action] [View action]
  | Text MisoString
  | TextRaw MisoString
  | VComp MisoString [Attribute action] (Maybe Key) Component
  deriving Functor
-----------------------------------------------------------------------------
-- | Existential wrapper used to allow the nesting of @App@ in @App@
data Component
   = forall name model action . Eq model
  => Component (App name model action)
-----------------------------------------------------------------------------
-- | Used in the @view@ function to embed an @App@ into another @App@
-- Use this function if you'd like send messages to this @App@ at @name@ via
-- @notify@ or to read the state of this @App@ via @sample@.
component
  :: forall name model action a . (Eq model, KnownSymbol name)
  => App name model action
  -> View a
component app = VComp (ms name) [] Nothing (Component app)
  where
    name = symbolVal (Proxy @name)
-----------------------------------------------------------------------------
-- | Like @component@, but uses a dynamically generated @name@ (enforced via @Component@).
-- The component name is dynamically generated at runtime and available via 'ask'.
-- This is for dynamic component creation, where a mounted @App@ isn't necessarily
-- statically known. Use this during circumstances where a parent would like
-- to dynamically generate / destroy n-many children in response to user input.
component_
  :: Component
  -> View a
component_ = VComp mempty [] Nothing
-----------------------------------------------------------------------------
-- | Like @component@ except it allows the specification of @Key@
-- and @Attribute action@.
componentWith
  :: forall name model action a . (Eq model, KnownSymbol name)
  => App name model action
  -> Maybe Key
  -> [Attribute a]
  -> View a
componentWith app key attrs = VComp (ms name) attrs key (Component app)
  where
    name = symbolVal (Proxy @name)
-----------------------------------------------------------------------------
-- | Like @component_@ except it allows the specification of @Key@
-- and @Attribute action@. Note: the @name@ parameter is ignored here.
componentWith_
  :: Component
  -> Maybe Key
  -> [Attribute a]
  -> View a
componentWith_ someApp key attrs = VComp mempty attrs key someApp
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
instance ToView (App name model action) where
  type ToViewAction (App name model action) = action
  toView App {..} = toView (view model)
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
  deriving (Show, Eq, IsString)
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
  | Event (Sink action -> Object -> LogLevel -> Events -> JSM ())
  | Styles (M.Map MisoString MisoString)
  deriving Functor
-----------------------------------------------------------------------------
-- | @IsString@ instance
instance IsString (View a) where
  fromString = Text . fromString
-----------------------------------------------------------------------------
