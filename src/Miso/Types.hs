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
  , SomeComponent    (..)
  , Dynamic
  , View             (..)
  , Key              (..)
  , Attribute        (..)
  , NS               (..)
  , CSS              (..)
  , LogLevel         (..)
  , MountPoint
  -- ** Classes
  , ToView           (..)
  , ToKey            (..)
  -- ** Smart Constructors
  , defaultComponent
  -- ** Components
  , component_
  -- ** Utils
  , getMountPoint
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson (Value, ToJSON)
import           Data.JSString (JSString)
import           Data.Kind (Type)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.String (IsString, fromString)
import qualified Data.Text as T
import           Data.Proxy (Proxy(Proxy))
import           Language.Javascript.JSaddle (ToJSVal(toJSVal), Object, JSM)
import           Prelude hiding (null)
import           GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import           Servant.API (HasLink(MkLink, toLink))
-----------------------------------------------------------------------------
import           Miso.Effect (Effect, Sub, Sink)
import           Miso.Event.Types
import           Miso.String (MisoString, toMisoString, ms)
-----------------------------------------------------------------------------
-- | Application entry point
data Component (name :: Symbol) model action = Component
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
  , initialAction :: Maybe action
  -- ^ Initial action that is run after the application has loaded, optional
  --
  -- @since 1.9.0.0
  , mountPoint :: Maybe MountPoint
  -- ^ Id of the root element for DOM diff.
  -- If 'Nothing' is provided, the entire document body is used as a mount point.
  , logLevel :: LogLevel
  -- ^ Debugging for prerendering and event delegation
  }
-----------------------------------------------------------------------------
-- | @mountPoint@ for @Component@, e.g "body"
type MountPoint = MisoString
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
-- | Smart constructor for @Component@ with sane defaults.
defaultComponent
  :: model
  -> (action -> Effect model action)
  -> (model -> View action)
  -> Component name model action
defaultComponent m u v = Component
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
  -- ^ No debug logging, the default value used in @defaultComponent@
  | DebugHydrate
  -- ^ Will warn if the structure or properties of the
  -- DOM vs. Virtual DOM differ during prerendering.
  | DebugEvents
  -- ^ Will warn if an event cannot be routed to the Haskell event
  -- handler that raised it. Also will warn if an event handler is
  -- being used, yet it's not being listened for by the event
  -- delegator mount point.
  | DebugNotify
  -- ^ Will warn if a @Component@ can't be found when using @notify@ or @notify'@
  | DebugAll
  -- ^ Logs on all of the above
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Core type for constructing a virtual DOM in Haskell
data View action
  = VNode NS MisoString [Attribute action] [View action]
  | VText MisoString
  | VTextRaw MisoString
  | VComp MisoString [Attribute action] SomeComponent
  deriving Functor
-----------------------------------------------------------------------------
-- | Existential wrapper used to allow the nesting of @Component@ in @Component@
data SomeComponent
   = forall name model action . Eq model
  => SomeComponent (Component name model action)
-----------------------------------------------------------------------------
-- | Used in the @view@ function to embed an @Component@ into another @Component@
-- Use this function if you'd like send messages to this @Component@ at @name@ via
-- @notify@ or to read the state of this @Component@ via @sample@.
component_
  :: forall name model action a . (Eq model, KnownSymbol name)
  => Component name model action
  -> [Attribute a]
  -> View a
component_ app attrs = VComp (ms name) attrs (SomeComponent app)
  where
    name = symbolVal (Proxy @name)
-----------------------------------------------------------------------------
-- | Type synonym for Dynamically constructed @Component@
-- @
-- sampleComponent :: Component Dynamic Model Action
-- @
type Dynamic = ""
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
instance ToView (Component name model action) where
  type ToViewAction (Component name model action) = action
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
  | Event (Sink action -> Object -> LogLevel -> Events -> JSM ())
  | Styles (M.Map MisoString MisoString)
  deriving Functor
-----------------------------------------------------------------------------
-- | @IsString@ instance
instance IsString (View a) where
  fromString = VText . fromString
-----------------------------------------------------------------------------
