-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds                 #-}
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
  , View             (..)
  , Key              (..)
  , Attribute        (..)
  , NS               (..)
  , LogLevel         (..)
  , Component        (..)
  , SomeComponent    (..)
  -- ** Classes
  , ToView           (..)
  , ToKey            (..)
  -- ** Functions
  , defaultApp
  , component
  , embed
  , embedKeyed
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
import           Language.Javascript.JSaddle (ToJSVal(toJSVal), Object, JSM)
import           Prelude hiding (null)
import           Servant.API (HasLink(MkLink, toLink))
-----------------------------------------------------------------------------
import           Miso.Effect (Effect, Sub, Sink)
import           Miso.Event.Types
import           Miso.String (MisoString, toMisoString)
-----------------------------------------------------------------------------
-- | Application entry point
data App model action = App
  { model :: model
  -- ^ initial model
  , update :: action -> model -> Effect action model
  -- ^ Function to update model, optionally providing effects.
  --   See the @Transition@ monad for succinctly expressing model transitions.
  , view :: model -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub action ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: M.Map MisoString Bool
  -- ^ List of delegated events that the body element will listen for.
  --   You can start with 'Miso.Event.Types.defaultEvents' and modify as needed.
  , initialAction :: Maybe action
  -- ^ Initial action that is run after the application has loaded, optional since *1.9*
  , mountPoint :: Maybe MisoString
  -- ^ Id of the root element for DOM diff.
  -- If 'Nothing' is provided, the entire document body is used as a mount point.
  , logLevel :: LogLevel
  }
-----------------------------------------------------------------------------
-- | Convenience for extracting mount point
getMountPoint :: Maybe MisoString -> MisoString
getMountPoint = fromMaybe "body"
-----------------------------------------------------------------------------
-- | Smart constructor for @App@ with sane defaults.
defaultApp
  :: model
  -> (action -> model -> Effect action model)
  -> (model -> View action)
  -> App model action
defaultApp m u v = App
  { model = m
  , view = v
  , update = u
  , subs = []
  , events = defaultEvents
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
  | Embed [Attribute action] SomeComponent
  deriving Functor
-----------------------------------------------------------------------------
-- | Existential wrapper used to allow the nesting of @Component@ in @App@
data SomeComponent
   = forall model action . Eq model
  => SomeComponent (Component model action)
-----------------------------------------------------------------------------
-- | Used with @component@ to parameterize @App@ by @name@
data Component model action
  = Component
  { componentKey :: Maybe Key
  , componentName :: MisoString
  , componentApp :: App model action
  }
-----------------------------------------------------------------------------
-- | Smart constructor for parameterizing @App@ by @name@
-- Needed when calling @embed@ and @embedWith@
component
  :: forall model action
   . MisoString  
  -> App model action
  -> Component model action
component = Component Nothing
-----------------------------------------------------------------------------
-- | Used in the @view@ function to @embed@ @Component@s in @App@
embed :: Eq model => Component model a -> [Attribute action] -> View action
embed comp attrs = Embed attrs (SomeComponent comp)
-----------------------------------------------------------------------------
-- | Used in the @view@ function to @embed@ @Component@s in @App@, with @Key@
embedKeyed :: Eq model => Component model a -> Key -> [Attribute action] -> View action
embedKeyed comp key attrs = Embed attrs $ SomeComponent comp { componentKey = Just key }
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
  toView (Component _ _ app) = toView app
-----------------------------------------------------------------------------
instance ToView (App model action) where
  type ToViewAction (App model action) = action
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
  | Style (M.Map MisoString MisoString)
  deriving Functor
-----------------------------------------------------------------------------
-- | @IsString@ instance
instance IsString (View a) where
  fromString = Text . fromString
-----------------------------------------------------------------------------
