{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE LambdaCase                #-}
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
  ( -- * App
    App              (..)
  , defaultApp
  -- * View
  , View             (..)
  , ToView           (..)
  -- * Key
  , Key              (..)
  , toKey
  -- * Attribute
  , Attribute        (..)
  , NS               (..)
  , LogLevel         (..)
  -- * Components
  , Component        (..)
  , SomeComponent    (..)
  , ComponentOptions (..)
  , component
  , embed
  , embedWith
  , getMountPoint
  , componentOptions
    -- * The Transition Monad
  , Transition
  , mapAction
  , fromTransition
  , toTransition
  , scheduleIO
  , scheduleIO_
  , scheduleIOFor_
  , scheduleSub
  -- * The Effect Monad
  , module Miso.Effect
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(StateT), execStateT, mapStateT)
import           Control.Monad.Trans.Writer.Strict (Writer, tell, mapWriter, runWriter)
import           Data.Aeson (Value)
import           Data.Bifunctor (second, Bifunctor(..))
import           Data.Foldable (for_)
import           Data.JSString (JSString)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.String (IsString, fromString)
import qualified Data.Text as T
import           GHC.TypeLits (KnownSymbol, symbolVal, Symbol)
import           GHCJS.Marshal (ToJSVal, toJSVal)
import           JavaScript.Object.Internal (Object)
import           Prelude hiding (null)
import           Servant.API (HasLink(MkLink, toLink))

import           Miso.Effect
import           Miso.Event.Types
import           Miso.FFI (JSM)
import           Miso.String (MisoString, toMisoString)
import qualified Miso.String as MS

-- | Application entry point
data App model action = App
  { model :: model
  -- ^ initial model
  , update :: action -> model -> Effect action model
  -- ^ Function to update model, optionally providing effects.
  --   See the 'Transition' monad for succinctly expressing model transitions.
  , view :: model -> View action
  -- ^ Function to draw `View`
  , subs :: [ Sub action ]
  -- ^ List of subscriptions to run during application lifetime
  , events :: M.Map MisoString Bool
  -- ^ List of delegated events that the body element will listen for.
  --   You can start with 'Miso.Event.Types.defaultEvents' and modify as needed.
  , initialAction :: action
  -- ^ Initial action that is run after the application has loaded
  , mountPoint :: Maybe MisoString
  -- ^ Id of the root element for DOM diff. If 'Nothing' is provided, the entire document body is used as a mount point.
  , logLevel :: LogLevel
  }

-- | Convenience for extracting mount point
getMountPoint :: Maybe MisoString -> MisoString
getMountPoint = fromMaybe "body"

-- | Smart constructor for @App@ with sane defaults.
defaultApp
  :: model
  -> (action -> model -> Effect action model)
  -> (model -> View action)
  -> action
  -> App model action
defaultApp m u v a = App
  { initialAction = a
  , model = m
  , view = v
  , update = u
  , subs = []
  , events = defaultEvents
  , mountPoint = Nothing
  , logLevel = Off
  }

-- | Optional Logging for debugging miso internals (useful to see if prerendering is successful)
data LogLevel
  = Off
  | DebugPrerender
  deriving (Show, Eq)

-- | A monad for succinctly expressing model transitions in the 'update' function.
--
-- @Transition@ is a state monad so it abstracts over manually passing the model
-- around. It's also a writer monad where the accumulator is a list of scheduled
-- IO actions. Multiple actions can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library and a single action
-- can be scheduled using 'scheduleIO'.
--
-- Tip: use the @Transition@ monad in combination with the stateful
-- <http://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Operators.html lens>
-- operators (all operators ending in "@=@"). The following example assumes
-- the lenses @field1@, @counter@ and @field2@ are in scope and that the
-- @LambdaCase@ language extension is enabled:
--
-- @
-- myApp = App
--   { update = 'fromTransition' . \\case
--       MyAction1 -> do
--         field1 .= value1
--         counter += 1
--       MyAction2 -> do
--         field2 %= f
--         scheduleIO $ do
--           putStrLn \"Hello\"
--           putStrLn \"World!\"
--   , ...
--   }
-- @
type Transition action model = StateT model (Writer [Sub action])

-- | Turn a transition that schedules subscriptions that consume
-- actions of type @a@ into a transition that schedules subscriptions
-- that consume actions of type @b@ using the supplied function of
-- type @a -> b@.
mapAction :: (actionA -> actionB) -> Transition actionA model r -> Transition actionB model r
mapAction = mapStateT . mapWriter . second . fmap . mapSub

-- | Convert a @Transition@ computation to a function that can be given to 'update'.
fromTransition
    :: Transition action model ()
    -> (model -> Effect action model) -- ^ model 'update' function.
fromTransition act = \m ->
  case runWriter (execStateT act m) of
    (n, actions) -> Effect n actions

-- | Convert an 'update' function to a @Transition@ computation.
toTransition
    :: (model -> Effect action model) -- ^ model 'update' function
    -> Transition action model ()
toTransition f =
  StateT $ \m ->
    case f m of
      Effect n actions -> do
        tell actions
        pure ((), n)

-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
scheduleIO :: JSM action -> Transition action model ()
scheduleIO ioAction = scheduleSub $ \sink -> ioAction >>= sink

-- | Like 'scheduleIO' but doesn't cause an action to be dispatched to
-- the 'update' function.
--
-- This is handy for scheduling IO computations where you don't care
-- about their results or when they complete.
scheduleIO_ :: JSM () -> Transition action model ()
scheduleIO_ ioAction = scheduleSub $ \_sink -> ioAction

-- | Like `scheduleIO_` but generalized to any instance of `Foldable`
--
-- This is handy for scheduling IO computations that return a `Maybe` value
scheduleIOFor_ :: Foldable f => JSM (f action) -> Transition action model ()
scheduleIOFor_ io = scheduleSub $ \sink -> io >>= flip for_ sink

-- | Like 'scheduleIO' but schedules a subscription which is an IO
-- computation that has access to a 'Sink' which can be used to
-- asynchronously dispatch actions to the 'update' function.
--
-- A use-case is scheduling an IO computation which creates a
-- 3rd-party JS widget which has an associated callback. The callback
-- can then call the sink to turn events into actions. To do this
-- without accessing a sink requires going via a @'Sub'scription@
-- which introduces a leaky-abstraction.
scheduleSub :: Sub action -> Transition action model ()
scheduleSub sub = lift $ tell [ sub ]

-- | Core type for constructing a `VTree`, use this instead of `VTree` directly.
data View action
  = Node NS MisoString (Maybe Key) [Attribute action] [View action]
  | Text MisoString
  | TextRaw MisoString
  | Embed SomeComponent (ComponentOptions action)
  deriving Functor

-- | Options for Components, used with @embedWith@
-- Components are implemented as `div`.
data ComponentOptions action
  = ComponentOptions
  { onMounted :: Maybe action
  , onUnmounted :: Maybe action
  , attributes :: [ Attribute action ]
  , componentKey :: Maybe Key
  } deriving Functor

-- | Smart constructor for @ComponentOptions@
componentOptions :: ComponentOptions action
componentOptions
  = ComponentOptions
  { onMounted = Nothing
  , onUnmounted = Nothing
  , attributes = []
  , componentKey = Nothing
  }

-- | Existential wrapper used to allow the nesting of @Component@ in @App@
data SomeComponent
   = forall name model action . Eq model
   => SomeComponent (Component name model action)

-- | Used with @component@ to parameterize @App@ by @name@
data Component (name :: Symbol) model action
  = Component
  { componentName :: MisoString
  , componentApp :: App model action
  }

-- | Smart constructor for parameterizing @App@ by @name@
-- Needed when calling @embed@ and @embedWith@
component
  :: forall name model action
  . KnownSymbol name
  => App model action
  -> Component name model action
component = Component (MS.ms (symbolVal (Proxy @name)))

-- | Used in the @view@ function to @embed@ @Component@s in @App@
embed :: Eq model => Component name model a -> View action
embed comp = Embed (SomeComponent comp) componentOptions

-- | Like @embed@ but with @ComponentOptions@ for mounting / unmounting, @Attribute@, etc.
embedWith
  :: Eq model
  => Component name model a
  -> ComponentOptions action
  -> View action
embedWith comp opts = Embed (SomeComponent comp) opts

-- | For constructing type-safe links
instance HasLink (View a) where
  type MkLink (View a) b = b
  toLink x _ = x

-- | Convenience class for using View
class ToView a where
  type ToViewAction a :: *
  toView :: a -> View (ToViewAction a)

instance ToView (View action) where
  type ToViewAction (View action) = action
  toView = id

instance ToView (Component name model action) where
  type ToViewAction (Component name model action) = action
  toView (Component _ app) = toView app

instance ToView (App model action) where
  type ToViewAction (App model action) = action
  toView App {..} = toView (view model)

-- | Namespace of DOM elements.
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  | MATHML  -- ^ MATHML Namespace
  deriving (Show, Eq)

instance ToJSVal NS where
  toJSVal SVG  = toJSVal ("svg" :: JSString)
  toJSVal HTML = toJSVal ("html" :: JSString)
  toJSVal MATHML = toJSVal ("mathml" :: JSString)

-- | A unique key for a dom node.
--
-- This key is only used to speed up diffing the children of a DOM
-- node, the actual content is not important. The keys of the children
-- of a given DOM node must be unique. Failure to satisfy this
-- invariant gives undefined behavior at runtime.
newtype Key = Key MisoString

instance ToJSVal Key where toJSVal (Key x) = toJSVal x

-- | Convert custom key types to `Key`.
--
-- Instances of this class do not have to guarantee uniqueness of the
-- generated keys, it is up to the user to do so. `toKey` must be an
-- injective function.
class ToKey key where toKey :: key -> Key
-- | Identity instance
instance ToKey Key where toKey = id
-- | Convert `MisoString` to `Key`
instance ToKey JSString where toKey = Key . toMisoString
-- | Convert `T.Text` to `Key`
instance ToKey T.Text where toKey = Key . toMisoString
-- | Convert `String` to `Key`
instance ToKey String where toKey = Key . toMisoString
-- | Convert `Int` to `Key`
instance ToKey Int where toKey = Key . toMisoString
-- | Convert `Double` to `Key`
instance ToKey Double where toKey = Key . toMisoString
-- | Convert `Float` to `Key`
instance ToKey Float where toKey = Key . toMisoString
-- | Convert `Word` to `Key`
instance ToKey Word where toKey = Key . toMisoString

-- | Attribute of a vnode in a `View`.
--
-- The 'Sink' callback can be used to dispatch actions which are fed back to
-- the @update@ function. This is especially useful for event handlers
-- like the @onclick@ attribute. The second argument represents the
-- vnode the attribute is attached to.
data Attribute action
  = P MisoString Value
  | E (Sink action -> Object -> JSM ())
  | S (M.Map MisoString MisoString)
  deriving Functor

-- | `IsString` instance
instance IsString (View a) where
  fromString = Text . fromString
