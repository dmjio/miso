{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE CPP                       #-}
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
  ( App (..)
  , defaultApp
  , View (..)
  , Component (..)
  , component
  , componentKey
  , componentMount
  , ToView (..)
  , Key (..)
  , toKey
  , Attribute (..)
  , NS (..)
  , LogLevel (..)
  , Mount (..)
  , getMountPoint
    -- * The Transition Monad
  , Transition
  , mapAction
  , fromTransition
  , toTransition
  , scheduleIO
  , scheduleIO_
  , scheduleIOFor_
  , scheduleSub
  , runTransition
  -- * The Effect Monad
  , Effect
  , Sub
  , Sink
  , mapSub
  , noEff
  , (<#)
  , (#>)
  , batchEff
  , effectSub
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT(StateT), runStateT, execStateT, mapStateT, put)
import           Control.Monad.Trans.Writer.Strict (Writer, tell, mapWriter, runWriter)
import           Data.Aeson (Value)
import qualified Data.Aeson as A
import           Data.Bifunctor (second, Bifunctor(..))
import           Data.Foldable (for_)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.String (IsString, fromString)
import qualified Data.Text as T
import qualified Lucid as L
import qualified Lucid.Base as L
import           Prelude hiding (null)
import           Servant.API (Get, HasLink(MkLink, toLink))

import           Data.JSString (JSString)
import           JavaScript.Object.Internal (Object)
import           GHCJS.Marshal (ToJSVal, toJSVal)
import           Miso.Event.Types
import           Miso.FFI (JSM)
import           Miso.String (MisoString, fromMisoString, toMisoString)
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

-- | Evaluate transition
runTransition :: model -> Transition action model a -> (model, [Sub action])
runTransition m transition = runWriter (execStateT transition m)
  
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
fromTransition act = \m -> act <* put m

-- | Convert an 'update' function to a @Transition@ computation.
toTransition
    :: (model -> Effect action model) -- ^ model 'update' function
    -> Transition action model ()
toTransition f = StateT $ \m -> runStateT (f m) m

-- | Schedule a single IO action for later execution.
--
-- Note that multiple IO action can be scheduled using
-- @Control.Monad.Writer.Class.tell@ from the @mtl@ library.
scheduleIO :: JSM action -> Transition action model ()
scheduleIO ioAction = scheduleSub $ \sink -> ioAction >>= liftIO . sink

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
scheduleIOFor_ io = scheduleSub $ \sink -> io >>= \m -> liftIO (for_ m sink)

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
  | ComponentNode MisoString Component (Maybe (Mount action))
  deriving Functor

data Mount a
  = Mount
  { onMounted, onUnmounted :: a
  } deriving Functor

data Component
  = forall model action . Eq model
  => Component (Maybe Key) (App model action)

component :: Eq model => MisoString -> App model a -> View action
component name app = ComponentNode name (Component Nothing app) Nothing

componentMount :: Eq model => MisoString -> App model a -> Mount action -> View action
componentMount name app m = ComponentNode name (Component Nothing app) (Just m)

componentKey :: Eq model => MisoString -> App model a -> Key -> View action
componentKey name app key = ComponentNode name (Component (Just key) app) Nothing

-- | For constructing type-safe links
instance HasLink (View a) where
#if MIN_VERSION_servant(0,14,0)
  type MkLink (View a) b = MkLink (Get '[] ()) b
  toLink toA Proxy = toLink toA (Proxy :: Proxy (Get '[] ()))
#else
  type MkLink (View a) = MkLink (Get '[] ())
  toLink _ = toLink (Proxy :: Proxy (Get '[] ()))
#endif

-- | Convenience class for using View
class ToView v where toView :: v -> View action

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

-- | Converting `View` to Lucid's `L.Html`
instance L.ToHtml (View action) where
  toHtmlRaw = L.toHtml
  toHtml (ComponentNode mount (Component _ app) _) =
    L.div_ [ L.id_ (fromMisoString mount) ] $
      L.toHtml (view app (model app))
  toHtml (Node _ vType _ attrs vChildren) = L.with ele lattrs
    where
      noEnd = ["img", "input", "br", "hr", "meta"]
      tag = toTag $ fromMisoString vType
      ele = if tag `elem` noEnd
          then L.makeElementNoEnd tag
          else L.makeElement tag kids
      classes = T.intercalate " " [ v | P "class" (A.String v) <- attrs ]
      propClass = M.fromList $ attrs >>= \case
          P k v -> [(k, v)]
          E _ -> []
          S m -> [("style", A.String . fromMisoString $ M.foldrWithKey go mempty m)]
            where
              go :: MisoString -> MisoString -> MisoString -> MisoString
              go k v ys = mconcat [ k, ":", v, ";" ] <> ys
      xs = if not (T.null classes)
          then M.insert "class" (A.String classes) propClass
          else propClass
      lattrs = [ L.makeAttribute k' (if k `elem` exceptions && v == A.Bool True then k' else v')
               | (k,v) <- M.toList xs
               , let k' = fromMisoString k
               , let v' = toHtmlFromJSON v
               , not (k `elem` exceptions && v == A.Bool False)
               ]
      exceptions = [ "checked"
                   , "disabled"
                   , "selected"
                   , "hidden"
                   , "readOnly"
                   , "autoplay"
                   , "required"
                   , "default"
                   , "autofocus"
                   , "multiple"
                   , "noValidate"
                   , "autocomplete"
                   ]
      toTag = T.toLower
      kids = foldMap L.toHtml $ collapseSiblingTextNodes vChildren
  toHtml (Text x) | MS.null x = L.toHtml (" " :: T.Text)
                  | otherwise = L.toHtml (fromMisoString x :: T.Text)
  toHtml (TextRaw x)
    | MS.null x = L.toHtml (" " :: T.Text)
    | otherwise = L.toHtmlRaw (fromMisoString x :: T.Text)

collapseSiblingTextNodes :: [View a] -> [View a]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (Text x : Text y : xs) =
  collapseSiblingTextNodes (Text (x <> y) : xs)
-- TextRaw is the only child, so no need to collapse.
collapseSiblingTextNodes (x:xs) =
  x : collapseSiblingTextNodes xs

-- | Helper for turning JSON into Text
-- Object, Array and Null are kind of non-sensical here
toHtmlFromJSON :: Value -> T.Text
toHtmlFromJSON (A.String t) = t
toHtmlFromJSON (A.Number t) = T.pack (show t)
toHtmlFromJSON (A.Bool b) = if b then "true" else "false"
toHtmlFromJSON A.Null = "null"
toHtmlFromJSON (A.Object o) = T.pack (show o)
toHtmlFromJSON (A.Array a) = T.pack (show a)

-- | An effect represents the results of an update action.
--
-- It consists of the updated model and a list of subscriptions. Each 'Sub' is
-- run in a new thread so there is no risk of accidentally blocking the
-- application.
type Effect action model = Transition action model ()

-- | Type synonym for constructing event subscriptions.
--
-- The 'Sink' callback is used to dispatch actions which are then fed
-- back to the 'Miso.Types.update' function.
type Sub action = Sink action -> JSM ()

-- | Function to asynchronously dispatch actions to the 'Miso.Types.update' function.
type Sink action = action -> IO ()

-- | Turn a subscription that consumes actions of type @a@ into a subscription
-- that consumes actions of type @b@ using the supplied function of type @a -> b@.
mapSub :: (a -> b) -> Sub a -> Sub b
mapSub f sub = \g -> sub (g . f)

-- | Smart constructor for an 'Effect' with no actions.
noEff :: model -> Effect action model
noEff = put

-- | Smart constructor for an 'Effect' with exactly one action.
(<#) :: model -> JSM action -> Effect action model
(<#) m a = effectSub m $ \sink -> a >>= liftIO . sink

-- | `Effect` smart constructor, flipped
(#>) :: JSM action -> model -> Effect action model
(#>) = flip (<#)

-- | Smart constructor for an 'Effect' with multiple actions.
batchEff :: model -> [JSM action] -> Effect action model
batchEff m actions = do
  mapM_ scheduleIO actions
  put m

-- | Like '<#' but schedules a subscription which is an IO computation which has
-- access to a 'Sink' which can be used to asynchronously dispatch actions to
-- the 'Miso.Types.update' function.
--
-- A use-case is scheduling an IO computation which creates a 3rd-party JS
-- widget which has an associated callback. The callback can then call the sink
-- to turn events into actions. To do this without accessing a sink requires
-- going via a @'Sub'scription@ which introduces a leaky-abstraction.
effectSub :: model -> Sub action -> Effect action model
effectSub m sub = do
  scheduleSub sub
  put m
