{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Miso.Html.Types (
    -- * Core types and interface
      Attribute (..)
    -- * Key patch internals
    , Key    (..)
    , ToKey  (..)
    -- * Namespace
    , NS(..)
    -- * Setting properties on virtual DOM nodes
    , prop
    -- * Setting css
    , style_
    -- * Handling events
    , on
    , onWithOptions
    -- * Life cycle events
    , onCreated
    , onDestroyed
    , onBeforeDestroyed
    ) where

import           Control.Monad ((<=<))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON, Value, toJSON)
import           Data.Aeson.Types (parseEither)
import           Data.JSString (JSString)
import qualified Data.Map as M
import qualified Data.Text as T
import           GHCJS.Marshal (fromJSVal, toJSVal)
import           JavaScript.Object (create, getProp)
import           JavaScript.Object.Internal (Object(Object))

import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.String

-- | Namespace of DOM elements.
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  | MATHML  -- ^ MATHML Namespace
  deriving (Show, Eq)

-- | A unique key for a dom node.
--
-- This key is only used to speed up diffing the children of a DOM
-- node, the actual content is not important. The keys of the children
-- of a given DOM node must be unique. Failure to satisfy this
-- invariant gives undefined behavior at runtime.
newtype Key = Key MisoString

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

-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM node associated with the vnode
-- to @v@.
prop :: ToJSON a => MisoString -> a -> Attribute action
prop k v = P k (toJSON v)

-- | Convenience wrapper for @onWithOptions defaultOptions@.
--
-- > let clickHandler = on "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: MisoString
   -> Decoder r
   -> (r -> action)
   -> Attribute action
on = onWithOptions defaultOptions

-- | @onWithOptions opts eventName decoder toAction@ is an attribute
-- that will set the event handler of the associated DOM node to a function that
-- decodes its argument using @decoder@, converts it to an action
-- using @toAction@ and then feeds that action back to the @update@ function.
--
-- @opts@ can be used to disable further event propagation.
--
-- > let clickHandler = onWithOptions defaultOptions "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
onWithOptions
  :: Options
  -> MisoString
  -> Decoder r
  -> (r -> action)
  -> Attribute action
onWithOptions options eventName Decoder{..} toAction =
  E $ \sink n -> do
   eventObj <- getProp "events" n
   eventHandlerObject@(Object eo) <- create
   jsOptions <- toJSVal options
   decodeAtVal <- toJSVal decodeAt
   cb <- callbackToJSVal <=< asyncCallback1 $ \e -> do
       Just v <- fromJSVal =<< objectToJSON decodeAtVal e
       case parseEither decoder v of
         Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
         Right r -> liftIO (sink (toAction r))
   set "runEvent" cb eventHandlerObject
   registerCallback cb
   set "options" jsOptions eventHandlerObject
   set eventName eo (Object eventObj)

-- | @onCreated action@ is an event that gets called after the actual DOM
-- element is created.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onCreated :: action -> Attribute action
onCreated action =
  E $ \sink n -> do
    cb <- callbackToJSVal =<< asyncCallback (liftIO (sink action))
    set "onCreated" cb n
    registerCallback cb

-- | @onDestroyed action@ is an event that gets called after the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onDestroyed :: action -> Attribute action
onDestroyed action =
  E $ \sink n -> do
    cb <- callbackToJSVal =<< asyncCallback (liftIO (sink action))
    set "onDestroyed" cb n
    registerCallback cb

-- | @onBeforeDestroyed action@ is an event that gets called before the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onBeforeDestroyed :: action -> Attribute action
onBeforeDestroyed action =
  E $ \sink n -> do
    cb <- callbackToJSVal =<< asyncCallback (liftIO (sink action))
    set "onBeforeDestroyed" cb n
    registerCallback cb

-- | @style_ attrs@ is an attribute that will set the @style@
-- attribute of the associated DOM node to @attrs@.
--
-- @style@ attributes not contained in @attrs@ will be deleted.
--
-- > import qualified Data.Map as M
-- > div_ [ style_  $ M.singleton "background" "red" ] [ ]
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS>
--
style_ :: M.Map MisoString MisoString -> Attribute action
style_ = S
