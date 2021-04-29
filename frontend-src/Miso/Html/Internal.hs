{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE RecordWildCards            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Internal
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.Internal (
  -- * Core types and interface
    VTree  (..)
  , View   (..)
  , ToView (..)
  , Attribute (..)
  -- * Smart `View` constructors
  , node
  , text
  , textRaw
  -- * Key patch internals
  , Key    (..)
  , ToKey  (..)
  -- * Namespace
  , NS     (..)
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
  -- * Events
  , defaultEvents
  -- * Subscription type
  , Sub
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson.Types (parseEither)
import           Data.JSString (JSString)
import qualified Data.Map as M
import           Data.Proxy
import           Data.String (IsString(..))
import qualified Data.Text as T
import           GHCJS.Marshal
import           GHCJS.Types
import qualified JavaScript.Array as JSArray
import           JavaScript.Object
import           JavaScript.Object.Internal (Object (Object))
import           Servant.API

import           Miso.Effect (Sub)
import           Miso.Event.Decoder
import           Miso.Event.Types
import           Miso.String
import           Miso.Effect (Sink)
import           Miso.FFI
import           Miso.Html.Types

-- | Virtual DOM implemented as a JavaScript `Object`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree = VTree { getTree :: Object }

-- | Core type for constructing a `VTree`, use this instead of `VTree` directly.
newtype View action = View {
  runView :: Sink action -> JSM VTree
} deriving Functor

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
class ToView v where toView :: v -> View m

-- | Create a new @Miso.Types.VNode@.
--
-- @node ns tag key attrs children@ creates a new node with tag @tag@
-- and 'Key' @key@ in the namespace @ns@. All @attrs@ are called when
-- the node is created and its children are initialized to @children@.
node :: NS
     -> MisoString
     -> Maybe Key
     -> [Attribute m]
     -> [View m]
     -> View m
node ns tag key attrs kids = View $ \sink -> do
  vnode <- create
  cssObj <- objectToJSVal =<< create
  propsObj <- objectToJSVal =<< create
  eventObj <- objectToJSVal =<< create
  set "css" cssObj vnode
  set "props" propsObj vnode
  set "events" eventObj vnode
  set "type" ("vnode" :: JSString) vnode
  set "ns" ns vnode
  set "tag" tag vnode
  set "key" key vnode
  setAttrs vnode sink
  flip (set "children") vnode
    =<< ghcjsPure . jsval
    =<< setKids sink
  pure $ VTree vnode
    where
      setAttrs vnode sink =
        forM_ attrs $ \(Attribute attr) ->
          attr sink vnode
      setKids sink = do
        kidsViews <- traverse (objectToJSVal . getTree <=< flip runView sink) kids
        ghcjsPure (JSArray.fromList kidsViews)

instance ToJSVal Options
instance ToJSVal Key where toJSVal (Key x) = toJSVal x

instance ToJSVal NS where
  toJSVal SVG  = toJSVal ("svg" :: JSString)
  toJSVal HTML = toJSVal ("html" :: JSString)
  toJSVal MATHML = toJSVal ("mathml" :: JSString)

-- | Create a new @VText@ with the given content.
text :: MisoString -> View m
text t = View . const $ do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" t vtree
  pure $ VTree vtree

-- | For parity with server-side rendering. Don't use directly.
textRaw :: MisoString -> View m
textRaw = text

-- | `IsString` instance
instance IsString (View a) where
  fromString = text . fromString

-- | Attribute of a vnode in a `View`.
--
-- The 'Sink' callback can be used to dispatch actions which are fed back to
-- the @update@ function. This is especially useful for event handlers
-- like the @onclick@ attribute. The second argument represents the
-- vnode the attribute is attached to.
newtype Attribute action = Attribute (Sink action -> Object -> JSM ())

-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM node associated with the vnode
-- to @v@.
prop :: ToJSVal a => MisoString -> a -> Attribute action
prop k v = Attribute . const $ \n -> do
  val <- toJSVal v
  o <- getProp "props" n
  set k val (Object o)

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
  Attribute $ \sink n -> do
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
  Attribute $ \sink n -> do
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
  Attribute $ \sink n -> do
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
  Attribute $ \sink n -> do
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
style_ m = Attribute . const $ \n -> do
   cssObj <- getProp "css" n
   forM_ (M.toList m) $ \(k,v) ->
     set k v (Object cssObj)
