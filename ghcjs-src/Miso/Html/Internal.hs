{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Internal
-- Copyright   :  (C) 2016-2017 David M. Johnson
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
  -- * String
  , module Miso.String
  -- * Events
  , defaultEvents
  -- * Subscription type
  , Sub
  ) where

import           Control.Monad
import           Data.Aeson hiding (Object)
import           Data.Aeson.Types (parseEither)
import           Data.Monoid
import           Data.JSString
import           Data.JSString.Text
import qualified Data.Map as M
import qualified Data.Text as T
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Array.Internal (fromList)
import           JavaScript.Object
import           JavaScript.Object.Internal (Object (Object))

import           Miso.Event.Decoder
import           Miso.Event.Types
import           Miso.String

type Sub a m = IO m -> (a -> IO ()) -> IO ()

-- | Virtual DOM implemented as a JavaScript `Object`
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree = VTree { getTree :: Object }

-- | Core type for constructing a `VTree`, use this instead of `VTree` directly.
newtype View action = View {
  runView :: (action -> IO ()) -> IO VTree
}

-- | Convenience class for using View
class ToView v where toView :: v -> View m

set :: ToJSVal v => JSString -> v -> Object -> IO ()
set k v obj = toJSVal v >>= \x -> setProp k x obj

-- | `VNode` creation
node :: NS
     -> MisoString
     -> Maybe Key
     -> [Attribute m]
     -> [View m]
     -> View m
node ns tag key attrs kids = View $ \sink -> do
  vnode <- create
  cssObj <- jsval <$> create
  propsObj <- jsval <$> create
  eventObj <- jsval <$> create
  set "css" cssObj vnode
  set "props" propsObj vnode
  set "events" eventObj vnode
  set "type" ("vnode" :: JSString) vnode
  set "ns" ns vnode
  set "tag" tag vnode
  set "key" key vnode
  setAttrs vnode sink
  flip (set "children") vnode =<< setKids sink
  pure $ VTree vnode
    where
      setAttrs vnode sink =
        forM_ attrs $ \(Attribute attr) ->
          attr sink vnode

      setKids sink =
        jsval . fromList <$>
          fmap (jsval . getTree) <$>
            traverse (flip runView sink) kids

instance ToJSVal Options
instance ToJSVal Key where toJSVal (Key x) = toJSVal x

instance ToJSVal NS where
  toJSVal SVG  = toJSVal ("svg" :: JSString)
  toJSVal HTML = toJSVal ("html" :: JSString)

-- | Namespace for element creation
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  deriving (Show, Eq)

-- | `VText` creation
text :: ToMisoString str => str -> View m
text t = View . const $ do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" (toMisoString t) vtree
  pure $ VTree vtree

newtype Key = Key MisoString

-- | Convert type into Key, ensure `Key` is unique
class ToKey key where toKey :: key -> Key
-- | Identity instance
instance ToKey Key where toKey = id
-- | Convert `MisoString` to `Key`
instance ToKey MisoString where toKey = Key
-- | Convert `Text` to `Key`
instance ToKey T.Text where toKey = Key . textToJSString
-- | Convert `String` to `Key`
instance ToKey String where toKey = Key . pack
-- | Convert `Int` to `Key`
instance ToKey Int where toKey = Key . pack . show
-- | Convert `Double` to `Key`
instance ToKey Double where toKey = Key . pack . show
-- | Convert `Float` to `Key`
instance ToKey Float where toKey = Key . pack . show
-- | Convert `Word` to `Key`
instance ToKey Word where toKey = Key . pack . show

-- | `View` Attributes to annotate DOM, converted into `Events`, `Props`, `Attrs` and `CSS`
newtype Attribute action = Attribute ((action -> IO ()) -> Object -> IO ())

-- | Constructs a property on a `VNode`, used to set fields on a DOM Node
prop :: ToJSVal a => MisoString -> a -> Attribute action
prop k v = Attribute . const $ \n -> do
  val <- toJSVal v
  o <- getProp ("props" :: MisoString) n
  set k val (Object o)

-- | For defining delegated events with options
--
-- > let clickHandler = on "click" $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: MisoString
   -> Decoder r
   -> (r -> action)
   -> Attribute action
on = onWithOptions defaultOptions

foreign import javascript unsafe "$r = objectToJSON($1,$2);"
  objectToJSON
    :: JSVal -- ^ decodeAt :: [JSString]
    -> JSVal -- ^ object with impure references to the DOM
    -> IO JSVal

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
   cb <- jsval <$> (asyncCallback1 $ \e -> do
        -- DMJ: This breaks on 8.x (FromJSVal Value instance)
       Just (v :: Value) <- fromJSVal =<< objectToJSON decodeAtVal e
       case parseEither decoder v of
         Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
         Right r -> sink (toAction r))
   setProp "runEvent" cb eventHandlerObject
   setProp "options" jsOptions eventHandlerObject
   setProp eventName eo (Object eventObj)

-- | Constructs `CSS` for a DOM Element
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
     setProp k (jsval v) (Object cssObj)
