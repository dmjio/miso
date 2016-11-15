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
  -- * Smart `View` constructors
  , node
  , text
  -- * Key patch internals
  , Key    (..)
  , ToKey  (..)
  -- * Namespace
  , NS     (..)
  -- * String module
  , MisoVal
  , MisoString
  -- * Helpers for interacting with Object
  , set
  ) where

import           Control.Monad
import           Data.JSString
import           Data.JSString.Text
import qualified Data.Text                  as T
import           GHC.Generics
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Array.Internal  (fromList)
import           JavaScript.Object
import           JavaScript.Object.Internal (Object (Object))

import           Miso.Event
import           Miso.Event.Interpreter
import           Miso.String
import           Miso.Html.Types
import           Miso.Html.Types.Event
import           Miso.Signal

-- | Virtual DOM implemented as a JavaScript `Object`
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree action = VTree { getTree :: Object }

-- | Core type for constructing a `VTree`, use this instead of `VTree` directly.
newtype View action = View { runView :: IO (VTree action) }

-- | Convenience class for using View
class ToView v where
  toView :: v -> View action

set :: ToJSVal v => JSString -> v -> Object -> IO ()
set k v obj = toJSVal v >>= \x -> setProp k x obj

-- | `VNode` creation
node :: NS
     -> MisoString
     -> Maybe Key
     -> [Attribute action]
     -> [View action]
     -> View action
node ns tag key attrs kids = View $ do
  vtree <- create
  set "type" ("vnode" :: JSString) vtree
  set "ns" ns vtree
  set "tag" tag vtree
  set "key" key vtree
  setAttrs vtree
  flip (set "children") vtree =<< setKids
  pure $ VTree vtree
    where
      setAttrs vtree = do
        [ attrObject@(Object ao),
          propObject@(Object po),
          cssObject@(Object co),
          eventObject@(Object eo)
          ] <- replicateM 4 create
        forM_ attrs $ \case
          A k v -> set k v attrObject
          P k v -> set k v propObject
          C k v -> set k v cssObject
          E (EventHandler options eventName grammar f) ->
            do eventHandlerObject@(Object o) <- create
               jsOptions <- toJSVal options
               setProp "options" jsOptions eventHandlerObject
               cb <- jsval <$> (asyncCallback1 $ \e -> do
                 let (_, writer) = defaultSignal
                 writer =<< f <$> evalEventGrammar e grammar)
               setProp "runEvent" cb eventHandlerObject
               setProp eventName o eventObject
        set "attrs" ao vtree
        set "props" po vtree
        set "css" co vtree
        set "events" eo vtree

      setKids =
        jsval . fromList <$>
          fmap (jsval . getTree) <$>
            traverse runView kids

instance ToJSVal Options

instance ToJSVal Key where
  toJSVal (Key x) = toJSVal x

instance ToJSVal NS where
  toJSVal SVG  = toJSVal ("svg" :: JSString)
  toJSVal HTML = toJSVal ("html" :: JSString)

-- | Namespace for element creation
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  deriving (Show, Eq)

-- | `VText` creation
text :: ToMisoString str => str -> View action
text t = View $ do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" (toMisoString t) vtree
  pure $ VTree vtree

-- | Key for specific children patch
newtype Key = Key { getKey :: MisoString }
  deriving (Show, Eq, Ord, Generic)

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
