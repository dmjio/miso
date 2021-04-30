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
  -- * `View` runner
  , runView
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
import           Data.Aeson (ToJSON, Value, toJSON)
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

-- | Convenience class for using View
class ToView v where toView :: v -> View m

runView :: View action -> Sink action -> JSM VTree
runView (Node ns tag key attrs kids) sink = do
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
        forM_ attrs $ \case
          P k v -> do
            val <- toJSVal v
            o <- getProp "props" vnode
            set k val (Object o)
          E attr -> attr sink vnode
          S m -> do
            cssObj <- getProp "css" vnode
            forM_ (M.toList m) $ \(k,v) -> do
              set k v (Object cssObj)
      setKids sink = do
        kidsViews <- traverse (objectToJSVal . getTree <=< flip runView sink) kids
        ghcjsPure (JSArray.fromList kidsViews)
runView (Text t) _ = do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" t vtree
  pure $ VTree vtree
runView (TextRaw t) s = runView (Text t) s
