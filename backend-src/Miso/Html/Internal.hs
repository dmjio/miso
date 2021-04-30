{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
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
    View   (..)
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
  -- * Setting CSS
  , style_
  -- * Handling events
  , on
  , onWithOptions
  -- * Life cycle events
  , onCreated
  , onDestroyed
  , onBeforeDestroyed
  ) where

import           Data.Aeson  (Value(..), ToJSON(..))
import qualified Data.Map    as M
import           Data.Proxy
import           Data.String (IsString(..))
import qualified Data.Text   as T
import qualified Data.Vector as V
import qualified Lucid       as L
import qualified Lucid.Base  as L
import           Prelude     hiding (null)
import           Servant.API

import           Miso.Event
import           Miso.Html.Types
import           Miso.String hiding (map)

-- | Converting `View` to Lucid's `L.Html`
instance L.ToHtml (View action) where
  toHtmlRaw = L.toHtml
  toHtml (Node vNs vType vKey attrs vChildren) = L.with ele lattrs
    where
      noEnd = ["img", "input", "br", "hr", "meta"]
      tag = toTag vType
      ele = if tag `elem` noEnd
          then L.makeElementNoEnd tag
          else L.makeElement tag kids
      classes = intercalate " " [ v | P "class" (String v) <- attrs ]
      propClass = M.fromList $ attrs >>= \case
          P k v -> [(k, v)]
          E _ -> []
          S m -> [("style", String (M.foldrWithKey go mempty m))]
            where
              go :: MisoString -> MisoString -> MisoString -> MisoString
              go k v ys = mconcat [ k, ":", v, ";" ] <> ys
      xs = if not (null classes)
          then M.insert "class" (String classes) propClass
          else propClass
      lattrs = [ L.makeAttribute k (if k `elem` exceptions && v == Bool True then k else v')
           | (k,v) <- M.toList xs
           , let v' = toHtmlFromJSON v
           , not (k `elem` exceptions && v == Bool False)
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
  toHtml (Text x) | T.null x = L.toHtml (" " :: MisoString)
                  | otherwise = L.toHtml x
  toHtml (TextRaw x) | T.null x = L.toHtml (" " :: MisoString)
                     | otherwise = L.toHtmlRaw x

collapseSiblingTextNodes :: [View a] -> [View a]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (Text x : Text y : xs) =
  collapseSiblingTextNodes (Text (x <> y) : xs)
-- TextRaw is the only child, so no need to collapse.
collapseSiblingTextNodes (x:xs) =
  x : collapseSiblingTextNodes xs

-- | Helper for turning JSON into Text
-- Object, Array and Null are kind of non-sensical here
toHtmlFromJSON :: Value -> Text
toHtmlFromJSON (String t) = t
toHtmlFromJSON (Number t) = pack (show t)
toHtmlFromJSON (Bool b) = if b then "true" else "false"
toHtmlFromJSON Null = "null"
toHtmlFromJSON (Object o) = pack (show o)
toHtmlFromJSON (Array a) = pack (show a)
