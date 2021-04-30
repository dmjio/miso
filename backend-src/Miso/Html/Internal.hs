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

-- | Virtual DOM implemented as a Rose `Vector`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
data VTree action where
  VNode :: { vType :: Text -- ^ Element type (i.e. "div", "a", "p")
           , vNs :: NS -- ^ HTML or SVG
           , vProps :: Props -- ^ Fields present on DOM Node
           , vKey :: Maybe Key -- ^ Key used for child swap patch
           , vChildren :: V.Vector (VTree action) -- ^ Child nodes
           } -> VTree action
  VText :: { vText :: Text -- ^ TextNode content
           } -> VTree action
  -- Invariant: To avoid complexity with collapsing mixed VText and
  -- VTextRaw nodes, VTextRaw node is always the only child.
  -- That's not a big limitation, since the intended purpose is to be able
  -- to use <style> and <script> tags. That means we can't support HTML
  -- entities like &nbsp; with VTextRaw, but the same can be achieved with
  -- Unicode.
  VTextRaw :: { vText :: Text -- ^ Raw TextNode content
              } -> VTree action
  deriving Functor

instance Show (VTree action) where
  show = show . L.toHtml

-- | Converting `VTree` to Lucid's `L.Html`
instance L.ToHtml (VTree action) where
  toHtmlRaw = L.toHtml
  toHtml (VText x) | T.null x = L.toHtml (" " :: MisoString)
                   | otherwise = L.toHtml x
  toHtml (VTextRaw x) | T.null x = L.toHtml (" " :: MisoString)
                      | otherwise = L.toHtmlRaw x
  toHtml VNode{..} =
    let
      noEnd = ["img", "input", "br", "hr", "meta"]
      tag = toTag vType
      ele =
          if tag `elem` noEnd
            then L.makeElementNoEnd tag
            else L.makeElement tag kids
    in L.with ele as
      where
        Props xs = vProps
        as = [ L.makeAttribute k (if k `elem` exceptions && v == Bool True then k else v')
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
        kids
          = foldMap L.toHtml
          . V.fromList
          . collapseSiblingTextNodes
          . V.toList
          $ vChildren

collapseSiblingTextNodes :: [VTree a] -> [VTree a]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (VText x : VText y : xs) =
  collapseSiblingTextNodes (VText (x <> y) : xs)
-- VTextRaw is the only child, so no need to collapse.
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

-- | Core type for constructing a `VTree`, use this instead of `VTree` directly.
newtype View action = View { runView :: VTree action }
  deriving Functor

-- | For constructing type-safe links
instance HasLink (View a) where
#if MIN_VERSION_servant(0,14,0)
  type MkLink (View a) b = MkLink (Get '[] ()) b
  toLink toA Proxy = toLink toA (Proxy :: Proxy (Get '[] ()))
#else
  type MkLink (View a) = MkLink (Get '[] ())
  toLink Proxy = toLink (Proxy :: Proxy (Get '[] ()))
#endif


-- | Convenience class for using View
class ToView v where toView :: v -> View action

-- | Show `View`
instance Show (View action) where
  show (View xs) = show xs

-- | Converting `View` to Lucid's `L.Html`
instance L.ToHtml (View action) where
  toHtmlRaw = L.toHtml
  toHtml (View xs) = L.toHtml xs

-- | `VNode` creation
node :: NS -> MisoString -> Maybe Key -> [Attribute action] -> [View action] -> View action
node vNs vType vKey as xs =
  let classes = intercalate " " [ v | P "class" (String v) <- as ]
      vProps  = Props $ do
        let propClass = M.fromList $ as >>= \case
              P k v -> [(k, v)]
              E _ -> []
              S m -> [("style", String (M.foldrWithKey go mempty m))]
                where
                  go :: MisoString -> MisoString -> MisoString -> MisoString
                  go k v xs = mconcat [ k, ":", v, ";" ] <> xs
        if not (null classes)
          then M.insert "class" (String classes) propClass
          else propClass
      vChildren = V.fromList $ map runView xs
  in View VNode {..}

-- | `VText` creation
text :: MisoString -> View action
text = View . VText

-- | `VTextRaw` creation. Don't use directly
textRaw :: MisoString -> View action
textRaw = View . VTextRaw

-- | `IsString` instance
instance IsString (View a) where
  fromString = text . fromString

-- | Properties
newtype Props = Props (M.Map MisoString Value)
  deriving (Show, Eq)
