{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
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
  -- * Setting CSS
  , style_
  -- * Handling events
  , on
  , onWithOptions
  -- * String
  , module Miso.String
  ) where

import           Data.Aeson
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Lucid as L
import qualified Lucid.Base as L
import           Miso.String hiding (map)
import           Miso.Event

-- | Virtual DOM implemented as a Rose `Vector`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
data VTree model where
  VNode :: { vType :: Text -- ^ Element type (i.e. "div", "a", "p")
           , vNs :: NS -- ^ HTML or SVG
           , vProps :: Props -- ^ Fields present on DOM Node
           , vCss :: CSS -- ^ Styles
           , vKey :: Maybe Key -- ^ Key used for child swap patch
           , vChildren :: V.Vector (VTree model) -- ^ Child nodes
           } -> VTree model
  VText :: { vText :: Text -- ^ TextNode content
           } -> VTree model

instance Show (VTree model) where
  show = show . L.toHtml

-- | Converting `VTree` to Lucid's `L.Html`
instance L.ToHtml (VTree model) where
  toHtmlRaw = L.toHtml
  toHtml (VText x) = L.toHtml x
  toHtml VNode{..} =
    let ele = L.makeElement (toTag vType) kids
    in L.with ele as
      where
        Props xs = vProps
        as = [ L.makeAttribute k v'
             | (k,v) <- M.toList xs
             , let v' = toHtmlFromJSON v
             ]
        toTag = T.toLower
        kids = foldMap L.toHtml vChildren

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
newtype View model = View { runView :: VTree model }

-- | Convenience class for using View
class ToView v where toView :: v -> View model

-- | Show `View`
instance Show (View model) where
  show (View xs) = show xs

-- | Converting `View` to Lucid's `L.Html`
instance L.ToHtml (View model) where
  toHtmlRaw = L.toHtml
  toHtml (View xs) = L.toHtml xs

-- | Namespace for element creation
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  deriving (Show, Eq)

-- | `VNode` creation
node :: NS -> MisoString -> Maybe Key -> [Attribute model] -> [View model] -> View model
node vNs vType vKey as xs =
  let vProps  = Props  $ M.fromList [ (k,v) | P k v <- as ]
      vCss    = CSS    $ M.fromList [ (k,v) | C k v <- as ]
      vChildren = V.fromList $ map runView xs
  in View VNode {..}

-- | `VText` creation
text :: ToMisoString str => str -> View model
text x = View $ VText (toMisoString x)

-- | Key for specific children patch
newtype Key = Key MisoString
  deriving (Show, Eq, Ord)

-- | Convert type into Key, ensure `Key` is unique
class ToKey key where toKey :: key -> Key
-- | Identity instance
instance ToKey Key    where toKey = id
-- | Convert `Text` to `Key`
instance ToKey MisoString where toKey = Key
-- | Convert `String` to `Key`
instance ToKey String where toKey = Key . T.pack
-- | Convert `Int` to `Key`
instance ToKey Int    where toKey = Key . T.pack . show
-- | Convert `Double` to `Key`
instance ToKey Double where toKey = Key . T.pack . show
-- | Convert `Float` to `Key`
instance ToKey Float  where toKey = Key . T.pack . show
-- | Convert `Word` to `Key`
instance ToKey Word   where toKey = Key . T.pack . show

-- | Fields that a DOM node contains
newtype Props = Props (M.Map MisoString Value)

-- | Individual CSS property diffing
newtype CSS = CSS (M.Map MisoString MisoString)

-- | `View` Attributes to annotate DOM, converted into `Events`, `Props`, `Attrs` and `CSS`
data Attribute model
  = C MisoString MisoString
  | P MisoString Value
  | E ()

-- | DMJ: this used to get set on preventDefault on Options... if options are dynamic now what
-- | Useful for `drop` events
newtype AllowDrop = AllowDrop Bool
  deriving (Show, Eq)

-- | Constructs a property on a `VNode`, used to set fields on a DOM Node
prop :: ToJSON a => MisoString -> a -> Attribute model
prop k v = P k (toJSON v)  

on :: MisoString
   -> Decoder r
   -> (r -> action)
   -> Attribute action
on _ _ _ = E ()

onWithOptions
   :: Options
   -> MisoString
   -> Decoder r
   -> (r -> action)
   -> Attribute action
onWithOptions _ _ _ _ = E ()

-- | Constructs `CSS` for a DOM Element
--
-- > import qualified Data.Map as M
-- > div_ [ style_  $ M.singleton "background" "red" ] [ ]
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS>
--
style_ :: M.Map MisoString MisoString -> Attribute action
style_ = C "style" . M.foldrWithKey go mempty
  where
    go :: MisoString -> MisoString -> MisoString -> MisoString
    go k v xs = mconcat [ k, ":", v, ";" ] <> xs
