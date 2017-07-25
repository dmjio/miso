{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
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
  ) where

import           Data.Aeson
import qualified Data.Map    as M
import           Data.Monoid
import           Data.Proxy
import           Data.Text   (Text)
import qualified Data.Text   as T
import qualified Data.Vector as V
import qualified Lucid       as L
import qualified Lucid.Base  as L
import           Servant.API

import           Miso.Event
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

instance Show (VTree action) where
  show = show . L.toHtml

-- | Converting `VTree` to Lucid's `L.Html`
instance L.ToHtml (VTree action) where
  toHtmlRaw = L.toHtml
  toHtml (VText x) | T.null x = L.toHtml (" " :: MisoString)
                   | otherwise = L.toHtml x
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
newtype View action = View { runView :: VTree action }

-- | For constructing type-safe links
instance HasLink (View a) where
  type MkLink (View a) = MkLink (Get '[] ())
  toLink _ = toLink (Proxy :: Proxy (Get '[] ()))

-- | Convenience class for using View
class ToView v where toView :: v -> View action

-- | Show `View`
instance Show (View action) where
  show (View xs) = show xs

-- | Converting `View` to Lucid's `L.Html`
instance L.ToHtml (View action) where
  toHtmlRaw = L.toHtml
  toHtml (View xs) = L.toHtml xs

-- | Namespace for element creation
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  deriving (Show, Eq)

-- | `VNode` creation
node :: NS -> MisoString -> Maybe Key -> [Attribute action] -> [View action] -> View action
node vNs vType vKey as xs =
  let vProps  = Props  $ M.fromList [ (k,v) | P k v <- as ]
      vChildren = V.fromList $ map runView xs
  in View VNode {..}

-- | `VText` creation
text :: ToMisoString str => str -> View action
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

-- | Properties
newtype Props = Props (M.Map MisoString Value)
  deriving (Show, Eq)

-- | `View` Attributes to annotate DOM, converted into Events, Props, Attrs and CSS
data Attribute action
  = P MisoString Value
  | E ()
  deriving (Show, Eq)

-- | DMJ: this used to get set on preventDefault on Options... if options are dynamic now what
-- | Useful for `drop` events
newtype AllowDrop = AllowDrop Bool
  deriving (Show, Eq)

-- | Constructs a property on a `VNode`, used to set fields on a DOM Node
prop :: ToJSON a => MisoString -> a -> Attribute action
prop k v = P k (toJSON v)

-- | For defining delegated events
--
-- > let clickHandler = on "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: MisoString
   -> Decoder r
   -> (r -> action)
   -> Attribute action
on _ _ _ = E ()

-- | For defining delegated events with options
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
onWithOptions _ _ _ _ = E ()

-- | Constructs CSS for a DOM Element
--
-- > import qualified Data.Map as M
-- > div_ [ style_  $ M.singleton "background" "red" ] [ ]
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS>
--
style_ :: M.Map MisoString MisoString -> Attribute action
style_ map' = P "style" $ String (M.foldrWithKey go mempty map')
  where
    go :: MisoString -> MisoString -> MisoString -> MisoString
    go k v xs = mconcat [ k, ":", v, ";" ] <> xs
