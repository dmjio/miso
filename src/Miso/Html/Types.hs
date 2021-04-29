{-# LANGUAGE FlexibleInstances    #-}

module Miso.Html.Types (
    -- * Key patch internals
      Key    (..)
    , ToKey  (..)
    -- * Namespace
    , NS(..)
    ) where

import           Data.JSString (JSString)
import qualified Data.Text as T
import           Miso.String (MisoString, toMisoString)

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
