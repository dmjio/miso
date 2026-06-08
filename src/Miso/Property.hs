-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Construct custom properties on DOM elements.
--
-- > div_ [ prop "id" "foo" ] [ ]
--
----------------------------------------------------------------------------
module Miso.Property
  ( -- *** Smart constructors
    prop
  , classList
  , textProp
  , stringProp
  , boolProp
  , intProp
  , integerProp
  , doubleProp
  , objectProp
  , keyProp
  , key_
  ) where
-----------------------------------------------------------------------------
import           Miso.JSON (ToJSON(..), Object)
-----------------------------------------------------------------------------
import           Miso.Types
-----------------------------------------------------------------------------
-- | Set an attribute @k@ on a DOM node to value @v@.
--
-- @
-- div_ [ prop "id" "foo", prop "tabIndex" (1 :: Int) ] []
-- @
prop
  :: ToJSON a
  => MisoString
  -- ^ Attribute name
  -> a
  -- ^ Attribute value (encoded to JSON)
  -> Attribute action
prop k v = Property k (toJSON v)
-----------------------------------------------------------------------------
-- | Set a DOM attribute to a 'Bool' value.
boolProp
  :: MisoString
  -- ^ Attribute name
  -> Bool
  -- ^ Boolean value
  -> Attribute action
boolProp = prop
-----------------------------------------------------------------------------
-- | Set a DOM attribute to a 'String' value.
stringProp
  :: MisoString
  -- ^ Attribute name
  -> String
  -- ^ String value
  -> Attribute action
stringProp = prop
-----------------------------------------------------------------------------
-- | Set a DOM attribute to a t'MisoString' value.
textProp
  :: MisoString
  -- ^ Attribute name
  -> MisoString
  -- ^ Text value
  -> Attribute action
textProp = prop
-----------------------------------------------------------------------------
-- | Set a DOM attribute to a JSON t'Object' value.
objectProp
  :: MisoString
  -- ^ Attribute name
  -> Object
  -- ^ JSON object value
  -> Attribute action
objectProp = prop
-----------------------------------------------------------------------------
-- | Set a DOM attribute to an 'Int' value.
intProp
  :: MisoString
  -- ^ Attribute name
  -> Int
  -- ^ Integer value
  -> Attribute action
intProp = prop
-----------------------------------------------------------------------------
-- | Set a DOM attribute to an 'Integer' value.
integerProp
  :: MisoString
  -- ^ Attribute name
  -> Integer
  -- ^ Integer value
  -> Attribute action
integerProp = prop
-----------------------------------------------------------------------------
-- | Set a DOM attribute to a 'Double' value.
doubleProp
  :: MisoString
  -- ^ Attribute name
  -> Double
  -- ^ Double value
  -> Attribute action
doubleProp = prop
-----------------------------------------------------------------------------
-- | Set the t'Key' used for virtual DOM reconciliation on a t'VNode'.
keyProp
  :: ToKey key
  => key
  -- ^ Unique key for this node
  -> Attribute action
keyProp key = prop "key" (toKey key)
-----------------------------------------------------------------------------
-- | Synonym for 'keyProp'.
-- Allows specifying a t'Key' inside an @['Attribute' action]@ list.
key_
  :: ToKey key
  => key
  -- ^ Unique key for this node
  -> Attribute action
key_ = keyProp
-----------------------------------------------------------------------------
-- | Set multiple CSS classes at once using a list of class names.
--
-- @since 1.9.0.0
classList
  :: [MisoString]
  -- ^ List of CSS class names to apply
  -> Attribute action
classList = ClassList
-----------------------------------------------------------------------------
