-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Construct custom properties on DOM elements
--
-- > div_ [ prop "id" "foo" ] [ ]
--
----------------------------------------------------------------------------
module Miso.Property
  ( -- *** Smart constructors
    textProp
  , stringProp
  , boolProp
  , intProp
  , integerProp
  , doubleProp
  , objectProp
  , prop
  , keyProp
  , key_
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson (ToJSON(..), Object)
-----------------------------------------------------------------------------
import           Miso.Types
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM 
-- node associated with the vnode to @v@.
prop :: ToJSON a => MisoString -> a -> Attribute action
prop k v = Property k (toJSON v)
-----------------------------------------------------------------------------
-- | Set field to `Bool` value
boolProp :: MisoString -> Bool -> Attribute action
boolProp = prop
-----------------------------------------------------------------------------
-- | Set field to `String` value
stringProp ::  MisoString -> String -> Attribute action
stringProp = prop
-----------------------------------------------------------------------------
-- | Set field to `MisoString` value
textProp ::  MisoString -> MisoString -> Attribute action
textProp = prop
-----------------------------------------------------------------------------
-- | Set field to `Object` value
objectProp ::  MisoString -> Object -> Attribute action
objectProp = prop
-----------------------------------------------------------------------------
-- | Set field to `Int` value
intProp ::  MisoString -> Int -> Attribute action
intProp = prop
-----------------------------------------------------------------------------
-- | Set field to `Integer` value
integerProp ::  MisoString -> Integer -> Attribute action
integerProp = prop
-----------------------------------------------------------------------------
-- | Set field to `Double` value
doubleProp ::  MisoString -> Double -> Attribute action
doubleProp = prop
-----------------------------------------------------------------------------
-- | Set `Key` on 'VNode'.
keyProp :: ToKey key => key -> Attribute action
keyProp key = prop "key" (toKey key)
-----------------------------------------------------------------------------
-- | Synonym for 'keyProp'
-- Allows a user to specify a 'Key' inside of an @[Attribute action]@
key_ :: ToKey key => key -> Attribute action
key_ = keyProp
-----------------------------------------------------------------------------
