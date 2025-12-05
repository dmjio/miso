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
-- Construct custom properties on DOM elements.
--
-- > div_ [ prop "id" "foo" ] [ ]
--
----------------------------------------------------------------------------
module Miso.Property
  ( -- *** Smart constructors
    prop
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
import           Data.Aeson (ToJSON(..), Object)
-----------------------------------------------------------------------------
import           Miso.Types
-----------------------------------------------------------------------------
-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM
-- node associated with the vnode to @v@.
prop :: ToJSON value => MisoString -> value -> Attribute action
prop k v = Property k (JSONProp (toJSON v))
-----------------------------------------------------------------------------
-- | Set field to 'Bool' value
boolProp :: MisoString -> Bool -> Attribute action
boolProp k v = Property k (BoolProp v)
-----------------------------------------------------------------------------
-- | Set field to 'String' value
stringProp ::  MisoString -> String -> Attribute action
stringProp k v = Property k (TextProp (ms v))
-----------------------------------------------------------------------------
-- | Set field to 'MisoString' value
textProp ::  MisoString -> MisoString -> Attribute action
textProp k v = Property k (TextProp v)
-----------------------------------------------------------------------------
-- | Set field to t'Object' value
objectProp ::  MisoString -> Object -> Attribute action
objectProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'Int' value
intProp ::  MisoString -> Int -> Attribute action
intProp k v = Property k (IntProp v)
-----------------------------------------------------------------------------
-- | Set field to 'Integer' value
integerProp ::  MisoString -> Integer -> Attribute action
integerProp k v = doubleProp k (fromIntegral v)
-----------------------------------------------------------------------------
-- | Set field to 'Double' value
doubleProp ::  MisoString -> Double -> Attribute action
doubleProp k v = Property k (DoubleProp v)
-----------------------------------------------------------------------------
-- | Set 'Miso.Types.Key' on 'VNode'.
keyProp :: ToKey key => key -> Attribute action
keyProp key = textProp "key" (ms (toKey key))
-----------------------------------------------------------------------------
-- | Synonym for 'keyProp'
-- Allows a user to specify a t'Key' inside of an '[Attribute action]'
key_ :: ToKey key => key -> Attribute action
key_ = keyProp
-----------------------------------------------------------------------------
