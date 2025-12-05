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
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso.Types
-----------------------------------------------------------------------------
-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM
-- node associated with the vnode to @v@.
prop :: ToJSVal a => MisoString -> a -> Attribute action
prop k v = Property k (toJSVal v)
-----------------------------------------------------------------------------
-- | Set field to 'Bool' value
boolProp :: MisoString -> Bool -> Attribute action
boolProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'String' value
stringProp ::  MisoString -> String -> Attribute action
stringProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'MisoString' value
textProp ::  MisoString -> MisoString -> Attribute action
textProp = prop
-----------------------------------------------------------------------------
-- | Set field to t'Object' value
objectProp ::  MisoString -> Object -> Attribute action
objectProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'Int' value
intProp ::  MisoString -> Int -> Attribute action
intProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'Integer' value
integerProp ::  MisoString -> Integer -> Attribute action
integerProp k v = doubleProp k (fromIntegral v)
-----------------------------------------------------------------------------
-- | Set field to 'Double' value
doubleProp ::  MisoString -> Double -> Attribute action
doubleProp = prop
-----------------------------------------------------------------------------
-- | Set 'Miso.Types.Key' on 'VNode'.
keyProp :: ToKey key => key -> Attribute action
keyProp key = prop "key" (toKey key)
-----------------------------------------------------------------------------
-- | Synonym for 'keyProp'
-- Allows a user to specify a t'Key' inside of an '[Attribute action]'
key_ :: ToKey key => key -> Attribute action
key_ = keyProp
-----------------------------------------------------------------------------
