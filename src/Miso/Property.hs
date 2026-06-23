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
-- = Overview
--
-- "Miso.Property" provides the low-level primitives for constructing
-- 'Miso.Types.Attribute' values that set DOM properties on virtual nodes.
-- All higher-level property combinators in "Miso.Html.Property",
-- "Miso.Svg.Property", and "Miso.Mathml.Property" are built on top of
-- these.
--
-- The central combinator is 'prop':
--
-- @
-- 'prop' :: 'Miso.JSON.ToJSON' a => 'Miso.String.MisoString' -> a -> 'Miso.Types.Attribute' action
-- @
--
-- It wraps any JSON-serialisable value as a 'Miso.Types.Property' node,
-- which the virtual DOM diffs and writes to the DOM node only when the
-- value changes.
--
-- = Typed convenience wrappers
--
-- ['textProp'] 'Miso.String.MisoString' — @'textProp' \"placeholder\" \"…\"@
-- ['stringProp'] 'String' — @'stringProp' \"lang\" \"en\"@
-- ['boolProp'] 'Bool' — @'boolProp' \"checked\" True@
-- ['intProp'] 'Int' — @'intProp' \"tabIndex\" 3@
-- ['integerProp'] 'Integer' — @'integerProp' \"size\" 10@
-- ['doubleProp'] 'Double' — @'doubleProp' \"volume\" 0.8@
-- ['objectProp'] 'Miso.JSON.Types.Object' — @'objectProp' \"dataset\" obj@
--
-- = Class list
--
-- 'classList' stores CSS class names as a deduplicated list rather than a
-- single concatenated string. The virtual DOM diffing engine handles the
-- list directly so individual class additions and removals are efficient:
--
-- @
-- 'classList' [\"btn\", \"btn-primary\"]
-- @
--
-- = Virtual DOM keys
--
-- 'key_' (alias 'keyProp') attaches a reconciliation key to a node, telling
-- the differ which old and new nodes correspond to each other in a dynamic
-- list:
--
-- @
-- 'Miso.Html.Element.ul_' []
--   [ 'Miso.Html.Element.li_' [ 'key_' item.id ] [ 'Miso.text' item.label ]
--   | item <- items
--   ]
-- @
--
-- = See also
--
-- * "Miso.Html.Property" — named HTML property combinators built on this module
-- * "Miso.Svg.Property" — SVG property combinators
-- * "Miso.Mathml.Property" — MathML property combinators
-- * "Miso.Types" — 'Miso.Types.Attribute', 'Miso.Types.Key', 'Miso.Types.ToKey'
-----------------------------------------------------------------------------
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
-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM
-- node associated with the vnode to @v@.
prop
  :: ToJSON a
  => MisoString
  -- ^ DOM property name (e.g. @\"value\"@, @\"className\"@)
  -> a
  -- ^ Property value; serialised to JSON before diffing
  -> Attribute action
prop k v = Property k (toJSON v)
-----------------------------------------------------------------------------
-- | Set field to 'Bool' value
boolProp
  :: MisoString
  -- ^ DOM property name
  -> Bool
  -- ^ Property value
  -> Attribute action
boolProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'String' value
stringProp
  :: MisoString
  -- ^ DOM property name
  -> String
  -- ^ Property value
  -> Attribute action
stringProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'MisoString' value
textProp
  :: MisoString
  -- ^ DOM property name
  -> MisoString
  -- ^ Property value
  -> Attribute action
textProp = prop
-----------------------------------------------------------------------------
-- | Set field to t'Object' value
objectProp
  :: MisoString
  -- ^ DOM property name
  -> Object
  -- ^ JSON object value
  -> Attribute action
objectProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'Int' value
intProp
  :: MisoString
  -- ^ DOM property name
  -> Int
  -- ^ Property value
  -> Attribute action
intProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'Integer' value
integerProp
  :: MisoString
  -- ^ DOM property name
  -> Integer
  -- ^ Property value
  -> Attribute action
integerProp = prop
-----------------------------------------------------------------------------
-- | Set field to 'Double' value
doubleProp
  :: MisoString
  -- ^ DOM property name
  -> Double
  -- ^ Property value
  -> Attribute action
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
-- | Smart constructor for specifying 'class'
--
-- @since 1.9.0.0
classList :: [MisoString] -> Attribute action
classList = ClassList
-----------------------------------------------------------------------------
