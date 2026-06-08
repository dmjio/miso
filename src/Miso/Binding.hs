-----------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Binding
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Data Bindings
--
-- miso includes an experimental feature that allows fields of different models to be synchronized
-- against each another in response to model changes. See 'Miso.Binding'. Note this feature is
-- experimental, it is recommended to use asynchronous Component communication (like 'broadcast') by default.
--
-- This module exposes combinators to construct a 'Binding' which holds two lenses that will alter
-- t'Component' model state along the parent-child relationship using a 'Lens'. Practically, this means when
-- one t'Component' is marked as dirty, another t'Component' will also potentially will be marked as
-- dirty if they are connected along an edge ('Binding').
--
-- See the [miso-reactive](https://github.com/haskell-miso/miso-reactive) project for more information.
--
----------------------------------------------------------------------------
module Miso.Binding
  ( -- ** Types
    Binding (..)
  , Precedence (..)
    -- ** Combinators
  , (<-->)
  , (<<-->)
  , (<-->>)
  , (<--)
  , (-->)
  , (<--->)
  , (<<--->)
  , (<--->>)
  , (<---)
  , (--->)
  ) where
----------------------------------------------------------------------------
import Data.Functor.Const (Const(..))
import Control.Monad.Identity (Identity(..))
----------------------------------------------------------------------------
import Miso.Lens (Lens, Lens', LensCore(..))
----------------------------------------------------------------------------
-- | t'Binding' is used to synchronize parent and child model changes at the granularity specified by a t'Miso.Lens.Lens'
--
-- This can be thought of as establishing an "edge" in the 'Miso.Types.Component' graph,
-- whereby events cause model change synchronization to "ripple" or "pulsate"
-- through the views. The "reactivity" of the graph is constructed manually
-- by the end-user, using the edge primitives `-->`, `<--`, `<-->` (reactive combinators).
--
-- @
-- main :: IO ()
-- main = run app { bindings = [ parentLens \<--\> childLens ] }
-- @
--
-- @since 1.9.0.0
data Binding parent child
  = forall field . ParentToChild (parent -> field) (field -> child -> child)
  | forall field . ChildToParent (field -> parent -> parent) (child -> field)
  | forall field . Bidirectional Precedence (parent -> field) (field -> parent -> parent) (child -> field) (field -> child -> child)
-----------------------------------------------------------------------------
-- | Data type used to express if the Child state should take precendence
-- over the parent state during 'Component' mount.
data Precedence = Child | Parent
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | Unidirectionally propagates a parent field's value into the child model on each update.
-- When the parent field changes, the child field is set to match.
--
-- @
-- bindings = [ parentName --> childName ]
-- @
--
-- @since 1.9.0.0
infixr 0 -->
(-->)
  :: Lens parent a
  -- ^ Lens into the parent model field to read from
  -> Lens model a
  -- ^ Lens into the child model field to write to
  -> Binding parent model
parent --> child = ParentToChild (_get parent) (_set child)
-----------------------------------------------------------------------------
-- | Unidirectionally propagates a child field's value into the parent model on each update.
-- When the child field changes, the parent field is set to match.
--
-- @
-- bindings = [ parentName <-- childName ]
-- @
--
-- @since 1.9.0.0
infixl 0 <--
(<--)
  :: Lens parent a
  -- ^ Lens into the parent model field to write to
  -> Lens model a
  -- ^ Lens into the child model field to read from
  -> Binding parent model
parent <-- child = ChildToParent (_set parent) (_get child)
-----------------------------------------------------------------------------
-- | Bidirectionally synchronises a parent field and a child field using a miso 'Lens'.
-- Parent state takes precedence on initial mount.
--
-- @
-- bindings = [ parentName \<--\> childName ]
-- @
--
-- @since 1.9.0.0
infix 0 <-->
(<-->)
  :: Lens parent field
  -- ^ Lens into the parent model field to synchronise
  -> Lens child field
  -- ^ Lens into the child model field to synchronise
  -> Binding parent child
p <--> c = Bidirectional Parent (_get p) (_set p) (_get c) (_set c)
-----------------------------------------------------------------------------
-- | Bidirectionally synchronises a parent field and a child field using a van Laarhoven 'Lens''.
-- Parent state takes precedence on initial mount.
--
-- @
-- bindings = [ parentName \<---\> childName ]
-- @
--
-- @since 1.9.0.0
infix 0 <--->
(<--->)
  :: Lens' parent field
  -- ^ Van Laarhoven lens into the parent model field
  -> Lens' child field
  -- ^ Van Laarhoven lens into the child model field
  -> Binding parent child
p <---> c = Bidirectional Parent (get_ p) (set_ p) (get_ c) (set_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
-- | Like '<--->' but explicitly biases to inherit 'Parent' state on 'Component' mount.
--
-- @since 1.10.0.0
(<--->>)
  :: Lens' parent field
  -- ^ Van Laarhoven lens into the parent model field
  -> Lens' child field
  -- ^ Van Laarhoven lens into the child model field
  -> Binding parent child
l <--->> r =
  case l <---> r of
    Bidirectional _ w x y z -> Bidirectional Parent w x y z
    _ -> error "impossible"
-----------------------------------------------------------------------------
-- | Like '<--->' but biases to inherit 'Child' state on 'Component' mount.
--
-- @since 1.10.0.0
(<<--->)
  :: Lens' parent field
  -- ^ Van Laarhoven lens into the parent model field
  -> Lens' child field
  -- ^ Van Laarhoven lens into the child model field
  -> Binding parent child
l <<---> r =
  case l <---> r of
    Bidirectional _ w x y z -> Bidirectional Child w x y z
    _ -> error "impossible"
-----------------------------------------------------------------------------
-- | Like '-->' but for van Laarhoven 'Lens'' style lenses.
-- Propagates the parent field into the child on each update.
--
-- @since 1.9.0.0
infixr 0 --->
(--->)
  :: Lens' parent field
  -- ^ Van Laarhoven lens into the parent model field to read from
  -> Lens' child field
  -- ^ Van Laarhoven lens into the child model field to write to
  -> Binding parent child
p ---> c = ParentToChild (get_ p) (set_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
-- | Like '<-->' but explicitly biases to inherit 'Parent' state on 'Component' mount.
--
-- @since 1.10.0.0
(<-->>)
  :: Lens parent field
  -- ^ Lens into the parent model field
  -> Lens child field
  -- ^ Lens into the child model field
  -> Binding parent child
l <-->> r =
  case l <--> r of
    Bidirectional _ w x y z -> Bidirectional Parent w x y z
    _ -> error "impossible"
-----------------------------------------------------------------------------
-- | Like '<-->' but biases to inherit 'Child' state on 'Component' mount.
--
-- @since 1.10.0.0
(<<-->)
  :: Lens parent field
  -- ^ Lens into the parent model field
  -> Lens child field
  -- ^ Lens into the child model field
  -> Binding parent child
l <<--> r =
  case l <--> r of
    Bidirectional _ w x y z -> Bidirectional Child w x y z
    _ -> error "impossible"
-----------------------------------------------------------------------------
-- | Like '<--' but for van Laarhoven 'Lens'' style lenses.
-- Propagates the child field into the parent on each update.
--
-- @since 1.9.0.0
infixl 0 <---
(<---)
  :: Lens' parent field
  -- ^ Van Laarhoven lens into the parent model field to write to
  -> Lens' child field
  -- ^ Van Laarhoven lens into the child model field to read from
  -> Binding parent child
p <--- c = ChildToParent (set_ p) (get_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
