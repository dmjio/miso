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
-- "Miso.Binding" provides an experimental mechanism for synchronizing
-- model fields between a parent and child t'Component' using
-- 'Miso.Lens.Lens'-based /bindings/. Rather than coordinating state
-- through explicit message passing, a 'Binding' declares a directed or
-- bidirectional edge between two model fields. When the runtime detects
-- that a bound field has changed, it propagates the new value to the
-- connected component automatically.
--
-- __Note:__ This feature is experimental. For production inter-component
-- communication, prefer asynchronous messaging via @broadcast@ or
-- "Miso.PubSub".
--
-- == How It Works
--
-- A 'Binding' encodes a directed /edge/ in the component graph. Each
-- edge is described by a pair of lenses — one projecting into the parent
-- model and one into the child model — that must share a common @field@
-- type. After each update cycle the runtime reads the source field
-- through its lens and writes it into the destination model through the
-- other lens, according to the edge direction.
--
-- == Example
--
-- Suppose a parent component tracks a user's name, and a child component
-- independently maintains a display name. Declaring a bidirectional
-- binding keeps both fields in sync automatically:
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
--
-- import "Miso"
-- import "Miso.Binding"
-- import "Miso.Lens.TH" ('Miso.Lens.TH.makeLenses')
-- import "Miso.String" ('Miso.String.MisoString')
--
-- data ParentModel = ParentModel { _userName    :: 'MisoString' } deriving 'Eq'
-- data ChildModel  = ChildModel  { _displayName :: 'MisoString' } deriving 'Eq'
--
-- 'Miso.Lens.makeLenses' ''ParentModel
-- 'Miso.Lens.makeLenses' ''ChildModel
--
-- -- Declare the child 'Component' with a bidirectional 'Binding'.
-- -- Changes to either field will be reflected in the other.
--
-- childComp :: 'Component' ParentModel () ChildModel ChildAction
-- childComp = ('component' initialChild updateChild viewChild)
--   { 'bindings' = [ userName @'<-->' displayName ] }
-- @
--
-- On mount, the parent field takes precedence by default (see 'Precedence').
-- To invert that behaviour, use @'<<-->'@ instead.
--
-- See the [miso-reactive](https://github.com/haskell-miso/miso-reactive)
-- project for extended examples.
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
-- | A 'Binding' encodes a directed or bidirectional synchronization edge
-- between a field in a parent model and a field in a child model. The
-- field is projected via a pair of lenses that must share a common type.
--
-- After each update cycle the runtime evaluates all bindings registered
-- in a t'Miso.Types.Component' and propagates field values along each edge according
-- to its direction. On initial mount the 'Precedence' value determines
-- which side wins when both fields carry different values.
--
-- Construct 'Miso.Types.bindings' using the operator combinators rather than the data
-- constructors directly. For miso 'Miso.Lens.Lens':
--
-- * @'-->'@      — parent→child
-- * @'<--'@      — child→parent
-- * @'<-->'@     — bidirectional ('Parent' precedence on mount)
--
-- For van Laarhoven 'Miso.Lens.Lens'':
--
-- * @'--->'@     — parent→child
-- * @'<---'@     — child→parent
-- * @'<--->'@    — bidirectional ('Parent' precedence on mount)
--
-- @since 1.9.0.0
data Binding parent child
  = forall field . ParentToChild (parent -> field) (field -> child -> child)
  | forall field . ChildToParent (field -> parent -> parent) (child -> field)
  | forall field . Bidirectional Precedence (parent -> field) (field -> parent -> parent) (child -> field) (field -> child -> child)
-----------------------------------------------------------------------------
-- | Determines which side of a 'Bidirectional' binding wins when parent
-- and child fields carry different values at t'Component' mount time.
-- After the initial mount both sides are kept in sync, so 'Precedence'
-- only affects the very first reconciliation.
data Precedence = Child | Parent
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | Constructs a unidirectional 'Binding' that propagates a field from
-- the parent model into the child model. Changes to the child field are
-- not reflected back to the parent.
--
-- Uses the miso 'Miso.Lens.Lens' representation. For van Laarhoven
-- lenses use @'--->'@.
--
-- @since 1.9.0.0
infixr 0 -->
(-->) :: Lens parent a -> Lens model a -> Binding parent model
parent --> child = ParentToChild (_get parent) (_set child)
-----------------------------------------------------------------------------
-- | Constructs a unidirectional 'Binding' that propagates a field from
-- the child model back into the parent model. Changes to the parent
-- field are not reflected into the child.
--
-- Uses the miso 'Miso.Lens.Lens' representation. For van Laarhoven
-- lenses use @'<---'@.
--
-- @since 1.9.0.0
infixl 0 <--
(<--) :: Lens parent a  -> Lens model a -> Binding parent model
parent <-- child = ChildToParent (_set parent) (_get child)
-----------------------------------------------------------------------------
-- | Constructs a bidirectional 'Binding' between a parent field and a
-- child field using the miso 'Miso.Lens.Lens' representation. Changes
-- to either field are propagated to the other after each update cycle.
--
-- On t'Component' mount, the parent field takes 'Precedence' over the
-- child field. Use @'<-->'@ to invert this and let the child win, or
-- @'<<-->'@ to state the parent precedence explicitly.
--
-- For van Laarhoven lenses use @'<--->'@.
--
-- @since 1.9.0.0
infix 0 <-->
(<-->) :: Lens parent field -> Lens child field -> Binding parent child
p <--> c = Bidirectional Parent (_get p) (_set p) (_get c) (_set c)
-----------------------------------------------------------------------------
-- | Constructs a bidirectional 'Binding' between a parent field and a
-- child field using the van Laarhoven 'Miso.Lens.Lens'' representation.
-- Changes to either field are propagated to the other after each update
-- cycle.
--
-- On t'Component' mount, the parent field takes 'Precedence' over the
-- child field. Use @'<<--->'@ to invert this and let the child win, or
-- @'<--->>'@ to state the parent 'Precedence' explicitly.
--
-- For miso lenses use @'<-->'@.
--
-- @since 1.9.0.0
infix 0 <--->
(<--->) :: Lens' parent field -> Lens' child field -> Binding parent child
p <---> c = Bidirectional Parent (get_ p) (set_ p) (get_ c) (set_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
-- | Like @'<--->'@ but explicitly sets 'Precedence' to 'Parent', so
-- the parent field overwrites the child field on t'Component' mount.
-- This is the default behaviour of @'<--->'@; use this combinator
-- when you want to be explicit about the 'Precedence'.
--
-- @since 1.10.0.0
(<--->>)
  :: Lens' parent field
  -> Lens' child field
  -> Binding parent child
l <--->> r =
  case l <---> r of
    Bidirectional _ w x y z -> Bidirectional Parent w x y z
    _ -> error "impossible"
-----------------------------------------------------------------------------
-- | Like @'<--->'@ but sets 'Precedence' to 'Child', so the child
-- field overwrites the parent field on t'Component' mount. Use this
-- when the child component owns the authoritative initial value for
-- the shared field.
--
-- @since 1.10.0.0
(<<--->)
  :: Lens' parent field
  -> Lens' child field
  -> Binding parent child
l <<---> r =
  case l <---> r of
    Bidirectional _ w x y z -> Bidirectional Child w x y z
    _ -> error "impossible"
-----------------------------------------------------------------------------
-- | Constructs a unidirectional 'Binding' that propagates a field from
-- the parent model into the child model. Changes to the child field are
-- not reflected back to the parent.
--
-- Uses the van Laarhoven 'Miso.Lens.Lens'' representation. For miso
-- lenses use @'-->'@.
--
-- @since 1.9.0.0
infixr 0 --->
(--->) :: Lens' parent field -> Lens' child field -> Binding parent child
p ---> c = ParentToChild (get_ p) (set_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
-- | Like @'<-->'@ but explicitly sets 'Precedence' to 'Parent', so
-- the parent field overwrites the child field on t'Component' mount.
-- This is the default behaviour of @'<-->'@ use this combinator
-- when you want to be explicit about the precedence.
--
-- @since 1.10.0.0
(<-->>)
  :: Lens parent field
  -> Lens child field
  -> Binding parent child
l <-->> r =
  case l <--> r of
    Bidirectional _ w x y z -> Bidirectional Parent w x y z
    _ -> error "impossible"
-----------------------------------------------------------------------------
-- | Like @'<-->'@ but sets 'Precedence' to 'Child', so the child
-- field overwrites the parent field on t'Component' mount. Use this
-- when the child component owns the authoritative initial value for
-- the shared field.
--
-- @since 1.10.0.0
(<<-->)
  :: Lens parent field
  -> Lens child field
  -> Binding parent child
l <<--> r =
  case l <--> r of
    Bidirectional _ w x y z -> Bidirectional Child w x y z
    _ -> error "impossible"
-----------------------------------------------------------------------------
-- | Constructs a unidirectional 'Binding' that propagates a field from
-- the child model back into the parent model. Changes to the parent
-- field are not reflected into the child.
--
-- Uses the van Laarhoven 'Miso.Lens.Lens'' representation. For miso
-- lenses use @'<--'@.
--
-- @since 1.9.0.0
infixl 0 <---
(<---) :: Lens' parent field -> Lens' child field -> Binding parent child
p <--- c = ChildToParent (set_ p) (get_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
