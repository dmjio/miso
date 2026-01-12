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
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Primitives for synchronizing parent and child models.
----------------------------------------------------------------------------
module Miso.Binding
  ( -- ** Types
    Binding (..)
    -- ** Combinators
  , (<-->)
  , (<--)
  , (-->)
  , (<--->)
  , (<---)
  , (--->)
  ) where
----------------------------------------------------------------------------
import Data.Functor.Const (Const(..))
import Control.Monad.Identity (Identity(..))
----------------------------------------------------------------------------
import Miso.Lens (Lens(..), Lens')
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
  | forall field . Bidirectional (parent -> field) (field -> parent -> parent) (child -> field) (field -> child -> child)
-----------------------------------------------------------------------------
-- | Unidirectionally binds a parent field to a child field
--
-- @since 1.9.0.0
(-->) :: Lens parent a -> Lens model a -> Binding parent model
parent --> child = ParentToChild (_get parent) (_set child)
-----------------------------------------------------------------------------
-- | Unidirectionally binds a child field to a parent field
--
-- @since 1.9.0.0
(<--) :: Lens parent a  -> Lens model a -> Binding parent model
parent <-- child = ChildToParent (_set parent) (_get child)
-----------------------------------------------------------------------------
-- | Bidirectionally binds a child field to a parent field, using @Lens@
--
-- This is a bidirectional reactive combinator for a miso @Lens@.
--
-- @since 1.9.0.0
(<-->) :: Lens parent field -> Lens child field -> Binding parent child
p <--> c = Bidirectional (_get p) (_set p) (_get c) (_set c)
-----------------------------------------------------------------------------
-- | Bidirectionally binds a child field to a parent field, using @Lens'@
--
-- This is a bidirectional reactive combinator for a van Laarhoven @Lens'@
--
-- @since 1.9.0.0
(<--->) :: Lens' parent field -> Lens' child field -> Binding parent child
p <---> c = Bidirectional (get_ p) (set_ p) (get_ c) (set_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
-- | Unidirectionally binds a parent field to a child field, for van Laarhoven
-- style @Lens'@
--
-- @since 1.9.0.0
(--->) :: Lens' parent field -> Lens' child field -> Binding parent child
p ---> c = ParentToChild (get_ p) (set_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
-- | Unidirectionally binds a child field to a parent field, for van Laarhoven
-- style @Lens'@
--
-- @since 1.9.0.0
(<---) :: Lens' parent field -> Lens' child field -> Binding parent child
p <--- c = ChildToParent (set_ p) (get_ c)
  where
    get_ lens_ record = getConst (lens_ Const record)
    set_ lens_ field = runIdentity . lens_ (\_ -> Identity field)
-----------------------------------------------------------------------------
