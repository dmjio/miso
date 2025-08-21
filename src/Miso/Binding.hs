-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveFunctor              #-}
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
import Miso.Lens (Lens(..), Lens', Setter, Getter)
----------------------------------------------------------------------------
-- | Type used for React-like "props" functionality. This is used to
-- to bind parent model changes to the child model, or vice versa.
--
-- The difference between miso and React here is that miso is
-- synchronizing model states of Components declaratively (outside of the
-- view). In React "props" are used in the view code.
--
-- <https://react.dev/learn/passing-props-to-a-component>
--
-- This can be thought of as establishing an "edge" in the 'Component' graph,
-- whereby events cause model change synchronization to "ripple" or "pulsate"
-- through the views. The "reactivity" of the graph is constructed manually
-- by the end-user, using the edge primitives `-->`, `<--`, `<-->` (reactive combinators).
--
-- This can also be thought of as a "Wire" (from `netwire`) for reactive
-- variable synchronization, except done at the granularity specified by the `Lens`.
--
-- @
--
-- main :: IO ()
-- main = run app { bindings = [ parentLens <--> childLens ] }
--
-- @
--
-- @since 1.9.0.0
data Binding parent child
  = forall field . ParentToChild (Getter parent field) (Setter child field)
  | forall field . ChildToParent (Setter parent field) (Getter child field)
  | forall field . Bidirectional (Getter parent field) (Setter parent field) (Getter child field) (Setter child field)
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
