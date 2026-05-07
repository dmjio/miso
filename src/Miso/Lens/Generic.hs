-----------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lens.Generic
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.Lens.Generic (HasLens(..), field) where

-----------------------------------------------------------------------------
import Data.Kind (Constraint, Type)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Meta (..), Rec0, S1, (:*:) (..), (:+:) (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)
-----------------------------------------------------------------------------
import Miso.Lens (Lens, lens)
-----------------------------------------------------------------------------

class Generic s => HasLens (name :: Symbol) s a | name s -> a where 
  getLens :: Lens s a

instance 
  (HasField name s a, TotalityCheck name s a (GetFieldType name (Rep s)), GSet name a (Rep s), Generic s) => 
  HasLens name s a where
  getLens = lens (getField @name) (\s v -> to . gSet @name v . from $ s)
  {-# INLINE getLens #-}

instance HasLens name s a => IsLabel name (Lens s a)
  where fromLabel = getLens @name

{-# INLINE field #-}
field :: forall name s a. HasLens name s a => Lens s a 
field = fromLabel @name

class GSet (name :: Symbol) typ f where
  gSet :: typ -> f x -> f x

instance (GSet name typ a, GSet name typ b) => GSet name typ (a :*: b) where
  gSet v (l :*: r) = gSet @name v l :*: gSet @name v r
  {-# INLINE gSet #-}

instance (GSet name typ a, GSet name typ b) => GSet name typ (a :+: b) where
  gSet v (L1 l) = L1 $ gSet @name v l
  gSet v (R1 r) = R1 $ gSet @name v r
  {-# INLINE gSet #-}

instance (GSet name typ f) => GSet name typ (C1 x f) where
  gSet v (M1 f) = M1 $ gSet @name v f
  {-# INLINE gSet #-}

instance (GSet name typ f) => GSet name typ (D1 x f) where
  gSet v (M1 f) = M1 $ gSet @name v f
  {-# INLINE gSet #-}

instance {-# OVERLAPPING #-} GSet name typ (S1 ('MetaSel ('Just name) b c d) (Rec0 typ)) where
  gSet v (M1 (K1 _)) = M1 (K1 v)

instance {-# OVERLAPPABLE #-} GSet name typ (S1 ('MetaSel ('Just anotherName) b c d) x) where
  gSet _ f = f
  {-# INLINE gSet #-}

type family TotalityCheck (name :: Symbol) r a (res :: Maybe Type) :: Constraint where
  TotalityCheck _ _ _ ('Just _) = ()
  TotalityCheck name r a 'Nothing =
    TypeError
      ( 'ShowType r
          ':<>: 'Text ": "
          ':<>: 'Text name
          ':<>: 'Text " field missing or not in all constructors"
      )

type family GetFieldType (field :: Symbol) f :: Maybe Type where
  GetFieldType field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t)) ='Just t
  GetFieldType field (l :*: r) = Or (GetFieldType field l) (GetFieldType field r)
  GetFieldType field (l :+: r) = And (GetFieldType field l) (GetFieldType field r)
  GetFieldType field (C1 _ f) = GetFieldType field f
  GetFieldType field (D1 _ f) = GetFieldType field f
  GetFieldType field x = 'Nothing

type family And (l :: Maybe Type) (r :: Maybe Type) :: Maybe Type where
  And ('Just a) ('Just a) = 'Just a
  And l r = 'Nothing

type family Or (l :: Maybe Type) (r :: Maybe Type) :: Maybe Type where
  Or ('Just l) _ = 'Just l
  Or _ r = r
