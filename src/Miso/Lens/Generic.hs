{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Miso.Lens.Generic where

import Data.Kind (Constraint, Type)
import GHC.Generics (C1, D1, Generic (..), K1 (..), M1 (..), Meta (..), Rec0, S1, U1, V1, (:*:) (..), (:+:) (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeLits (Symbol)
import Miso.Lens (Lens, lens)

instance
  (HasField name s a, Generic s, GSet name a (Rep s), ErrorCheck name s a (HasFieldPred name (Rep s))) =>
  IsLabel name (Lens s a)
  where
  fromLabel :: (HasField name s a, Generic s, GSet name a (Rep s)) => Lens s a
  fromLabel = lens (getField @name) (\s v -> to . gSet @name v . from $ s)
  {-# INLINE fromLabel #-}


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


instance {-# OVERLAPPING #-} GSet name typ (S1 (MetaSel ('Just name) b c d) (Rec0 typ)) where
  gSet v (M1 (K1 _)) = M1 (K1 v)

instance {-# OVERLAPPABLE #-} GSet name typ (S1 (MetaSel ('Just anotherName) b c d) x) where
  gSet _ f = f
  {-# INLINE gSet #-}


type family ErrorCheck (name :: Symbol) r a (res :: Maybe Type) :: Constraint where
  ErrorCheck _ _ _ ('Just _) = ()
  ErrorCheck name r a 'Nothing =
    TypeError
      ( 'ShowType r
          ':<>: 'Text ": "
          ':<>: 'Text name
          ':<>: 'Text " field missing or not in all constructors"
      )

type family HasFieldPred (field :: Symbol) f :: Maybe Type where
  HasFieldPred field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t)) =
    'Just t
  HasFieldPred field (S1 _ _) = 'Nothing
  HasFieldPred field (l :*: r) = Alt (HasFieldPred field l) (HasFieldPred field r)
  HasFieldPred field (l :+: r) = Both (HasFieldPred field l) (HasFieldPred field r)
  HasFieldPred field (C1 _ f) = HasFieldPred field f
  HasFieldPred field (D1 _ f) = HasFieldPred field f
  HasFieldPred field (K1 _ _) = 'Nothing
  HasFieldPred field U1 = 'Nothing
  HasFieldPred field V1 = 'Nothing

type family Both (m1 :: Maybe Type) (m2 :: Maybe Type) :: Maybe Type where
  Both ('Just a) ('Just a) = 'Just a
  Both x y = 'Nothing

type family Alt (m1 :: Maybe Type) (m2 :: Maybe Type) :: Maybe Type where
  Alt ('Just a) _ = 'Just a
  Alt _ b = b
