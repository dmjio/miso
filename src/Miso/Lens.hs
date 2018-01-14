{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lens
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Lens
  ( Lens
  , Lens'
  , Getting
  , get
  , set
  , makeLens
  ) where

import Data.Functor.Identity
import Control.Applicative

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
type Getting r s a = (a -> Const r a) -> (s -> Const r s)

get :: Getting a s a -> s -> a
get l = \ s -> getConst (l Const s)

set :: Lens s t a b -> b -> s -> t
set l b = \s -> runIdentity (l (\ _ -> pure b) s)

makeLens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
makeLens get' upd = \ f s -> upd s `fmap` f (get' s)
