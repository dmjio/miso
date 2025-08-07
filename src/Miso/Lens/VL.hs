{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Miso.Lens.VL where

import Control.Monad.Identity
import Data.Functor.Contravariant
import Data.Kind
import Miso.Lens
import Miso.Types
import Control.Applicative

-- | Van Laarhoven formulation, used for conversion w/ 'miso' @Lens@.
type Lens' s a = forall (f :: Type -> Type). (Functor f) => (a -> f a) -> s -> f s
type Getter' record field = forall (f :: Type -> Type). (Contravariant f, Functor f) => (field -> f field) -> record -> f record
type Setter' record field = (field -> Identity field) -> record -> Identity record

----------------------------------------------------------------------------

-- | Convert from `miso` @Lens@ to Van Laarhoven @Lens'@
lensFromVL ::
    Lens' record field ->
    Lens record field
lensFromVL lens_ = Lens{..}
  where
    _get = getterFromVL lens_
    _set = setterFromVL lens_
getterFromVL ::
    Getter' record field ->
    Getter record field
getterFromVL lens_ record = getConst (lens_ Const record)
setterFromVL ::
    Setter' record field ->
    Setter record field
setterFromVL lens_ field = runIdentity . lens_ (\_ -> Identity field)

{- | Bidirectionally binds a child field to a parent field, using @Lens'@

This is a bidirectional reactive combinator for a Van Laarhoven @Lens'@

@since 1.9.0.0
-}
(<--->) :: Lens' parent field -> Lens' child field -> Binding parent child
p <---> c = Bidirectional (_get p') (_set p') (_get c') (_set c')
  where
    p' = lensFromVL p
    c' = lensFromVL c
(--->) :: Getter' parent field -> Setter' child field -> Binding parent child
p ---> c = ParentToChild (getterFromVL p) (setterFromVL c)
(<---) :: Setter' parent field -> Getter' child field -> Binding parent child
p <--- c = ChildToParent (setterFromVL p) (getterFromVL c)
