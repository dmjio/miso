{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Miso.TypeLevel.Internal (CanMapLeaves, ToApi, ToHandler, mapLeaves) where

import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Symbol)
import Servant.API

import Miso.Html (View)

type family ToApi (a :: *) (layout :: k) :: k where
    ToApi a (x :<|> y) = ToApi x a :<|> ToApi y a
    ToApi a (x :> y) = x :> ToApi y a
    ToApi a (View b) = a
    ToApi a Raw = a

-- ToHandlers?
type family ToHandler (a :: *) (layout :: k) :: * where
    ToHandler a (x :<|> y) = ToHandler a x :<|> ToHandler a y
    ToHandler a (Capture sym x :> y) = x -> ToHandler a y
    ToHandler a (QueryParam sym x :> y) = Maybe x -> ToHandler a y
    ToHandler a (QueryParams sym x :> y) = [x] -> ToHandler a y
    ToHandler a (QueryFlag sym :> y) = Bool -> ToHandler a y
    ToHandler a (sym :> y) = ToHandler a y
    ToHandler a (View b) = a
    ToHandler a Raw = a

-- CanMapHandlers?
class CanMapLeaves (layout :: k) where
    mapLeaves :: Proxy layout -> (a -> b) -> ToHandler a layout -> ToHandler b layout
    -- mapHandlers

instance (CanMapLeaves x, CanMapLeaves y) => CanMapLeaves (x :<|> y) where
    mapLeaves _ f (x :<|> y) = mapLeaves (Proxy :: Proxy x) f x
                          :<|> mapLeaves (Proxy :: Proxy y) f y

instance CanMapLeaves y => CanMapLeaves (Capture sym (x :: *) :> y) where
    mapLeaves _ f g = mapLeaves (Proxy :: Proxy y) f . g

instance CanMapLeaves y => CanMapLeaves (QueryParam sym (x :: *) :> y) where
    mapLeaves _ f g = mapLeaves (Proxy :: Proxy y) f . g

instance CanMapLeaves y => CanMapLeaves (QueryParams sym (x :: *) :> y) where
    mapLeaves _ f g = mapLeaves (Proxy :: Proxy y) f . g

instance CanMapLeaves y => CanMapLeaves (QueryFlag sym :> y) where
    mapLeaves _ f g = mapLeaves (Proxy :: Proxy y) f . g

instance CanMapLeaves y => CanMapLeaves ((sym :: Symbol) :> y) where
    mapLeaves _ f g = mapLeaves (Proxy :: Proxy y) f g

instance CanMapLeaves Raw where
    mapLeaves _ f g = f g
