{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Miso.TypeLevel.Internal (CanMapHandlers, MapApi, MapHandlers, mapHandlers) where

import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Symbol)
import Servant.API

import Miso.Html (View)

type family MapApi (a :: *) (layout :: k) :: k where
    MapApi a (x :<|> y) = MapApi a x :<|> MapApi a y
    MapApi a (x :> y) = x :> MapApi a y
    MapApi a (View b) = a
    MapApi a Raw = a

type family MapHandlers (a :: *) (layout :: k) :: * where
    MapHandlers a (x :<|> y) = MapHandlers a x :<|> MapHandlers a y
    MapHandlers a (Capture sym x :> y) = x -> MapHandlers a y
    MapHandlers a (QueryParam sym x :> y) = Maybe x -> MapHandlers a y
    MapHandlers a (QueryParams sym x :> y) = [x] -> MapHandlers a y
    MapHandlers a (QueryFlag sym :> y) = Bool -> MapHandlers a y
    MapHandlers a (sym :> y) = MapHandlers a y
    MapHandlers a (View b) = a
    MapHandlers a Raw = a

class CanMapHandlers (layout :: k) where
    mapHandlers :: Proxy layout -> (a -> b) -> MapHandlers a layout -> MapHandlers b layout

instance (CanMapHandlers x, CanMapHandlers y) => CanMapHandlers (x :<|> y) where
    mapHandlers _ f (x :<|> y) = mapHandlers (Proxy :: Proxy x) f x
                          :<|> mapHandlers (Proxy :: Proxy y) f y

instance CanMapHandlers y => CanMapHandlers (Capture sym (x :: *) :> y) where
    mapHandlers _ f g = mapHandlers (Proxy :: Proxy y) f . g

instance CanMapHandlers y => CanMapHandlers (QueryParam sym (x :: *) :> y) where
    mapHandlers _ f g = mapHandlers (Proxy :: Proxy y) f . g

instance CanMapHandlers y => CanMapHandlers (QueryParams sym (x :: *) :> y) where
    mapHandlers _ f g = mapHandlers (Proxy :: Proxy y) f . g

instance CanMapHandlers y => CanMapHandlers (QueryFlag sym :> y) where
    mapHandlers _ f g = mapHandlers (Proxy :: Proxy y) f . g

instance CanMapHandlers y => CanMapHandlers ((sym :: Symbol) :> y) where
    mapHandlers _ f g = mapHandlers (Proxy :: Proxy y) f g

instance CanMapHandlers (View a) where
    mapHandlers _ f g = f g

instance CanMapHandlers Raw where
    mapHandlers _ f g = f g
