{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Miso.TypeLevel ( ToServerRoutes ) where

import Data.Kind
import Servant.API
import Servant.HTML.Lucid

import Miso.Html

-- | Convert client route type to a server web handler type
type family ToServerRoutes (layout :: k) (wrapper :: Type -> Type) (action :: Type) :: k where
  ToServerRoutes (a :<|> b) wrapper action =
    ToServerRoutes a wrapper action :<|>
      ToServerRoutes b wrapper action
  ToServerRoutes (a :> b) wrapper action =
    a :> ToServerRoutes b wrapper action
  ToServerRoutes (View a) wrapper action =
    Get '[HTML] (wrapper (View action))
