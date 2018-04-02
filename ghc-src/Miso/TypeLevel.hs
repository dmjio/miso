{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Miso.TypeLevel ( CanMapHandlers, MapApi, MapHandlers, ToServerRoutes, mapHandlers ) where

import Miso.Html
import Miso.TypeLevel.Internal
import Servant.API
import Servant.HTML.Lucid

-- | Convert client route type to a server web handler type
type family ToServerRoutes (layout :: k) (wrapper :: * -> *) (action :: *) :: k where
  ToServerRoutes (a :<|> b) wrapper action =
    ToServerRoutes a wrapper action :<|>
      ToServerRoutes b wrapper action
  ToServerRoutes (a :> b) wrapper action =
    a :> ToServerRoutes b wrapper action
  ToServerRoutes (View a) wrapper action =
    Get '[HTML] (wrapper (View action))
