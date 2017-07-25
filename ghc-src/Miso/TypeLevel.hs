{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Miso.TypeLevel ( ToServerRoutes ) where

import Miso.Html
import Servant.API
import Servant.HTML.Lucid

-- | Convert client route type to a server web handler type
type family ToServerRoutes (layout :: k) (wrapper :: * -> *) (action :: *) :: k where
  ToServerRoutes (a :<|> b) wrapper action =
    ToServerRoutes a wrapper action :<|>
      ToServerRoutes b wrapper action
  ToServerRoutes (a :> b) wrapper action =
    a :> ToServerRoutes b wrapper action
  ToServerRoutes (View _) wrapper action =
    Get '[HTML] (wrapper (View action))
