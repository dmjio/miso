-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Fetch
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Fetch
  ( -- * Class
    Fetch (fetch)
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits
import           Language.Javascript.JSaddle (JSM)
import           Servant.API
-----------------------------------------------------------------------------
import           Miso.FFI.Internal (fetchJSON)
import           Miso.Lens
import           Miso.String
-----------------------------------------------------------------------------
-- | Internal type used to accumulate options during the type-level traversal
data FetchOptions
  = FetchOptions
  { _baseUrl :: MisoString
  , _currentPath :: MisoString
  , _body :: Maybe MisoString
  , _headers :: [(MisoString, MisoString)]
  , _queryParams :: [(MisoString,MisoString)]
  , _queryFlags :: [MisoString]
  }
-----------------------------------------------------------------------------
baseUrl :: Lens FetchOptions MisoString
baseUrl = lens _baseUrl $ \record field -> record { _baseUrl = field }
-----------------------------------------------------------------------------
currentPath :: Lens FetchOptions MisoString
currentPath = lens _currentPath $ \record field -> record { _currentPath = field }
-----------------------------------------------------------------------------
body :: Lens FetchOptions (Maybe MisoString)
body = lens _body $ \record field -> record { _body = field }
-----------------------------------------------------------------------------
headers :: Lens FetchOptions [(MisoString,MisoString)]
headers = lens _headers $ \record field -> record { _headers = field }
-----------------------------------------------------------------------------
queryParams :: Lens FetchOptions [(MisoString,MisoString)]
queryParams = lens _queryParams $ \record field -> record { _queryParams = field }
-----------------------------------------------------------------------------
queryFlags :: Lens FetchOptions [MisoString]
queryFlags = lens _queryFlags $ \record field -> record { _queryFlags = field }
-----------------------------------------------------------------------------
defaultFetchOptions :: FetchOptions
defaultFetchOptions
  = FetchOptions
  { _headers = []
  , _baseUrl = mempty
  , _currentPath = ms "/"
  , _queryParams = []
  , _queryFlags = []
  , _body = Nothing
  }
-----------------------------------------------------------------------------
class Fetch (api :: Type) where
  type ToFetch api :: Type
  fetch :: Proxy api -> MisoString -> ToFetch api
  fetch proxy url = fetchWith proxy (defaultFetchOptions & baseUrl .~ url)
  fetchWith :: Proxy api -> FetchOptions -> ToFetch api
-----------------------------------------------------------------------------
instance (Fetch left , Fetch right) => Fetch (left :<|> right) where
  type ToFetch (left :<|> right) = ToFetch left :<|> ToFetch right
  fetchWith Proxy o = fetchWith (Proxy @left) o :<|> fetchWith (Proxy @right) o
-----------------------------------------------------------------------------
instance (Fetch api, KnownSymbol path) => Fetch (path :> api) where
  type ToFetch (path :> api) = ToFetch api
  fetchWith Proxy options = fetchWith (Proxy @api) options_
    where
      path :: MisoString
      path = ms $ symbolVal (Proxy @path)

      options_ :: FetchOptions
      options_ = options & currentPath %~ (<> ms "/" <> path)
-----------------------------------------------------------------------------
instance (Show a, Fetch api, KnownSymbol path) => Fetch (Capture path a :> api) where
  type ToFetch (Capture path a :> api) = a -> ToFetch api
  fetchWith Proxy options arg = fetchWith (Proxy @api) options_
    where
      options_ :: FetchOptions
      options_ = options & currentPath %~ (<> ms "/" <> ms (show arg))
-----------------------------------------------------------------------------
instance (Show a, Fetch api, KnownSymbol name) => Fetch (QueryParam name a :> api) where
  type ToFetch (QueryParam name a :> api) = a -> ToFetch api
  fetchWith Proxy options arg = fetchWith (Proxy @api) options_
    where
      options_ :: FetchOptions
      options_ = options & queryParams <>~ [(ms "/", ms (show arg))]
-----------------------------------------------------------------------------
instance (Fetch api, KnownSymbol name) => Fetch (QueryFlag name :> api) where
  type ToFetch (QueryFlag name :> api) = Bool -> ToFetch api
  fetchWith Proxy options flag = fetchWith (Proxy @api) options_
    where
      options_ :: FetchOptions
      options_ = options & queryFlags <>~ [ ms $ symbolVal (Proxy @name) | flag ]
-----------------------------------------------------------------------------
instance (ToJSON a, Fetch api) => Fetch (ReqBody '[JSON] a :> api) where
  type ToFetch (ReqBody '[JSON] a :> api) = a -> ToFetch api
  fetchWith Proxy options body_ = fetchWith (Proxy @api) (options_ (ms (encode body_)))
    where
      options_ :: MisoString -> FetchOptions
      options_ b = options & body ?~ b
-----------------------------------------------------------------------------
instance (KnownSymbol name, ToHttpApiData a, Fetch api) => Fetch (Header name a :> api) where
  type ToFetch (Header name a :> api) = a -> ToFetch api
  fetchWith Proxy options value = fetchWith (Proxy @api) o
    where
      headerName :: MisoString
      headerName = ms $ symbolVal (Proxy @name)

      o :: FetchOptions
      o = options & headers <>~ [ (headerName, ms (toHeader value)) ]
-----------------------------------------------------------------------------
instance (ReflectMethod method, FromJSON a) => Fetch (Verb method code content a) where
  type ToFetch (Verb method code content a) = (a -> JSM()) -> (MisoString -> JSM ()) -> JSM ()
  fetchWith Proxy options success_ error_ =
     fetchJSON url method (options ^. body) success_ error_
    where
      method = ms (reflectMethod (Proxy @method))
      url = options ^. baseUrl <> options ^. currentPath
-----------------------------------------------------------------------------
