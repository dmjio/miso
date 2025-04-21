-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Fetch
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Module for interacting with the Fetch API <https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API>
-- with a servant-style interface.
--
-- > import Miso (fetch)
-- > import Data.Proxy
-- > import Servant.API
-- >
-- > data Action
-- >  = FetchGitHub
-- >  | SetGitHub GitHub
-- >  | ErrorHandler MisoString
-- >  deriving (Show, Eq)
-- >
-- > type GithubAPI = Get '[JSON] GitHub
-- >
-- > getGithubAPI
-- >   :: (GitHub -> JSM ())
-- >   -- ^ Successful callback
-- >   -> (MisoString -> JSM ())
-- >   -- ^ Errorful callback
-- >   -> JSM ()
-- > getGithubAPI = fetch (Proxy @GithubAPI) "https://api.github.com"
-- >
-- > updateModel :: Action -> Effect Model Action ()
-- > updateModel FetchGitHub =
-- >   withSink $ \snk ->
-- >     getGithubAPI (snk . SetGitHub) (snk . ErrorHandler)
-- > updateModel (SetGitHub apiInfo) =
-- >   info ?= apiInfo
-- > updateModel (ErrorHandler msg) =
-- >   io (consoleError msg)
--
----------------------------------------------------------------------------
module Miso.Fetch
  ( -- * Class
    Fetch (..)
    -- ** Simple non-Servant API
  , fetchJSON
  ) where
-----------------------------------------------------------------------------
import           Data.Bifunctor (bimap)
import           Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import           Data.Either (partitionEithers)
import           Data.Kind (Type)
import           Data.Proxy (Proxy (Proxy))
import           Generics.SOP (All, (:.:) (Comp), NS (S), I (I))
import           Generics.SOP.NP (NP (..), cpure_NP)
import           GHC.TypeLits
import           Language.Javascript.JSaddle (JSM)
import           Network.HTTP.Media (renderHeader, MediaType)
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.API.Modifiers
-----------------------------------------------------------------------------
import           Miso.FFI.Internal (fetchJSON, fetchFFI)
import           Miso.Lens
import           Miso.String (MisoString, ms)
import qualified Miso.String as MS
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
  , _currentPath = mempty
  , _queryParams = []
  , _queryFlags = []
  , _body = Nothing
  }
-----------------------------------------------------------------------------
optionsToUrl :: FetchOptions -> MisoString
optionsToUrl options = options ^. baseUrl <> options ^. currentPath <> params <> flags
  where
    params = MS.concat
      [ mconcat
         [ ms "?"
         , MS.intercalate (ms "&")
           [ k <> ms "=" <> v
           | (k,v) <- options ^. queryParams
           ]
         ]
       | not $ null (options ^. queryParams)
       ]
    flags = MS.mconcat [ ms "?" <> k | k <- options ^. queryFlags ]
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
instance (ToHttpApiData a, Fetch api, KnownSymbol path) => Fetch (Capture' mods path a :> api) where
  type ToFetch (Capture' mods path a :> api) = a -> ToFetch api
  fetchWith Proxy options arg = fetchWith (Proxy @api) options_
    where
      options_ :: FetchOptions
      options_ = options & currentPath %~ (<> ms "/" <> ms (toEncodedUrlPiece arg))
-----------------------------------------------------------------------------
instance (ToHttpApiData a, Fetch api, SBoolI (FoldRequired mods), KnownSymbol name) => Fetch (QueryParam' mods name a :> api) where
  type ToFetch (QueryParam' mods name a :> api) = RequiredArgument mods a -> ToFetch api
  fetchWith Proxy options arg = fetchWith (Proxy @api) options_
    where
      param :: a -> [(MisoString, MisoString)]
      param x = [(ms $ symbolVal (Proxy @name), ms (enc x))]

#if MIN_VERSION_http_api_data(0,5,1)
      enc = toEncodedQueryParam
#else
      enc = toEncodedUrlPiece
#endif

      options_ :: FetchOptions
      options_ = options & queryParams <>~ foldRequiredArgument (Proxy @mods) param (foldMap param) arg
-----------------------------------------------------------------------------
instance (Fetch api, KnownSymbol name) => Fetch (QueryFlag name :> api) where
  type ToFetch (QueryFlag name :> api) = Bool -> ToFetch api
  fetchWith Proxy options flag = fetchWith (Proxy @api) options_
    where
      options_ :: FetchOptions
      options_ = options & queryFlags <>~ [ms $ symbolVal (Proxy @name) | flag]
-----------------------------------------------------------------------------
instance (MimeRender ct a, Fetch api) => Fetch (ReqBody' mods (ct ': cts) a :> api) where
  type ToFetch (ReqBody' mods (ct ': cts) a :> api) = a -> ToFetch api
  fetchWith Proxy options body_ = fetchWith (Proxy @api) options_
    where
      ctProxy :: Proxy ct
      ctProxy = Proxy

      options_ :: FetchOptions
      options_ = options
        & body ?~ (ms (mimeRender ctProxy body_))
        & headers <>~ [(ms "Content-Type", ms $ renderHeader $ contentType ctProxy)]
-----------------------------------------------------------------------------
instance (KnownSymbol name, ToHttpApiData a, Fetch api, SBoolI (FoldRequired mods)) => Fetch (Header' mods name a :> api) where
  type ToFetch (Header' mods name a :> api) = RequiredArgument mods a -> ToFetch api
  fetchWith Proxy options value = fetchWith (Proxy @api) options_
    where
      headerName :: MisoString
      headerName = ms $ symbolVal (Proxy @name)

      header :: a -> [(MisoString, MisoString)]
      header x = [(headerName, ms (toHeader x))]

      options_ :: FetchOptions
      options_ = options & headers <>~ foldRequiredArgument (Proxy @mods) header (foldMap header) value
-----------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} (ReflectMethod method, MimeUnrender ct a, cts' ~ (ct ': cts)) => Fetch (Verb method code cts' a) where
  type ToFetch (Verb method code cts' a) = (a -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
  fetchWith Proxy options success_ error_ =
    fetchFFI
      (mimeUnrender ctProxy . MS.fromMisoString)
      (optionsToUrl options)
      (ms $ reflectMethod (Proxy @method))
      (options ^. body)
      (options ^. headers <> [(ms "Accept", ms $ renderHeader $ contentType ctProxy)])
      success_
      error_
    where
      ctProxy = Proxy @ct
instance {-# OVERLAPPING #-} (ReflectMethod method) => Fetch (Verb method code cts NoContent) where
  type ToFetch (Verb method code cts NoContent) = (NoContent -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
  fetchWith Proxy options success_ error_ =
    fetchFFI
      (const $ pure NoContent)
      (optionsToUrl options)
      (ms $ reflectMethod (Proxy @method))
      (options ^. body)
      (options ^. headers)
      success_
      error_
-----------------------------------------------------------------------------
instance
  ( contentTypes ~ (ct ': otherContentTypes),
    as ~ (a ': as'),
    AllMime contentTypes,
    ReflectMethod method,
    All (UnrenderResponse contentTypes) as
  ) =>
  Fetch (UVerb method contentTypes as)
  where
  type ToFetch (UVerb method contentTypes as) = (Union as -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
  fetchWith Proxy options success_ error_ =
    fetchFFI
      (first unlines . tryParsers . mimeUnrenders . MS.fromMisoString)
      (optionsToUrl options)
      (ms $ reflectMethod (Proxy @method))
      (options ^. body)
      (options ^. headers <> map ((ms "Accept",) . ms . renderHeader) (allMime $ Proxy @contentTypes))
      success_
      error_
    where
      mimeUnrenders :: BSL.ByteString -> NP ([] :.: Either (MediaType, String)) as
      mimeUnrenders body_ = cpure_NP (Proxy @(UnrenderResponse contentTypes))
        $ Comp $ unrenderResponse (Proxy @contentTypes) body_
      tryParsers :: NP ([] :.: Either (MediaType, String)) xs -> Either [String] (Union xs)
      tryParsers = \case
        Nil -> Left ["no parsers"]
        Comp x :* xs -> case partitionEithers x of
          (err', []) -> bimap (map (\(t, s) -> show t <> ": " <> s) err' <>) S $ tryParsers xs
          (_, (res : _)) -> Right . inject . I $ res
class UnrenderResponse (cts :: [Type]) (a :: Type) where
  unrenderResponse :: Proxy cts -> BSL.ByteString -> [Either (MediaType, String) a]
instance AllMimeUnrender cts a => UnrenderResponse cts a where
  unrenderResponse Proxy body_ = allMimeUnrender @_ @a (Proxy @cts) <&> \(t, f) -> first (t,) $ f body_
-----------------------------------------------------------------------------
