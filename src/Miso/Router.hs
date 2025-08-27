-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Router
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module introduces a @Router@ that produces "correct-by-construction" URL
-- encoding and decoding from any Haskell algebraic data type. This @Router@ can be used
-- in conjunction with @uriSub@ or @routeSub@ to perform client-side routing. Further
-- it also supports the construction of type-safe links in any @View model action@ via
-- the @href_@ function exported from this module.
--
-- This module can be used in two ways, one is the manual construction of a @Router@
-- as seen below.
--
-- @
--
-- data Route = Widget Int
--    deriving (Show, Eq)
--
-- instance Router Route where
--   routeParser = routes [ Widget \<$\> (path "widget" *\> capture) ]
--   fromRoute (Widget value) = [ toPath "widget", toCapture value ]
--
-- main :: IO ()
-- main = print (runRouter "/widget/10" router)
--
-- > Right (Widget "widget" 10)
-- @
--
-- The second way is using the @Generic@ deriving mechanism. This should ensure that
--
-- @
--
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DeriveAnyClass     #-}
-- {-# LANGUAGE DeriveGeneric      #-}
--
-- data Route
--  = About
--  | Home
--  | Widget (Capture "thing" Int) (Path "foo") (Capture "other" MisoString) (QueryParam "bar" Int)
--  deriving stock (Generic, Show)
--  deriving anyclass Router
-- @
--
-- The @Generic@ deriving works by converting the constructor name to a path so
--
-- > test :: Either RoutingError Route
-- > test = toRoute "/widget/23/foo/okay?bar=0"
--
-- Decodes as
--
-- > Right (Widget (Capture 23) (Path "foo") (Capture "okay") (QueryParam (Just 0)))
--
-- The order of `Capture` and `Path` matters when defined on your sum type. The order of `QueryParam` and `QueryFlag` does not.
--
-- The router is "reversible" which means it can produce type-safe links using the `href_` function.
--
-- > prettyRoute $ Widget (Capture 23) (Path ("foo")) (Capture ("okay")) (QueryParam (Just 0))
-- > "/widget/23/foo/okay?bar=0"
--
-- This can be used in conjunction with the @href_@ field below to embed type safe links into 'miso' @View action@ code.
--
-- > button_ [ Miso.Router.href_ (Widget 10) ] [ "click me" ]
--
-- Note: the `Index` constructor is name special, it used to encode the `"/"` path.
--
-- @
--
-- data Route = Index
--   deriving stock (Show, Eq)
--   deriving anyclass (Router)
--
-- main :: IO ()
-- main = print (toRoute Index)
--
-- -- "/"
-- @
--
-----------------------------------------------------------------------------
module Miso.Router
  ( -- ** Classes
    Router (..)
    -- ** Types
  , Capture (..)
  , Path (..)
  , QueryParam (..)
  , QueryFlag (..)
  , Token (..)
  , URI (..)
    -- ** Re-exports
  , URI (..)
    -- ** Errors
  , RoutingError (..)
    -- ** Functions
  , parseURI
  , prettyURI
  , prettyQueryString
    -- ** Manual Routing
  , runRouter
  , routes
    -- ** Construction
  , toQueryFlag
  , toQueryParam
  , toCapture
  , toPath
  , emptyURI
    -- ** Parser combinators
  , queryFlag
  , queryParam
  , capture
  , path
  ) where
-----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Functor
import           Data.Proxy
import           Data.Char
import qualified Data.Char as C
import           Data.String
import           Control.Applicative
import           Control.Monad
import           GHC.Generics
import           GHC.TypeLits
-----------------------------------------------------------------------------
import           Miso.Types hiding (model)
import           Miso.Util
import qualified Miso.Html.Property as P
import           Miso.Util.Parser hiding (NoParses)
import qualified Miso.Util.Lexer as L
import           Miso.Util.Lexer (Lexer)
import           Miso.String (ToMisoString, FromMisoString, fromMisoStringEither)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
-- dmj: used for sanity checks
--
-- data Routel
--   = Index
--   | FooBah (Path "whohah") (Capture "foo" Int)
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (Router)
--
-- main :: IO ()
-- main = do
--   print (toRoute @Routel "/")
--   print (prettyURI (toURI Index))
-----------------------------------------------------------------------------
newtype Capture sym a = Capture a
  deriving stock (Generic, Eq, Show)
  deriving newtype (ToMisoString, FromMisoString)
-----------------------------------------------------------------------------
newtype Path (path :: Symbol) = Path MisoString
  deriving (Generic, Eq, Show)
  deriving newtype (ToMisoString, IsString)
-----------------------------------------------------------------------------
newtype QueryFlag (path :: Symbol) = QueryFlag Bool
  deriving (Generic, Eq, Show)
-----------------------------------------------------------------------------
newtype QueryParam (path :: Symbol) a = QueryParam (Maybe a)
  deriving (Generic, Eq, Show)
-----------------------------------------------------------------------------
instance (ToMisoString a, KnownSymbol path) => ToMisoString (QueryParam path a) where
  toMisoString (QueryParam maybeVal) =
    maybe mempty (\param -> "?" <> param <> "=" <> val) (ms <$> maybeVal)
      where
        val = ms $ symbolVal (Proxy @path)
-----------------------------------------------------------------------------
instance (FromMisoString a, KnownSymbol path) => FromMisoString (QueryParam path a) where
  fromMisoStringEither x =
    case fromMisoStringEither @a x of
      Right r -> Right $ QueryParam (Just r)
      Left v -> Left v
-----------------------------------------------------------------------------
instance KnownSymbol name => ToMisoString (QueryFlag name) where
  toMisoString = \case
    QueryFlag True ->
      "?" <> ms (symbolVal (Proxy @name))
    QueryFlag False ->
      mempty
-----------------------------------------------------------------------------
data Token
  = QueryParamTokens [(MisoString, MisoString)]
  | QueryParamToken MisoString MisoString
  | QueryFlagToken MisoString
  | CaptureOrPathToken MisoString
  | FragmentToken MisoString
  | IndexToken
  deriving (Show, Eq)
-----------------------------------------------------------------------------
toQueryParam :: ToMisoString s => MisoString -> s -> Token
toQueryParam k v = QueryParamToken k (ms v)
-----------------------------------------------------------------------------
toQueryFlag :: MisoString -> Token
toQueryFlag = QueryFlagToken
-----------------------------------------------------------------------------
toCapture :: ToMisoString string => string -> Token
toCapture = CaptureOrPathToken . ms
-----------------------------------------------------------------------------
toPath :: MisoString -> Token
toPath = CaptureOrPathToken
-----------------------------------------------------------------------------
-- | Converts a list of @[Token]@ into an actual @URI@.
tokensToURI :: [Token] -> URI
tokensToURI tokens = URI
  { uriPath =
      foldMap ms (filter isPathRelated tokens)
  , uriQueryString =
      M.unions
        [ case queryToken of
            QueryFlagToken k ->
              M.singleton k Nothing
            QueryParamTokens queryParams_ ->
              M.fromList
              [ (k, Just v)
              | (k,v) <- queryParams_
              ]
            QueryParamToken k v ->
              M.singleton k (pure v)
            _ ->
              mempty
        | queryToken <- filter isQuery tokens
        ]
  , uriFragment =
      foldMap ms (filter isFragment tokens)
  } where
      isFragment = \case
        FragmentToken{} -> True
        _ -> False
      isQuery = \case
        QueryFlagToken{} -> True
        QueryParamToken{} -> True
        _ -> False
      isPathRelated = \case
        CaptureOrPathToken {} -> True
        IndexToken {} -> True
        _ -> False
-----------------------------------------------------------------------------
instance ToMisoString Token where
  toMisoString = \case
    CaptureOrPathToken x -> "/" <> x
    QueryFlagToken x -> "?" <> x
    FragmentToken x -> "#" <> x
    QueryParamTokens params ->
      "?" <> MS.intercalate "&"
        [ key <> "=" <> value
        | (key, value) <- params
        ]
    QueryParamToken k v ->
      "?" <> k <> "=" <> v
    IndexToken -> "/"
-----------------------------------------------------------------------------
data RoutingError
  = ParseError MisoString [Token]
  | AmbiguousParse MisoString [Token]
  | LexError MisoString MisoString
  | LexErrorEOF MisoString
  | NoParses MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
type RouteParser = ParserT URI [Token] []
-----------------------------------------------------------------------------
capture :: FromMisoString value => RouteParser value
capture = do
  CaptureOrPathToken capture_ <- captureOrPathToken
  case fromMisoStringEither capture_ of
    Left msg -> fail (fromMisoString (ms msg))
    Right token -> pure token
-----------------------------------------------------------------------------
path :: MisoString -> RouteParser MisoString
path specified = do
  CaptureOrPathToken parsed <- captureOrPathToken
  when (specified /= parsed) (fail "path")
  pure specified
-----------------------------------------------------------------------------
index :: MisoString -> RouteParser MisoString
index specified = do
  IndexToken <- indexToken
  when (specified /= "index") (fail "index")
  pure "/"
-----------------------------------------------------------------------------
parseURI :: MisoString -> Either MisoString URI
parseURI txt =
  case lexTokens txt of
    Left (L.LexerError err _) -> Left err
    Left (L.UnexpectedEOF eof) -> Left ("EOF: " <> ms (show eof))
    Right tokens -> Right (tokensToURI tokens)
-----------------------------------------------------------------------------
class Router route where
  fromRoute :: route -> [Token]
  default fromRoute :: (Generic route, GRouter (Rep route)) => route -> [Token]
  fromRoute = gFromRoute . from

  toURI :: route -> URI
  toURI = tokensToURI . fromRoute

  route :: URI -> Either RoutingError route
  route = toRoute . prettyURI

  href_ :: route -> Attribute action
  href_ = P.href_ . prettyRoute

  prettyRoute :: route -> MisoString
  prettyRoute = prettyURI . tokensToURI . fromRoute

  dumpURI :: route -> MisoString
  dumpURI = ms . show . tokensToURI . fromRoute

  toRoute :: MisoString -> Either RoutingError route
  toRoute input = parseRoute input routeParser

  routeParser :: RouteParser route
  default routeParser :: (Generic route, GRouter (Rep route)) => RouteParser route
  routeParser = to <$> gRouteParser
-----------------------------------------------------------------------------
-- | Smart constructor for building a @RouteParser@
--
-- @
--
-- data Route = Widget MisoString Int
--
-- instance Router Route where
--   routeParser = routes [ Widget <$> path "widget" <*> capture ]
--   fromRoute (Widget path value) = [ toPath path, toCapture value ]
--
-- router :: Router router => RouteParser router
-- router = routes [ Widget <$> path "widget" <*> capture ]
--
-- > Right (Widget "widget" 10)
-- @
--
-----------------------------------------------------------------------------
runRouter :: MisoString -> RouteParser route -> Either RoutingError route
runRouter = parseRoute
-----------------------------------------------------------------------------
routes :: [ RouteParser route ] -> RouteParser route
routes = foldr (<|>) empty
-----------------------------------------------------------------------------
prettyURI :: URI -> MisoString
prettyURI uri@URI {..} = "/" <> uriPath <> prettyQueryString uri <> uriFragment
-----------------------------------------------------------------------------
prettyQueryString :: URI -> MisoString
prettyQueryString URI {..} = queries <> flags
  where
    queries =
      MS.concat
      [ "?" <>
        MS.intercalate "&"
        [ k <> "=" <> v
        | (k, Just v) <- M.toList uriQueryString
        ]
      | any isJust (M.elems uriQueryString)
      ]
    flags = mconcat
        [ "?" <> k
        | (k, Nothing) <- M.toList uriQueryString
        ]
-----------------------------------------------------------------------------
class GRouter f where
  gFromRoute :: f route -> [Token]
  gRouteParser :: RouteParser (f route)
-----------------------------------------------------------------------------
instance GRouter next => GRouter (D1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser = M1 <$> gRouteParser
-----------------------------------------------------------------------------
instance (KnownSymbol name, GRouter next) => GRouter (C1 (MetaCons name x y) next) where
  gFromRoute (M1 x) =
    case name of
      "index" -> [IndexToken]
      _ -> CaptureOrPathToken name : gFromRoute x
      where
        name = lowercase $ symbolVal (Proxy @name)
  gRouteParser = do
    case name of
      "index" -> do
        void (index name)
        M1 <$> gRouteParser
      _ -> do
        void (path name)
        M1 <$> gRouteParser
      where
        name = lowercaseStrip $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance GRouter next => GRouter (S1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser = M1 <$> gRouteParser
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall path m . KnownSymbol path => GRouter (K1 m (Path path)) where
  gFromRoute (K1 x) = pure $ CaptureOrPathToken (ms x)
  gRouteParser = K1 (Path chunk) <$ path chunk
    where
      chunk = ms $ symbolVal (Proxy :: Proxy path)
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} (FromMisoString a, ToMisoString a) => GRouter (K1 m (Capture sym a)) where
  gFromRoute (K1 x) = pure $ CaptureOrPathToken (ms x)
  gRouteParser = K1 <$> capture
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall param m a . (ToMisoString a, FromMisoString a, KnownSymbol param) =>
  GRouter (K1 m (QueryParam param a)) where
    gFromRoute (K1 (QueryParam maybeParam)) =
      case maybeParam of
        Nothing -> []
        Just v -> pure $ QueryParamToken (ms (symbolVal (Proxy @param))) (ms v)
    gRouteParser = K1 <$> queryParam
-----------------------------------------------------------------------------
queryParam
  :: forall param a . (FromMisoString a, KnownSymbol param)
  => RouteParser (QueryParam param a)
queryParam = do
  URI {..} <- askParser
  QueryParam <$> do
    case M.lookup (ms (symbolVal (Proxy @param))) uriQueryString of
      Just (Just value) ->
        case fromMisoStringEither value of
          Left _ -> pure Nothing
          Right parsed -> pure (Just parsed)
      _ -> pure Nothing
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall flag m . KnownSymbol flag => GRouter (K1 m (QueryFlag flag)) where
  gFromRoute (K1 (QueryFlag specified))
    | specified = [ QueryFlagToken flag ]
    | otherwise = []
        where
          flag = ms (symbolVal (Proxy @flag))
  gRouteParser = K1 <$> queryFlag
-----------------------------------------------------------------------------
queryFlag :: forall flag . KnownSymbol flag => RouteParser (QueryFlag flag)
queryFlag = do
  URI {..} <- askParser
  pure $ QueryFlag $ isJust (M.lookup flag uriQueryString)
    where
      flag = ms $ symbolVal (Proxy @flag)
-----------------------------------------------------------------------------
instance Router a => GRouter (K1 m a) where
  gFromRoute (K1 x) = fromRoute x
  gRouteParser = K1 <$> routeParser
-----------------------------------------------------------------------------
instance GRouter U1 where
  gFromRoute U1 = []
  gRouteParser = pure U1
-----------------------------------------------------------------------------
instance (GRouter left, GRouter right) => GRouter (left :*: right) where
  gFromRoute (left :*: right) = gFromRoute left <> gFromRoute right
  gRouteParser = liftA2 (:*:) gRouteParser gRouteParser
-----------------------------------------------------------------------------
instance (GRouter left, GRouter right) => GRouter (left :+: right) where
  gFromRoute = \case
    L1 m1 -> gFromRoute m1
    R1 m1 -> gFromRoute m1
  gRouteParser = foldr (<|>) empty
    [ L1 <$> gRouteParser
    , R1 <$> gRouteParser
    ]
-----------------------------------------------------------------------------
captureOrPathToken :: RouteParser Token
captureOrPathToken = satisfy $ \case
  CaptureOrPathToken {} -> True
  _ -> False
-----------------------------------------------------------------------------
indexToken :: RouteParser Token
indexToken = satisfy $ \case
  IndexToken {} -> True
  _ -> False
-----------------------------------------------------------------------------
uriLexer :: Lexer [Token]
uriLexer = do
  tokens <- some lexer
  void $ optional (L.char '/')
  pure (postProcess tokens)
    where
      postProcess :: [Token] -> [Token]
      postProcess = concatMap $ \case
        QueryParamTokens queryParams_ ->
          [ QueryParamToken k v
          | (k,v) <- queryParams_
          ]
        x -> pure x
      lexer = msum
        [ queryFlagLexer
        , captureOrPathLexer
        , queryParamLexer
        , fragmentLexer
        , indexLexer
        ] where
            indexLexer =
              IndexToken <$ L.char '/' 
            captureOrPathLexer = do
              void (L.char '/')
              CaptureOrPathToken <$> chars
            fragmentLexer = do
              void (L.char '#')
              FragmentToken <$> chars
            queryFlagLexer = do
              void (L.char '?')
              QueryFlagToken <$> chars
            queryParamLexer = QueryParamTokens <$> do
              void (L.char '?')
              sepBy (L.char '&') $ do
                key <- chars
                void (L.char '=')
                value <- chars
                pure (key, value)
-----------------------------------------------------------------------------
chars :: Lexer MisoString
chars = fmap ms <$> some $ do
  L.satisfy $ \x -> or
    [ isAlphaNum x
    , x == '-'
    , x == '_'
    , x == '~'
    , x == '%'
    , x == '.'
    ]
-----------------------------------------------------------------------------
lexTokens :: MisoString -> Either L.LexerError [Token]
lexTokens input =
  case L.runLexer uriLexer (L.mkStream input) of
    Right (tokens, _) -> Right tokens
    Left x -> Left x
-----------------------------------------------------------------------------
parseRoute :: MisoString -> RouteParser a -> Either RoutingError a
parseRoute input parser =
  case L.runLexer uriLexer (L.mkStream input) of
    Left (L.LexerError lexErrorMessage _) ->
      Left (LexError input lexErrorMessage)
    Left (L.UnexpectedEOF _) ->
      Left (LexErrorEOF input)
    Right (tokens, _) -> do
      let
        uri = tokensToURI tokens
        isCapturePathOrIndex = \case
          CaptureOrPathToken{} -> True
          IndexToken{} -> True
          _ -> False
      case runParserT parser uri (filter isCapturePathOrIndex tokens) of
        [(x, [])]  ->
          Right x
        [(_, leftovers)]  ->
          Left $ ParseError input leftovers
        []  ->
          Left $ NoParses input
        (_, leftovers) : _  ->
          Left $ AmbiguousParse input leftovers
-----------------------------------------------------------------------------
lowercase :: String -> MisoString
lowercase (x:xs) = ms (C.toLower x : xs)
lowercase x = ms x
-----------------------------------------------------------------------------
lowercaseStrip :: String -> MisoString
lowercaseStrip (x:xs) = ms (C.toLower x : takeWhile C.isLower xs)
lowercaseStrip x = ms x
-----------------------------------------------------------------------------
-- | Type for dealing with @URI@
--
-- <<https://datatracker.ietf.org/doc/html/rfc3986>>
--
data URI
  = URI
  { uriPath, uriFragment :: MisoString
  , uriQueryString :: M.Map MisoString (Maybe MisoString)
  } deriving (Eq)
----------------------------------------------------------------------------
emptyURI :: URI
emptyURI = URI mempty mempty mempty
----------------------------------------------------------------------------
instance Show URI where
  show = MS.unpack . prettyURI
----------------------------------------------------------------------------
instance ToMisoString URI where
  toMisoString = prettyURI
----------------------------------------------------------------------------
