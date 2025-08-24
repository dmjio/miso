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
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Router
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Introduces a Router that can produce "correct-by-construction" URL
-- encoding and decoding from a sum type. This can be used in conjunction
-- with @uriSub@ to perform client side routing and the embedding
-- of type-safe links in any @View model action@.
--
-- @
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
-----------------------------------------------------------------------------
module Miso.Router
  ( -- ** Classes
    Router (..)
    -- ** Types
  , Capture
    -- ** Re-exports
  , URI (..)
    -- ** Errors
  , RoutingError (..)
    -- ** Functions
  , route
  , parseURI
  , prettyURI
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
import           Miso.Html.Property
import           Miso.Util.Parser
import qualified Miso.Util.Lexer as L
import           Miso.Util.Lexer (Lexer)
import           Miso.String (ToMisoString, FromMisoString, fromMisoStringEither)
import qualified Miso.String as MS
-----------------------------------------------------------------------------
newtype Capture sym a = Capture a
  deriving stock (Generic, Show)
  deriving newtype (ToMisoString, FromMisoString)
-----------------------------------------------------------------------------
newtype Path (path :: Symbol) = Path MisoString
  deriving (Generic, Show)
  deriving newtype (ToMisoString, IsString)
-----------------------------------------------------------------------------
newtype QueryFlag (path :: Symbol) = QueryFlag Bool
  deriving (Generic, Show)
-----------------------------------------------------------------------------
newtype QueryParam (path :: Symbol) a = QueryParam (Maybe a)
  deriving (Generic, Show)
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
  deriving (Show, Eq)
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
-----------------------------------------------------------------------------
data RoutingError
  = PathNotFound MisoString
  | DecodeFailure MisoString MisoString
  | ParseError MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance IsString RoutingError where
  fromString s = ParseError (ms s)
-----------------------------------------------------------------------------
type RouteParser = ParserT [Token] []
-----------------------------------------------------------------------------
capture :: FromMisoString value => RouteParser value
capture = do
  CaptureOrPathToken capture_ <- captureOrPathToken
  case fromMisoStringEither capture_ of
    Left msg -> fail (fromMisoString (ms msg))
    Right token -> pure token
-----------------------------------------------------------------------------
queryParam :: FromMisoString value => URI -> MisoString -> RouteParser (Maybe value)
queryParam URI {..} key = do
  case M.lookup key uriQueryString of
    Just (Just value) ->
      case fromMisoStringEither value of
        Left _ -> pure Nothing
        Right parsed -> pure (Just parsed)
    _ -> pure Nothing
-----------------------------------------------------------------------------
queryFlag :: MisoString -> URI -> RouteParser Bool
queryFlag specified URI {..} =
  pure $ isJust (M.lookup specified uriQueryString)
      -- dmj: no values allowed in query flags
-----------------------------------------------------------------------------
path :: MisoString -> RouteParser ()
path specified = do
  CaptureOrPathToken parsed <- captureOrPathToken
  when (specified /= parsed) (fail "path")
-----------------------------------------------------------------------------
route :: Router route => MisoString -> Either RoutingError route
route = toRoute
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

  fromURI :: route -> URI
  fromURI = tokensToURI . fromRoute

  link_ :: route -> Attribute action
  link_ = href_ . prettyRoute

  prettyRoute :: route -> MisoString
  prettyRoute = prettyURI . tokensToURI . fromRoute

  toRoute :: MisoString -> Either RoutingError route
  toRoute input = parseRoute input routeParser

  routeParser :: RouteParser route
  default routeParser :: (Generic route, GRouter (Rep route)) => RouteParser route
  routeParser = do
    uri <- tokensToURI <$> allTokens
    modifyTokens $ \tokens -> [ token | token@CaptureOrPathToken{} <- tokens ]
    to <$> gRouteParser uri
-----------------------------------------------------------------------------
prettyURI :: URI -> MisoString
prettyURI URI {..} = uriPath <> queries <> flags <> uriFragment
  where
    queries = "?" <>
      MS.intercalate "&"
        [ k <> "=" <> v
        | (k, Just v) <- M.toList uriQueryString
        ]
    flags = mconcat
        [ "?" <> k
        | (k, Nothing) <- M.toList uriQueryString
        ]
-----------------------------------------------------------------------------
class GRouter f where
  gFromRoute :: f route -> [Token]
  gRouteParser :: URI -> RouteParser (f route)
-----------------------------------------------------------------------------
instance GRouter next => GRouter (D1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser uri = M1 <$> gRouteParser uri
-----------------------------------------------------------------------------
instance (KnownSymbol name, GRouter next) => GRouter (C1 (MetaCons name x y) next) where
  gFromRoute (M1 x) =
    CaptureOrPathToken name : gFromRoute x
      where
        name = lowercase $ symbolVal (Proxy @name)
  gRouteParser uri = do
    void (path name)
    M1 <$> gRouteParser uri
      where
        name = lowercase $ symbolVal (Proxy @name)
-----------------------------------------------------------------------------
instance GRouter next => GRouter (S1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser uri = M1 <$> gRouteParser uri
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall path m . KnownSymbol path => GRouter (K1 m (Path path)) where
  gFromRoute (K1 x) = pure $ CaptureOrPathToken (ms x)
  gRouteParser _ = K1 (Path chunk) <$ path chunk
    where
      chunk = ms $ symbolVal (Proxy :: Proxy path)
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} (FromMisoString a, ToMisoString a) => GRouter (K1 m (Capture sym a)) where
  gFromRoute (K1 x) = pure $ CaptureOrPathToken (ms x)
  gRouteParser _ = K1 <$> capture
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall param m a . (ToMisoString a, FromMisoString a, KnownSymbol param) => GRouter (K1 m (QueryParam param a)) where
  gFromRoute (K1 (QueryParam Nothing)) = []
  gFromRoute (K1 (QueryParam (Just v))) =
    pure (QueryParamToken (ms (symbolVal (Proxy @param))) (ms v))
  gRouteParser uri = K1 . QueryParam <$> do
    queryParam uri (ms (symbolVal (Proxy @param)))
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall flag m . KnownSymbol flag => GRouter (K1 m (QueryFlag flag)) where
  gFromRoute (K1 x) = pure $ QueryFlagToken (ms x)
  gRouteParser uri = K1 . QueryFlag <$> queryFlag flag uri
    where
      flag = ms $ symbolVal (Proxy @flag)
-----------------------------------------------------------------------------
instance (FromMisoString a, ToMisoString a) => GRouter (K1 m a) where
  gFromRoute (K1 x) = pure $ CaptureOrPathToken (ms x)
  gRouteParser _ = K1 <$> capture
-----------------------------------------------------------------------------
instance GRouter U1 where
  gFromRoute U1 = pure $ CaptureOrPathToken $ lowercase (show U1)
  gRouteParser _ = pure U1
-----------------------------------------------------------------------------
instance (GRouter left, GRouter right) => GRouter (left :*: right) where
  gFromRoute (left :*: right) = gFromRoute left <> gFromRoute right
  gRouteParser uri = liftA2 (:*:) (gRouteParser uri) (gRouteParser uri)
-----------------------------------------------------------------------------
instance (GRouter left, GRouter right) => GRouter (left :+: right) where
  gFromRoute = \case
    L1 m1 -> gFromRoute m1
    R1 m1 -> gFromRoute m1
  gRouteParser uri = foldr (<|>) empty
    [ L1 <$> gRouteParser uri
    , R1 <$> gRouteParser uri
    ]
-----------------------------------------------------------------------------
captureOrPathToken :: RouteParser Token
captureOrPathToken = satisfy $ \case
  CaptureOrPathToken {} -> True
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
        ] where
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
    Left e ->
      Left $ ParseError (ms (show e))
    Right (tokens, _) ->
      case runParserT parser tokens of
        [(x, _)]  ->
          Right x
        []  ->
          Left $ ParseError ("No parses for: " <> input)
        (_, _) : _  ->
          Left $ ParseError ("Ambiguous parse for: " <> input)
-----------------------------------------------------------------------------
lowercase :: String -> MisoString
lowercase (x:xs) = ms (C.toLower x : xs)
lowercase x = ms x
-----------------------------------------------------------------------------
