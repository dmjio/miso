-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleContexts           #-}
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
----------------------------------------------------------------------------
module Miso.Router
  ( -- ** Classes
    Router (..)
    -- ** Types
  , Capture
    -- ** Errors
  , RoutingError (..)
  ) where
-----------------------------------------------------------------------------
import           Data.Char
import qualified Data.Char as C
import           Data.Proxy
import           Data.String
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad
import           Control.Monad.State
import           GHC.Generics
import           GHC.TypeLits
-----------------------------------------------------------------------------
import           Miso.Types hiding (model)
import           Miso.Util
import           Miso.Util.Parser
import qualified Miso.Util.Lexer as L
import           Miso.Util.Lexer (Lexer)
import           Miso.String
import qualified Miso.String as MS
-----------------------------------------------------------------------------
data Route
  = Home 
  | About
  | Widget (Capture Int)
  deriving stock (Generic, Show)
  deriving anyclass Router
-----------------------------------------------------------------------------
test :: Either RoutingError Route
test = toRoute "/widget/1/"
-----------------------------------------------------------------------------
newtype Capture a = Capture a
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
instance KnownSymbol name => ToMisoString (QueryFlag name) where
  toMisoString = \case
    QueryFlag True ->
      "?" <> ms (symbolVal (Proxy @name))
    QueryFlag False ->
      mempty
-----------------------------------------------------------------------------
data Token
  = PathToken MisoString
  | QueryParamToken [(MisoString, MisoString)]
  | QueryFlagToken MisoString
  | CaptureToken MisoString
  | FragmentToken MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
orderTokens :: [Token] -> [Token]
orderTokens tokens = mconcat
  [ Prelude.filter isPathRelated tokens
  , Prelude.filter (not . isPathRelated) tokens
  ] where
      isPathRelated = \case
        PathToken {} -> True
        CaptureToken {} -> True
        _ -> False
-----------------------------------------------------------------------------
instance ToMisoString Token where
  toMisoString = \case
    PathToken x -> "/" <> x
    QueryFlagToken x -> "?" <> x
    CaptureToken x -> "/" <> x
    FragmentToken x -> "#" <> x
    QueryParamToken params ->
      "?" <> MS.intercalate "&"
        [ key <> "=" <> value
        | (key, value) <- params
        ]
-----------------------------------------------------------------------------
data RoutingError
  = PathNotFound MisoString
  | DecodeFailure MisoString
  | ParseError MisoString
  | RoutingError
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance IsString RoutingError where
  fromString s = ParseError (ms s)
-----------------------------------------------------------------------------
instance Semigroup RoutingError where
  l <> RoutingError = l
  RoutingError <> r = r
  _ <> r = r
-----------------------------------------------------------------------------
instance Monoid RoutingError where
  mempty = RoutingError
-----------------------------------------------------------------------------
type RouteParser route = ExceptT RoutingError (StateT [Token] []) route
-----------------------------------------------------------------------------
capture :: FromMisoString value => RouteParser value
capture = do
  CaptureToken capture_ <- captureToken
  case fromMisoStringEither capture_ of
    Left msg -> throwError (DecodeFailure msg)
    Right token -> pure token
-----------------------------------------------------------------------------
queryParams :: FromMisoString value => RouteParser [(MisoString, value)]
queryParams = do
  QueryParamToken queryParams_ <- captureToken
  forM queryParams_ $ \(key, value) ->
    case fromMisoStringEither value of
      Left msg -> throwError (DecodeFailure msg)
      Right parsed -> pure (key, parsed)
-----------------------------------------------------------------------------
queryFlag :: MisoString -> RouteParser Bool
queryFlag specified = foundFlag <|> pure False
  where
    foundFlag = do
      QueryFlagToken parsed <- captureToken
      pure (specified == parsed)
-----------------------------------------------------------------------------
path :: MisoString -> RouteParser MisoString
path specified = do
  PathToken parsed <- pathToken
  when (specified /= parsed) $ throwError (PathNotFound specified)
  pure parsed
-----------------------------------------------------------------------------
class Router route where
  fromRoute :: route -> [Token]
  default fromRoute :: (Generic route, GRouter (Rep route)) => route -> [Token]
  fromRoute = gFromRoute . from

  prettyRoute :: route -> MisoString
  default prettyRoute :: (Generic route, GRouter (Rep route)) => route -> MisoString
  prettyRoute = prettyToken . fromRoute

  toRoute :: MisoString -> Either RoutingError route
  toRoute input = parseURI input routeParser

  routeParser :: RouteParser route
  default routeParser :: (Generic route, GRouter (Rep route)) => RouteParser route
  routeParser = to <$> gRouteParser
-----------------------------------------------------------------------------
prettyToken :: [Token] -> MisoString
prettyToken tokens = MS.concat [ ms token | token <- orderTokens tokens ]
-----------------------------------------------------------------------------
class GRouter f where
  gFromRoute :: f route -> [Token]
  gRouteParser :: RouteParser (f route)
-----------------------------------------------------------------------------
instance GRouter next => GRouter (D1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser = M1 <$> gRouteParser
-----------------------------------------------------------------------------
instance GRouter next => GRouter (C1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser = M1 <$> gRouteParser
-----------------------------------------------------------------------------
instance GRouter next => GRouter (S1 m next) where
  gFromRoute (M1 x) = gFromRoute x
  gRouteParser = M1 <$> gRouteParser
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall path m . KnownSymbol path => GRouter (K1 m (Path path)) where
  gFromRoute (K1 x) = pure $ PathToken (ms x)
  gRouteParser = K1 . Path <$> path chunk
    where
      chunk = ms $ symbolVal (Proxy :: Proxy path)
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} (FromMisoString a, ToMisoString a) => GRouter (K1 m (Capture a)) where
  gFromRoute (K1 x) = pure $ CaptureToken (ms x)
  gRouteParser = K1 <$> capture
-----------------------------------------------------------------------------
instance {-# OVERLAPS #-} forall path m . KnownSymbol path => GRouter (K1 m (QueryFlag path)) where
  gFromRoute (K1 x) = pure $ QueryFlagToken (ms x)
  gRouteParser = K1 . QueryFlag <$> do
    queryFlag $ ms (symbolVal (Proxy @path))
-----------------------------------------------------------------------------
instance (FromMisoString a, ToMisoString a) => GRouter (K1 m a) where
  gFromRoute (K1 x) = pure $ CaptureToken (ms x)
  gRouteParser = K1 <$> capture
-----------------------------------------------------------------------------
instance GRouter U1 where
  gFromRoute U1 
    | x : xs <- show U1 = pure $ CaptureToken $ ms (C.toLower x : xs)
    | otherwise = mempty
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
  gRouteParser = msum
    [ L1 <$> gRouteParser
    , R1 <$> gRouteParser
    ]
-----------------------------------------------------------------------------
pathToken :: RouteParser Token
pathToken = lift $ satisfy $ \case
  PathToken {} -> True
  _ -> False
-----------------------------------------------------------------------------
captureToken :: RouteParser Token
captureToken = lift $ satisfy $ \case
  CaptureToken {} -> True
  _ -> False
-----------------------------------------------------------------------------
urlLexer :: Lexer [Token]
urlLexer = some $ msum
  [ captureLexer
  , pathLexer
--  , queryParamLexer
  , queryFlagLexer
  , fragmentLexer
  ] where
      captureLexer = do
        void (L.string "/:")
        CaptureToken <$> chars
      pathLexer = do
        void (L.char '/')
        PathToken <$> chars
      fragmentLexer =
        FragmentToken <$> chars <* L.char '#'
      queryFlagLexer =
        QueryFlagToken <$>
          chars <* L.char '?'
      queryParamLexer = QueryParamToken <$> do
        sepBy (L.char '&') $ do
          void (L.char '?')
          key <- chars
          void (L.char '=')
          value <- chars
          pure (key, value)
-----------------------------------------------------------------------------
chars :: Lexer MisoString
chars = ms <$> some (L.satisfy isAlphaNum)
-----------------------------------------------------------------------------
lexTokens :: MisoString -> Either L.LexerError [Token]
lexTokens input =
  case L.runLexer urlLexer (L.mkStream input) of
    Right (tokens, _) -> Right tokens
    Left x -> Left x
-----------------------------------------------------------------------------
parseURI :: MisoString -> RouteParser a -> Either RoutingError a
parseURI input parser = 
  case L.runLexer urlLexer (L.mkStream input) of
    Left e ->
      Left $ ParseError (ms (show e))
    Right (tokens, _) -> 
      case flip runStateT tokens (runExceptT parser) of
        [(Right x, _)]  ->
          Right x
        (Left e, _) : _  ->
          Left e
        []  ->
          Left "No parses"
        (Right _, _) : _  ->
          Left "Ambiguous parse"
-----------------------------------------------------------------------------
-- type RouteParser route = ExceptT RoutingError (StateT [Token] []) route
