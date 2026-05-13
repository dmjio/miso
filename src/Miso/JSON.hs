-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.JSON
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A JSON library specialized to MisoString for performance. Largely based
-- on [microaeson](https://hackage-content.haskell.org/package/microaeson).
--
-- Uses JS runtime primitives `JSON.stringify()` and `JSON.parse()`. 
--
----------------------------------------------------------------------------
module Miso.JSON
  ( -- * JSON
    -- ** Core JSON types
    Value(..)
  , Object
  , Pair
  , Result (..)
    -- ** Constructors
  , (.=)
  , object
  , emptyArray
  , emptyObject
    -- ** Accessors
  , (.:)
  , (.:?)
  , (.:!)
  , (.!=)
    -- * Encoding and decoding
  , encode
  , encodePure
  , decode
  , Parser.decodePure
    -- * Prism-style parsers
  , withObject
  , withText
  , withArray
  , withNumber
  , withBool
    -- * Type conversion
  , FromJSON(parseJSON)
  , Parser (..)
  , parseMaybe
  , ToJSON(toJSON)
  -- * Misc.
  , fromJSON
  , parseEither
  , eitherDecode
  , typeMismatch
  -- * Pretty
  , encodePretty
  , encodePretty'
  , defConfig
  , Config (..)
  -- * FFI
  , fromJSVal_Value
  , toJSVal_Value
  , jsonStringify
  , jsonParse
  -- * Options
  , Options (..)
  , defaultOptions
  -- * Generics
  , GToJSON (..)
  , GToFields (..)
  , GToJSONSum (..)
  , Fields (..)
  , GFromJSON (..)
  , GFromFields (..)
  , GFromJSONSum (..)
  , genericToJSON
  , genericParseJSON
  -- * Modifiers
  , camelTo2
  ) where
----------------------------------------------------------------------------
#ifdef GHCJS_BOTH
import qualified GHCJS.Marshal as Marshal
#endif
----------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
#if __GLASGOW_HASKELL__ <= 865
import           Control.Monad.Fail
import           GHC.Natural (Natural)
#endif
import           Data.Char
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Int
import           GHC.Natural (naturalToInteger, naturalFromInteger)
import           GHC.TypeLits
import           Data.Kind
#ifndef VANILLA
import qualified Data.Text as T
#endif
import qualified Data.Text.Lazy as LT
import           Data.Word
import           GHC.Generics
----------------------------------------------------------------------------
import           Miso.DSL.FFI
import           Miso.String (FromMisoString, ToMisoString, MisoString, ms, singleton, pack)
import qualified Miso.String as MS
import           Miso.JSON.Types
import qualified Miso.JSON.Parser as Parser
----------------------------------------------------------------------------
#ifndef VANILLA
import           Control.Monad.Trans.Maybe
import           System.IO.Unsafe (unsafePerformIO)
#else
import           Numeric (showHex)
#endif

----------------------------------------------------------------------------
infixr 8 .=
(.=) :: ToJSON v => MisoString -> v -> Pair
k .= v  = (k, toJSON v)
----------------------------------------------------------------------------
-- | Create a 'Value' from a list of name\/value 'Pair's.
object :: [Pair] -> Value
object = Object . M.fromList
----------------------------------------------------------------------------
-- | The empty JSON 'Object' (i.e. @{}@).
emptyObject :: Value
emptyObject = Object mempty
----------------------------------------------------------------------------
-- | The empty JSON 'Array' (i.e. @[]@).
emptyArray :: Value
emptyArray = Array mempty
----------------------------------------------------------------------------
(.:) :: FromJSON a => Object -> MisoString -> Parser a
m .: k = maybe (pfail ("Key not found: " <> k)) parseJSON (M.lookup k m)
----------------------------------------------------------------------------
(.:?) :: FromJSON a => Object -> MisoString -> Parser (Maybe a)
m .:? k = maybe (pure Nothing) parseJSON (M.lookup k m)
----------------------------------------------------------------------------
(.:!) :: FromJSON a => Object -> MisoString -> Parser (Maybe a)
m .:! k = maybe (pure Nothing) (fmap Just . parseJSON) (M.lookup k m)
----------------------------------------------------------------------------
(.!=) :: Parser (Maybe a) -> a -> Parser a
mv .!= def = fmap (maybe def id) mv
----------------------------------------------------------------------------
class ToJSON a where
  toJSON :: a -> Value
  default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
  toJSON = genericToJSON defaultOptions
----------------------------------------------------------------------------
genericToJSON :: (Generic a, GToJSON (Rep a)) => Options -> a -> Value
genericToJSON opts = gToJSON opts . from
----------------------------------------------------------------------------
data Options
  = Options
  { fieldLabelModifier :: String -> String
  }
----------------------------------------------------------------------------
defaultOptions :: Options
defaultOptions = Options { fieldLabelModifier = \x -> x }
----------------------------------------------------------------------------
camelTo2 :: Char -> String -> String
camelTo2 c = Prelude.map toLower . go2 . go1
    where go1 "" = ""
          go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
          go1 (x:xs) = x : go1 xs
          go2 "" = ""
          go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
          go2 (x:xs) = x : go2 xs
----------------------------------------------------------------------------
-- | Intermediate representation of a constructor's fields after encoding.
--
-- 'RecordFields' is produced when every selector has a name (record syntax);
-- 'PositionalFields' is produced for all other constructors.
data Fields
  = RecordFields   [(MisoString, Value)]
  -- ^ Named fields (record constructor)
  | PositionalFields [Value]
  -- ^ Positional fields (non-record constructor)
----------------------------------------------------------------------------
combineFields :: Fields -> Fields -> Fields
combineFields (RecordFields   xs) (RecordFields   ys) = RecordFields   (xs <> ys)
combineFields (PositionalFields xs) (PositionalFields ys) = PositionalFields (xs <> ys)
combineFields _ _ = PositionalFields []  -- mixed; shouldn't occur in valid GHC Generics
----------------------------------------------------------------------------
-- | Collect a constructor's fields into 'Fields'.
class GToFields (f :: Type -> Type) where
  gToFields :: Options -> f a -> Fields
----------------------------------------------------------------------------
instance GToFields U1 where
  gToFields _ _ = PositionalFields []
----------------------------------------------------------------------------
instance GToFields V1 where
  gToFields _ v = v `seq` PositionalFields []
----------------------------------------------------------------------------
instance (GToFields f, GToFields g) => GToFields (f :*: g) where
  gToFields opts (x :*: y) = combineFields (gToFields opts x) (gToFields opts y)
----------------------------------------------------------------------------
instance (Selector m, GToFields f) => GToFields (S1 m f) where
  gToFields opts (M1 x) =
    let n = selName (undefined :: S1 m f ())
    in if null n
       then gToFields opts x
       else case gToFields opts x of
              PositionalFields [v] -> RecordFields [(ms (fieldLabelModifier opts n), v)]
              fs                   -> fs  -- shouldn't happen
----------------------------------------------------------------------------
instance ToJSON a => GToFields (K1 r a) where
  gToFields _ (K1 x) = PositionalFields [toJSON x]
----------------------------------------------------------------------------
-- | Encode a single-constructor (product) type. No tag is added.
--
-- * Record:       @{"field1": v, ...}@
-- * 0 fields:     @[]@
-- * 1 field:      the value itself (unwrapped, like a newtype)
-- * 2+ fields:    @[v1, v2, ...]@
encodeProduct :: Fields -> Value
encodeProduct = \case
  RecordFields   kvs  -> Object (M.fromList kvs)
  PositionalFields []  -> Array []
  PositionalFields [v] -> v
  PositionalFields vs  -> Array vs
----------------------------------------------------------------------------
-- | Encode a sum constructor. Adds a @\"tag\"@ key.
--
-- * Record:       @{\"tag\": \"C\", \"field1\": v, ...}@
-- * 0 fields:     @{\"tag\": \"C\"}@
-- * 1 field:      @{\"tag\": \"C\", \"contents\": v}@
-- * 2+ fields:    @{\"tag\": \"C\", \"contents\": [v1, v2, ...]}@
encodeTaggedCon :: MisoString -> Fields -> Value
encodeTaggedCon tag = \case
  RecordFields   kvs  -> Object (M.fromList (("tag", String tag) : kvs))
  PositionalFields []  -> Object (M.singleton "tag" (String tag))
  PositionalFields [v] -> object [("tag", String tag), ("contents", v)]
  PositionalFields vs  -> object [("tag", String tag), ("contents", Array vs)]
----------------------------------------------------------------------------
-- | Top-level generic encoding class.
--
-- Encoding rules match aeson's default @TaggedObject \"tag\" \"contents\"@
-- (with @allNullaryToStringTag = False@):
--
-- * Single-constructor record:          @{\"field1\": v1, ...}@
-- * Single-constructor positional:      @v@ (1 field), @[v1,v2,...]@ (n>1), @[]@ (0)
-- * Sum record constructor:             @{\"tag\": \"C\", \"field1\": v1, ...}@
-- * Sum nullary constructor:            @{\"tag\": \"C\"}@
-- * Sum positional constructor:         @{\"tag\": \"C\", \"contents\": v}@ or @[...]@
class GToJSON (f :: Type -> Type) where
  gToJSON :: Options -> f a -> Value
----------------------------------------------------------------------------
instance GToJSONRep f => GToJSON (D1 m f) where
  gToJSON opts (M1 x) = gToJSONRep opts x
----------------------------------------------------------------------------
-- Internal: dispatches single-constructor vs sum at the child of D1.
class GToJSONRep (f :: Type -> Type) where
  gToJSONRep :: Options -> f a -> Value
-- Single constructor: no tag
instance GToFields f => GToJSONRep (C1 m f) where
  gToJSONRep opts (M1 x) = encodeProduct (gToFields opts x)
-- Sum: each branch tagged
instance (GToJSONSum f, GToJSONSum g) => GToJSONRep (f :+: g) where
  gToJSONRep opts x = gToJSONSum opts x
----------------------------------------------------------------------------
-- | Encode sum constructors with a @\"tag\"@ key.
class GToJSONSum (f :: Type -> Type) where
  gToJSONSum :: Options -> f a -> Value
----------------------------------------------------------------------------
instance (GToJSONSum f, GToJSONSum g) => GToJSONSum (f :+: g) where
  gToJSONSum opts (L1 x) = gToJSONSum opts x
  gToJSONSum opts (R1 x) = gToJSONSum opts x
----------------------------------------------------------------------------
instance (Constructor m, GToFields f) => GToJSONSum (C1 m f) where
  gToJSONSum opts c@(M1 x) = encodeTaggedCon (ms (conName c)) (gToFields opts x)
----------------------------------------------------------------------------
instance ToJSON () where
  toJSON () = Array []
----------------------------------------------------------------------------
instance ToJSON Value where
  toJSON = id
----------------------------------------------------------------------------
instance ToJSON Char where
  toJSON c = String (singleton c)
----------------------------------------------------------------------------
instance ToJSON Bool where
  toJSON = Bool
----------------------------------------------------------------------------
instance {-# OVERLAPPING #-} ToJSON String where
  toJSON = toJSON . MS.pack
----------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON [a] where
  toJSON = Array . Prelude.map toJSON
----------------------------------------------------------------------------
instance ToJSON v => ToJSON (M.Map MisoString v) where
  toJSON = Object . M.map toJSON
----------------------------------------------------------------------------
instance ToJSON a => ToJSON (Maybe a) where
  toJSON = \case
    Nothing -> Null
    Just a -> toJSON a
----------------------------------------------------------------------------
instance (ToJSON a,ToJSON b) => ToJSON (a,b) where
  toJSON (a,b) = Array [toJSON a, toJSON b]
----------------------------------------------------------------------------
instance (ToJSON a,ToJSON b,ToJSON c) => ToJSON (a,b,c) where
  toJSON (a,b,c) = Array [toJSON a, toJSON b, toJSON c]
----------------------------------------------------------------------------
instance (ToJSON a,ToJSON b,ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
  toJSON (a,b,c,d) = Array [toJSON a, toJSON b, toJSON c, toJSON d]
----------------------------------------------------------------------------
instance ToJSON MisoString where
  toJSON = String
----------------------------------------------------------------------------
#ifndef VANILLA
instance ToJSON T.Text where
  toJSON = toJSON . ms
#endif
----------------------------------------------------------------------------
instance ToJSON LT.Text where
  toJSON = toJSON . ms
----------------------------------------------------------------------------
instance ToJSON Float where
  toJSON = Number . realToFrac
----------------------------------------------------------------------------
instance ToJSON Double where
  toJSON = Number
----------------------------------------------------------------------------
instance ToJSON Int    where  toJSON = Number . realToFrac
instance ToJSON Int8   where  toJSON = Number . realToFrac
instance ToJSON Int16  where  toJSON = Number . realToFrac
instance ToJSON Int32  where  toJSON = Number . realToFrac
----------------------------------------------------------------------------
instance ToJSON Word   where  toJSON = Number . realToFrac
instance ToJSON Word8  where  toJSON = Number . realToFrac
instance ToJSON Word16 where  toJSON = Number . realToFrac
instance ToJSON Word32 where  toJSON = Number . realToFrac
----------------------------------------------------------------------------
-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Int64  where  toJSON = Number . realToFrac
----------------------------------------------------------------------------
-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Word64 where  toJSON = Number . realToFrac
----------------------------------------------------------------------------
-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Integer where toJSON = Number . fromInteger
----------------------------------------------------------------------------
-- | Possibly lossy due to conversion to 'Double'
instance ToJSON Natural where toJSON = Number . fromInteger . naturalToInteger
----------------------------------------------------------------------------
newtype Parser a = Parser { unParser :: Either MisoString a }
  deriving (Functor, Applicative, Monad)
----------------------------------------------------------------------------
instance MonadFail Parser where
  fail = pfail . pack
----------------------------------------------------------------------------
instance Alternative Parser where
  empty = Parser (Left mempty)
  Parser (Left _) <|> r = r
  l <|> _ = l
----------------------------------------------------------------------------
instance MonadPlus Parser
----------------------------------------------------------------------------
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v =
  case parseEither m v of
    Left _ -> Nothing
    Right r -> Just r 
----------------------------------------------------------------------------
parseEither :: (a -> Parser b) -> a -> Either MisoString b
parseEither m v = unParser (m v)
----------------------------------------------------------------------------
pfail :: MisoString -> Parser a
pfail message = Parser (Left message)
----------------------------------------------------------------------------
class FromJSON a where
  parseJSON :: Value -> Parser a
  default parseJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
  parseJSON = genericParseJSON defaultOptions
----------------------------------------------------------------------------
-- | Top-level generic decoding class. Symmetric with 'GToJSON'.
--
-- Decoding rules match aeson's default @TaggedObject \"tag\" \"contents\"@
-- (with @allNullaryToStringTag = False@).
class GFromJSON (f :: Type -> Type) where
  gParseJSON :: Options -> Value -> Parser (f a)
----------------------------------------------------------------------------
genericParseJSON :: (Generic a, GFromJSON (Rep a)) => Options -> Value -> Parser a
genericParseJSON opts value = to <$> gParseJSON opts value
----------------------------------------------------------------------------
instance GFromJSONRep f => GFromJSON (D1 m f) where
  gParseJSON opts v = M1 <$> gFromJSONRep opts v
----------------------------------------------------------------------------
-- Internal: dispatches single-constructor vs sum at the child of D1.
class GFromJSONRep (f :: Type -> Type) where
  gFromJSONRep :: Options -> Value -> Parser (f a)
-- Single constructor
instance GFromFields f => GFromJSONRep (C1 m f) where
  gFromJSONRep opts v = M1 <$> parseProd opts v
-- Sum type
instance (GFromJSONSum f, GFromJSONSum g) => GFromJSONRep (f :+: g) where
  gFromJSONRep opts v = gFromJSONSum opts v
----------------------------------------------------------------------------
-- | Parse sum constructors, trying each branch left-to-right.
class GFromJSONSum (f :: Type -> Type) where
  gFromJSONSum :: Options -> Value -> Parser (f a)
----------------------------------------------------------------------------
instance (GFromJSONSum f, GFromJSONSum g) => GFromJSONSum (f :+: g) where
  gFromJSONSum opts v = (L1 <$> gFromJSONSum opts v) <|> (R1 <$> gFromJSONSum opts v)
----------------------------------------------------------------------------
instance (Constructor m, GFromFields f) => GFromJSONSum (C1 m f) where
  gFromJSONSum opts v = M1 <$> parseTaggedCon tag opts v
    where tag = ms (conName (undefined :: C1 m f a))
----------------------------------------------------------------------------
-- | Parse a single-constructor (product) type from a 'Value'.
parseProd :: forall f a. GFromFields f => Options -> Value -> Parser (f a)
parseProd opts v
  | gIsRecord @f = withObject "generic record" (gFromRecord opts) v
  | otherwise    = case v of
      Array vs -> gFromPositional opts vs
      _        -> gFromPositional opts [v]  -- single-field shorthand
----------------------------------------------------------------------------
-- | Parse a tagged sum constructor from an Object envelope.
parseTaggedCon :: forall f a. GFromFields f => MisoString -> Options -> Value -> Parser (f a)
parseTaggedCon tag opts = \case
  Object o -> do
    t <- case M.lookup "tag" o of
           Just (String t) -> pure t
           Just _          -> pfail "\"tag\" field is not a string"
           Nothing         -> pfail "missing \"tag\" field"
    if t /= tag
      then pfail ("expected tag " <> ms (show tag) <> ", got " <> ms (show t))
      else if gIsRecord @f
           then gFromRecord opts o
           else case M.lookup "contents" o of
                  Just (Array vs) -> gFromPositional opts vs
                  Just single     -> gFromPositional opts [single]
                  Nothing         -> gFromPositional opts []
  _ -> pfail ("expected JSON object for constructor " <> ms (show tag))
----------------------------------------------------------------------------
-- | Field-level decoder. Knows whether the constructor is a record and
-- how many fields it has; can decode from a JSON 'Object' (record mode)
-- or a positional '[Value]' list.
class GFromFields (f :: Type -> Type) where
  -- | Is this a record constructor (all selectors have names)?
  gIsRecord      :: Bool
  -- | Number of fields.
  gFieldCount    :: Int
  -- | Decode from a JSON 'Object' (record mode: look up by field name).
  gFromRecord    :: Options -> Object -> Parser (f a)
  -- | Decode from a positional list of 'Value'.
  gFromPositional :: Options -> [Value] -> Parser (f a)
----------------------------------------------------------------------------
instance GFromFields U1 where
  gIsRecord       = False
  gFieldCount     = 0
  gFromRecord   _ _ = pure U1
  gFromPositional _ _ = pure U1
----------------------------------------------------------------------------
instance GFromFields V1 where
  gIsRecord       = False
  gFieldCount     = 0
  gFromRecord   _ _ = pfail "V1"
  gFromPositional _ _ = pfail "V1"
----------------------------------------------------------------------------
instance (GFromFields f, GFromFields g) => GFromFields (f :*: g) where
  gIsRecord       = gIsRecord @f
  gFieldCount     = gFieldCount @f + gFieldCount @g
  gFromRecord opts o =
    (:*:) <$> gFromRecord opts o
          <*> gFromRecord opts o
  gFromPositional opts vs =
    let n = gFieldCount @f
    in (:*:) <$> gFromPositional opts (take n vs)
             <*> gFromPositional opts (drop n vs)
----------------------------------------------------------------------------
-- | Selector with a 'Maybe' field: uses '.:?' so missing keys decode as Nothing.
instance {-# OVERLAPPING #-} (Selector m, FromJSON a)
    => GFromFields (S1 m (K1 r (Maybe a))) where
  gIsRecord       = not (null name)
    where name = selName (undefined :: S1 m (K1 r (Maybe a)) ())
  gFieldCount     = 1
  gFromRecord opts o =
    M1 . K1 <$> o .:? ms (fieldLabelModifier opts
                    (selName (undefined :: S1 m (K1 r (Maybe a)) ())))
  gFromPositional _ vs = case vs of
    (v:_) -> M1 . K1 <$> parseJSON v
    []    -> pure (M1 (K1 Nothing))
----------------------------------------------------------------------------
-- | General selector.
instance {-# OVERLAPPABLE #-} (Selector m, FromJSON a)
    => GFromFields (S1 m (K1 r a)) where
  gIsRecord       = not (null name)
    where name = selName (undefined :: S1 m (K1 r a) ())
  gFieldCount     = 1
  gFromRecord opts o =
    M1 . K1 <$> o .: ms (fieldLabelModifier opts
                    (selName (undefined :: S1 m (K1 r a) ())))
  gFromPositional _ vs = case vs of
    (v:_) -> M1 . K1 <$> parseJSON v
    []    -> pfail "gFromPositional: unexpected end of fields"
----------------------------------------------------------------------------
instance FromJSON Value where
  parseJSON = pure
----------------------------------------------------------------------------
instance FromJSON Bool where
  parseJSON = withBool "Bool" pure
----------------------------------------------------------------------------
instance FromJSON MisoString where
  parseJSON = withText "MisoString" pure
----------------------------------------------------------------------------
#ifndef VANILLA
instance FromJSON T.Text where
  parseJSON = withText "Text" go
    where
      go s =
        case MS.fromMisoStringEither s of
          Right lt -> pure lt
          Left e -> pfail $ ms e
#endif
----------------------------------------------------------------------------
instance FromJSON LT.Text where
  parseJSON = withText "LText" go
    where
      go s =
        case MS.fromMisoStringEither s of
          Right lt -> pure lt
          Left e -> pfail $ ms e
----------------------------------------------------------------------------
instance {-# OVERLAPPING #-} FromJSON String where
  parseJSON = withText "String" (pure . MS.unpack)
----------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} FromJSON a => FromJSON [a] where
  parseJSON = withArray "[a]" (mapM parseJSON)
----------------------------------------------------------------------------
instance FromJSON Double where
  parseJSON Null = pure (0/0)
  parseJSON j    = withNumber "Double" pure j
----------------------------------------------------------------------------
instance FromJSON Float where
  parseJSON Null = pure (0/0)
  parseJSON j    = withNumber "Float" (pure . realToFrac) j
----------------------------------------------------------------------------
instance FromJSON Integer where
  parseJSON = withNumber "Integer" (pure . round)
----------------------------------------------------------------------------
instance FromJSON Natural where
  parseJSON = withNumber "Natural" parseNumber
    where parseNumber d | d < 0 = pfail ("Cannot parse negative number as Natural: " <> ms d)
                        | isNaN d = pfail ("Cannot parse NaN as Natural: " <> ms d)
                        | otherwise  = pure $ naturalFromInteger $ fromInteger $ round d 
----------------------------------------------------------------------------
instance FromJSON Int where
  parseJSON = withNumber "Int" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Int8 where
  parseJSON = withNumber "Int8" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Int16 where
  parseJSON = withNumber "Int16" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Int32 where
  parseJSON = withNumber "Int32" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Int64 where
  parseJSON = withNumber "Int64" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word where
  parseJSON = withNumber "Word" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word8 where
  parseJSON = withNumber "Word8" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word16 where
  parseJSON = withNumber "Word16" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word32 where
  parseJSON = withNumber "Word32" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON Word64 where
  parseJSON = withNumber "Word64" (pure . fromInteger . round)
----------------------------------------------------------------------------
instance FromJSON () where
  parseJSON = withArray "()" $ \lst ->
    case lst of
      [] -> pure ()
      _  -> pfail "expected ()"
----------------------------------------------------------------------------
instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
  parseJSON = withArray "(a,b)" $ \lst ->
    case lst of
      [a,b] -> liftM2 (,) (parseJSON a) (parseJSON b)
      _     -> pfail "expected (a,b)"
----------------------------------------------------------------------------
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a,b,c) where
  parseJSON = withArray "(a,b,c)" $ \lst ->
    case lst of
      [a,b,c] -> liftM3 (,,) (parseJSON a) (parseJSON b) (parseJSON c)
      _       -> pfail "expected (a,b,c)"
----------------------------------------------------------------------------
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSON (a,b,c,d) where
  parseJSON = withArray "(a,b,c,d)" $ \lst ->
    case lst of
      [a,b,c,d] -> liftM4 (,,,) (parseJSON a) (parseJSON b) (parseJSON c) (parseJSON d)
      _         -> pfail "expected (a,b,c,d)"
----------------------------------------------------------------------------
instance FromJSON a => FromJSON (Maybe a) where
  parseJSON Null = pure Nothing
  parseJSON j    = Just <$> parseJSON j
----------------------------------------------------------------------------
instance FromJSON Ordering where
  parseJSON = withText "{'LT','EQ','GT'}" $ \s ->
    case s of
      "LT" -> pure LT
      "EQ" -> pure EQ
      "GT" -> pure GT
      _    -> pfail "expected {'LT','EQ','GT'}"
----------------------------------------------------------------------------
instance FromJSON Char where
  parseJSON = withText "Char" $ \xs ->
    case xs of
     x | MS.length x == 1 -> pure (MS.head x)
       | otherwise -> pfail ("expected Char, received: " <> x)
----------------------------------------------------------------------------
instance FromJSON v => FromJSON (Map MisoString v) where
  parseJSON = withObject "FromJSON v => Map MisoString v" $ mapM parseJSON
----------------------------------------------------------------------------
withBool :: MisoString -> (Bool -> Parser a) -> Value -> Parser a
withBool _        f (Bool arr) = f arr
withBool expected _ v          = typeMismatch expected v
----------------------------------------------------------------------------
withText :: MisoString -> (MisoString -> Parser a) -> Value -> Parser a
withText _        f (String txt) = f txt
withText expected _ v            = typeMismatch expected v
----------------------------------------------------------------------------
withArray :: MisoString -> ([Value] -> Parser a) -> Value -> Parser a
withArray _        f (Array lst) = f lst
withArray expected _ v           = typeMismatch expected v
----------------------------------------------------------------------------
withObject :: MisoString -> (Object -> Parser a) -> Value -> Parser a
withObject _        f (Object obj) = f obj
withObject expected _ v            = typeMismatch expected v
----------------------------------------------------------------------------
withNumber :: MisoString -> (Double -> Parser a) -> Value -> Parser a
withNumber _        f (Number n) = f n
withNumber expected _ v          = typeMismatch expected v
----------------------------------------------------------------------------
typeMismatch :: MisoString -> Value -> Parser a
typeMismatch expected actual =
  pfail
    ( "typeMismatch: Expected " <> expected <> " but encountered " <> case actual of
        Object _ -> "Object"
        Array _ -> "Array"
        String _ -> "String"
        Number _ -> "Number"
        Bool _ -> "Boolean"
        Null -> "Null"
    )
----------------------------------------------------------------------------
#ifdef VANILLA
encode :: ToJSON a => a -> MisoString
encode = encodePure
#else
encode :: ToJSON a => a -> MisoString
encode x = unsafePerformIO $ jsonStringify =<< toJSVal_Value (toJSON x)
#endif
----------------------------------------------------------------------------
-- | Relies on the pure implementation of JSON parsing / serialization.
--
-- This can be used on the server or the client, it is more efficient to
-- use 'encode' on the client (since it relies on @JSON.stringify()@).
--
encodePure :: ToJSON a => a -> MisoString
encodePure = ms . toJSON
----------------------------------------------------------------------------
instance FromMisoString Value where
  fromMisoStringEither = Parser.decodePure
----------------------------------------------------------------------------
#ifdef VANILLA
-- | Escape special characters in a string for JSON serialization
-- Handles: \, ", and all JSON control characters per RFC 8259
escapeJSONString :: MisoString -> MisoString
escapeJSONString = MS.concatMap escapeChar
  where
    escapeChar :: Char -> MisoString
    escapeChar '\\' = "\\\\"   -- Backslash
    escapeChar '"'  = "\\\""   -- Double quote
    escapeChar '\b' = "\\b"    -- Backspace
    escapeChar '\f' = "\\f"    -- Form feed
    escapeChar '\n' = "\\n"    -- Newline
    escapeChar '\r' = "\\r"    -- Carriage return
    escapeChar '\t' = "\\t"    -- Tab
    escapeChar c
      | isControl c = ms ("\\u" <> padHex (ord c))  -- Other control chars as \uXXXX
      | otherwise   = singleton c

    padHex :: Int -> MisoString
    padHex n = MS.pack $ replicate (4 - length h) '0' ++ h
      where h = showHex n ""
----------------------------------------------------------------------------
#endif
instance ToMisoString Value where
  toMisoString = \case
    String s ->
#ifdef VANILLA
      "\"" <> escapeJSONString s <> "\""
#else
      "\"" <> s <> "\""
#endif
    Number n
      | (i, 0.0) <- properFraction n -> ms @Int i
      | otherwise -> ms n
    Null ->
      "null"
    Array xs ->
      "[" <> MS.intercalate "," (fmap ms xs) <> "]"
    Bool True ->
      "true"
    Bool False ->
      "false"
    Object o ->
      "{" <>
#ifdef VANILLA
        MS.intercalate "," [ "\"" <> escapeJSONString k <> "\"" <> ":" <> ms v | (k,v) <- M.toList o ]
#else
        MS.intercalate "," [ "\"" <> k <> "\"" <> ":" <> ms v | (k,v) <- M.toList o ]
#endif
      <> "}"
----------------------------------------------------------------------------
#ifdef VANILLA
decode :: FromJSON a => MisoString -> Maybe a
decode s
  | Right x <- Parser.decodePure s
  , Success v <- fromJSON x = v
  | otherwise = Nothing
#else
decode :: FromJSON a => MisoString -> Maybe a
decode s
  | Right x <- eitherDecode s = Just x
  | otherwise = Nothing
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_OLD
foreign import javascript unsafe
  "$r = JSON.stringify($1, null, $2)"
  encodePretty_ffi :: JSVal -> Int -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return JSON.stringify($1, null, $2); })"
  encodePretty_ffi :: JSVal -> Int -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign import javascript unsafe
  "return JSON.stringify($1, null, $2);"
  encodePretty_ffi :: JSVal -> Int -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
encodePretty' :: ToJSON a => Config -> a -> MisoString
encodePretty' = error "encodePretty': not implemented"
-----------------------------------------------------------------------------
encodePretty :: ToJSON a => a -> MisoString
encodePretty _ = error "encodePretty: not implemented"
-----------------------------------------------------------------------------
#else
-----------------------------------------------------------------------------
encodePretty' :: ToJSON a => Config -> a -> MisoString
encodePretty' (Config s) x = unsafePerformIO (flip encodePretty_ffi s =<< toJSVal_Value (toJSON x))
-----------------------------------------------------------------------------
encodePretty :: ToJSON a => a -> MisoString
encodePretty = encodePretty' defConfig
#endif
-----------------------------------------------------------------------------
newtype Config
  = Config
  { spaces :: Int
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
defConfig :: Config
defConfig = Config 4
-----------------------------------------------------------------------------
#ifdef GHCJS_OLD
foreign import javascript unsafe
  "$r = JSON.stringify($1)"
  jsonStringify :: JSVal -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return JSON.stringify($1); })"
  jsonStringify :: JSVal -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign import javascript unsafe
  "return JSON.stringify($1);"
  jsonStringify :: JSVal -> IO MisoString
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
jsonStringify :: JSVal -> IO MisoString
jsonStringify _ = error "jsonStringify: not implemented"
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_OLD
foreign import javascript unsafe
  "$r = JSON.parse($1)"
  jsonParse :: MisoString -> IO JSVal
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return JSON.parse($1); })"
  jsonParse :: MisoString -> IO JSVal
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign import javascript unsafe
  "return JSON.parse($1);"
  jsonParse :: MisoString -> IO JSVal
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
jsonParse :: MisoString -> IO JSVal
jsonParse _ = error "jsonParse: not implemented"
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
eitherDecode :: FromJSON a => MisoString -> Either MisoString a
eitherDecode string =
  case Parser.decodePure string of
    Left s ->
      Left (pack s)
    Right v ->
      parseEither parseJSON v
#else
eitherDecode :: FromJSON a => MisoString -> Either MisoString a
eitherDecode string = unsafePerformIO $ do
  (jsonParse string >>= fromJSVal_Value) >>= \case
    Nothing ->
      pure $ Left ("eitherDecode: " <> string)
    Just result ->
      pure (case fromJSON result of
        Success x -> Right x
        Error err -> Left err)
#endif
----------------------------------------------------------------------------
fromJSON :: FromJSON a => Value -> Result a
fromJSON value =
  case parseEither parseJSON value of
    Left s -> Error s
    Right x -> Success x
----------------------------------------------------------------------------
#ifdef GHCJS_BOTH
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = \case
  Null ->
    pure jsNull
  Bool bool_ ->
    Marshal.toJSVal bool_
  String string ->
    Marshal.toJSVal string
  Number double ->
    Marshal.toJSVal double
  Array arr ->
    toJSVal_List =<< mapM toJSVal_Value arr
  Object hms -> do
    o <- create_ffi
    forM_ (M.toList hms) $ \(k,v) -> do
      v' <- toJSVal_Value v
      setProp_ffi k v' o
    pure o
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_BOTH
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value jsval_ = do
  typeof jsval_ >>= \case
    0 -> return (Just Null)
    1 -> Just . Number <$> Marshal.fromJSValUnchecked jsval_
    2 -> Just . String <$> Marshal.fromJSValUnchecked jsval_
    3 -> fromJSValUnchecked_Int jsval_ >>= \case
      0 -> pure $ Just (Bool False)
      1 -> pure $ Just (Bool True)
      _ -> pure Nothing
    4 -> do xs <- Marshal.fromJSValUnchecked jsval_
            values <- forM xs fromJSVal_Value
            pure (Array <$> sequence values)
    5 -> do keys <- Marshal.fromJSValUnchecked =<< listProps_ffi jsval_
            result <-
              runMaybeT $ forM keys $ \k -> do
                key <- MaybeT (Marshal.fromJSVal k)
                raw <- MaybeT $ Just <$> getProp_ffi key jsval_
                value <- MaybeT (fromJSVal_Value raw)
                pure (key, value)
            pure (toObject <$> result)
    _ -> error "fromJSVal_Value: Unknown JSON type"
  where
    toObject = Object . M.fromList
#endif
-----------------------------------------------------------------------------
#ifdef WASM
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value jsval = do
  typeof jsval >>= \case
    0 -> return (Just Null)
    1 -> Just . Number <$> fromJSValUnchecked_Double jsval
    2 -> pure $ Just $ String $ (JSString jsval)
    3 -> fromJSValUnchecked_Int jsval >>= \case
      0 -> pure $ Just (Bool False)
      1 -> pure $ Just (Bool True)
      _ -> pure Nothing
    4 -> do xs <- fromJSValUnchecked_List jsval
            values <- forM xs fromJSVal_Value
            pure (Array <$> sequence values)
    5 -> do keys <- fromJSValUnchecked_List =<< listProps_ffi jsval
            result <-
              runMaybeT $ forM keys $ \k -> do
                let key = JSString k
                raw <- MaybeT $ Just <$> getProp_ffi key jsval
                value <- MaybeT (fromJSVal_Value raw)
                pure (key, value)
            pure (toObject <$> result)
    _ -> error "fromJSVal_Value: Unknown JSON type"
  where
    toObject = Object . M.fromList
#endif
-----------------------------------------------------------------------------
#ifdef VANILLA
-----------------------------------------------------------------------------
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value = error "fromJSVal_Value: not implemented"
-----------------------------------------------------------------------------
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = error "toJSVal_Value: not implemented"
-----------------------------------------------------------------------------
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_NEW
foreign import javascript unsafe
  "(($1) => { return globalThis.miso.typeOf($1); })"
  typeof :: JSVal -> IO Int
#endif
-----------------------------------------------------------------------------
#ifdef WASM
foreign import javascript unsafe
 "return globalThis.miso.typeOf($1);"
  typeof :: JSVal -> IO Int
#endif
-----------------------------------------------------------------------------
#ifdef GHCJS_OLD
foreign import javascript unsafe
  "$r = globalThis.miso.typeOf($1);"
  typeof :: JSVal -> IO Int
#endif
-----------------------------------------------------------------------------
#ifdef WASM
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = \case
  Null ->
    pure jsNull
  Bool bool_ ->
    toJSVal_Bool bool_
  String string ->
    toJSVal_JSString string
  Number double ->
    toJSVal_Double double
  Array arr ->
    toJSVal_List =<< mapM toJSVal_Value arr
  Object hms -> do
    o <- create_ffi
    forM_ (M.toList hms) $ \(k,v) -> do
      v' <- toJSVal_Value v
      setProp_ffi k v' o
    pure o
#endif
-----------------------------------------------------------------------------
