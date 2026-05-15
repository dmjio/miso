-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE CPP                  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.DSL
-- Copyright   :  (C) 2016-2026 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A JavaScript DSL for interacting with the browser or JS runtime environments.
-----------------------------------------------------------------------------
module Miso.DSL
  ( -- * Classes
    ToJSVal (..)
  , GToJSVal (..)
  , GToJSValSum (..)
  , GToJSValFields (..)
  , FromJSVal (..)
  , GFromJSVal (..)
  , GFromJSValSum (..)
  , GFromJSValFields (..)
  , ToArgs (..)
  , ToObject (..)
    -- * Types
  , JSVal
  , Object (..)
  , Function (..)
    -- * Utils
  , jsg
  , jsg0
  , jsg1
  , jsg2
  , jsg3
  , jsg4
  , jsg5
  , jsgf
  , global
  , (#)
  , setField
  , (<##)
  , (!)
  , listProps
  , call
  , new
  , create
  , createWith
  , setProp
  , getProp
  , eval
  , requestAnimationFrame
  , cancelAnimationFrame
  , freeFunction
  , (!!)
  , isUndefined
  , isNull
  , jsNull
  , syncCallback
  , syncCallback1
  , syncCallback2
  , syncCallback3
  , syncCallback'
  , syncCallback1'
  , syncCallback2'
  , syncCallback3'
  , asyncCallback
  , asyncCallback1
  , asyncCallback2
  , asyncCallback3
  , apply
  ) where
-----------------------------------------------------------------------------
import           Control.Applicative
#ifndef VANILLA
import           Data.Text (Text)
#endif
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           GHC.Generics
import           Data.Kind
import           Prelude hiding ((!!))
-----------------------------------------------------------------------------
import           Miso.DSL.FFI
import           Miso.JSON (Value, fromJSVal_Value, toJSVal_Value)
import           Miso.String
-----------------------------------------------------------------------------
-- | A class for marshaling Haskell values into JS
class ToJSVal a where
  toJSVal :: a -> IO JSVal
  default toJSVal :: (Generic a, GToJSVal (Rep a)) => a -> IO JSVal
  toJSVal = gToJSVal . from
-----------------------------------------------------------------------------
-- | Helper class for field-level JS serialization inside a constructor body.
class GToJSValFields (f :: Type -> Type) where
  gWriteFields :: f a -> Object -> IO ()
  gPositionals :: f a -> IO [JSVal]
  gIsRecord    :: f a -> Bool
  gFieldCount  :: f a -> Int
-----------------------------------------------------------------------------
instance GToJSValFields U1 where
  gWriteFields U1 _ = pure ()
  gPositionals U1   = pure []
  gIsRecord    _    = False
  gFieldCount  _    = 0
  {-# INLINE gWriteFields #-}
  {-# INLINE gPositionals #-}
  {-# INLINE gIsRecord    #-}
  {-# INLINE gFieldCount  #-}
-----------------------------------------------------------------------------
instance GToJSValFields V1 where
  gWriteFields _ _ = pure ()
  gPositionals _   = pure []
  gIsRecord    _   = False
  gFieldCount  _   = 0
  {-# INLINE gWriteFields #-}
  {-# INLINE gPositionals #-}
  {-# INLINE gIsRecord    #-}
  {-# INLINE gFieldCount  #-}
-----------------------------------------------------------------------------
instance (GToJSValFields f, GToJSValFields g) => GToJSValFields (f :*: g) where
  gWriteFields (x :*: y) o = gWriteFields x o >> gWriteFields y o
  gPositionals (x :*: y)   = (++) <$> gPositionals x <*> gPositionals y
  gIsRecord    (x :*: y)   = gIsRecord x
  gFieldCount  (x :*: y)   = gFieldCount x + gFieldCount y
  {-# INLINE gWriteFields #-}
  {-# INLINE gPositionals #-}
  {-# INLINE gIsRecord    #-}
  {-# INLINE gFieldCount  #-}
-----------------------------------------------------------------------------
instance (ToJSVal a, Selector s) => GToJSValFields (S1 s (K1 i a)) where
  gWriteFields (M1 (K1 x)) o
    | Prelude.null name = pure ()
    | otherwise = setField o (ms name) =<< toJSVal x
    where name = selName (undefined :: S1 s (K1 i a) ())
  gPositionals (M1 (K1 x))   = (:[]) <$> toJSVal x
  gIsRecord    _              = not $ Prelude.null (selName (undefined :: S1 s (K1 i a) ()))
  gFieldCount  _              = 1
  {-# INLINE gWriteFields #-}
  {-# INLINE gPositionals #-}
  {-# INLINE gIsRecord    #-}
  {-# INLINE gFieldCount  #-}
-----------------------------------------------------------------------------
-- | Generic JS serialization over a datatype's generic rep.
class GToJSVal (f :: Type -> Type) where
  gToJSVal :: f a -> IO JSVal
-----------------------------------------------------------------------------
-- | Single-constructor datatype encoding:
--
-- * 0 fields (nullary):  empty JS object @{}@
-- * Record fields:       flat object @{field1:v1, ...}@
-- * 1 positional field:  the value directly (unwrapped)
-- * N positional fields: JS array @[v1,...,vN]@
instance {-# OVERLAPPABLE #-} (Constructor c, GToJSValFields f)
    => GToJSVal (D1 i (C1 c f)) where
  gToJSVal (M1 (M1 x))
    | gFieldCount x == 0 = toJSVal =<< create
    | gIsRecord x = do
        o <- create
        gWriteFields x o
        toJSVal o
    | gFieldCount x == 1 = do
        [v] <- gPositionals x
        pure v
    | otherwise = toJSVal_List =<< gPositionals x
  {-# INLINE gToJSVal #-}
-----------------------------------------------------------------------------
-- | Sum datatype: each constructor is encoded with a @\"tag\"@ field.
instance {-# OVERLAPPING #-} (GToJSValSum f, GToJSValSum g)
    => GToJSVal (D1 i (f :+: g)) where
  gToJSVal (M1 x) = gToJSValSum x
  {-# INLINE gToJSVal #-}
-----------------------------------------------------------------------------
instance GToJSVal V1 where
  gToJSVal v = case v of
  {-# INLINE gToJSVal #-}
-----------------------------------------------------------------------------
-- | Encode one arm of a sum type, including a @\"tag\"@ field.
--
-- * Nullary constructor:      @\"ConstructorName\"@
-- * Record constructor:       @{\"tag\":\"Ctor\", field1:v1, ...}@
-- * Single-field constructor: @{\"tag\":\"Ctor\", \"contents\":v}@
-- * Multi-field constructor:  @{\"tag\":\"Ctor\", \"contents\":[v1,...,vN]}@
class GToJSValSum (f :: Type -> Type) where
  gToJSValSum :: f a -> IO JSVal
-----------------------------------------------------------------------------
instance (GToJSValSum f, GToJSValSum g) => GToJSValSum (f :+: g) where
  gToJSValSum (L1 x) = gToJSValSum x
  gToJSValSum (R1 x) = gToJSValSum x
  {-# INLINE gToJSValSum #-}
-----------------------------------------------------------------------------
instance (Constructor c, GToJSValFields f, ToJSVal MisoString) => GToJSValSum (C1 c f) where
  gToJSValSum (M1 x)
    | gFieldCount x == 0 =
        toJSVal (ms ctorName :: MisoString)
    | gIsRecord x = do
        o <- create
        setField o (ms ("tag" :: String)) =<< toJSVal (ms ctorName :: MisoString)
        gWriteFields x o
        toJSVal o
    | gFieldCount x == 1 = do
        [v] <- gPositionals x
        o   <- create
        setField o (ms ("tag" :: String)) =<< toJSVal (ms ctorName :: MisoString)
        setField o (ms ("contents" :: String)) v
        toJSVal o
    | otherwise = do
        vs  <- gPositionals x
        arr <- toJSVal_List vs
        o   <- create
        setField o (ms ("tag" :: String)) =<< toJSVal (ms ctorName :: MisoString)
        setField o (ms ("contents" :: String)) arr
        toJSVal o
    where
      ctorName = conName (undefined :: C1 c f ())
  {-# INLINE gToJSValSum #-}
-----------------------------------------------------------------------------
instance ToJSVal Bool where
  toJSVal = toJSVal_Bool
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal Double where
  toJSVal = toJSVal_Double
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal Float where
  toJSVal = toJSVal_Float
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal a => ToJSVal (IO a) where
  toJSVal action = toJSVal =<< action
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal () where
  toJSVal () = pure jsNull
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal Char where
  toJSVal = toJSVal_Char
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal Int where
  toJSVal = toJSVal_Int
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal a => ToJSVal (Map MisoString a) where
  toJSVal map_ = do
    o <- create
    forM_ (M.toList map_) $ \(k,v) ->
      setField o k =<< toJSVal v
    toJSVal o
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal a => FromJSVal (Map MisoString a) where
  fromJSVal o = pure <$> do foldM populate M.empty =<< listProps (Object o)
    where
      populate m k = do
        v <- fromJSValUnchecked =<< getProp k (Object o)
        pure (M.insert k v m)
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal a => ToObject (Map MisoString a) where
  toObject x = Object <$> toJSVal x
  {-# INLINE toObject #-}
-----------------------------------------------------------------------------
instance ToJSVal a => ToJSVal (Maybe a) where
  toJSVal = \case
    Nothing -> pure jsNull
    Just x -> toJSVal x
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} FromJSVal a => FromJSVal [a] where
  fromJSVal jsval_ = do
    fromJSVal_List jsval_ >>= \case
      Nothing -> pure Nothing
      Just xs -> sequence <$> mapM fromJSVal xs
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal [Char] where
  fromJSVal jsval_ = fmap unpack <$> fromJSVal jsval_
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal [Char] where
  toJSVal = toJSVal . toMisoString
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance {-# OVERLAPPABLE #-} ToJSVal a => ToJSVal [a] where
  toJSVal = toJSVal_List <=< mapM toJSVal
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal JSVal where
  toJSVal = pure
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal Value where
  fromJSVal = fromJSVal_Value
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
-- | A class for marshaling JS values into Haskell
class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)
  default fromJSVal :: (Generic a, GFromJSVal (Rep a)) => JSVal -> IO (Maybe a)
  fromJSVal x = fmap to <$> gFromJSVal x
  fromJSValUnchecked :: JSVal -> IO a
  fromJSValUnchecked x = do
    fromJSVal x >>= \case
      Nothing -> error "fromJSValUnchecked: failure"
      Just y -> pure y
-----------------------------------------------------------------------------
-- | Helper class for field-level JS deserialization inside a constructor body.
class GFromJSValFields (f :: Type -> Type) where
  -- | Read named record fields from an 'Object'.
  gReadFields      :: Object -> IO (Maybe (f a))
  -- | Read positional fields from a list of 'JSVal's.
  gReadPositionals :: [JSVal] -> IO (Maybe (f a))
  -- | 'True' when all fields carry selector names (record syntax).
  gIsRecordF   :: f a -> Bool
  -- | Number of fields.
  gFieldCountF :: f a -> Int
-----------------------------------------------------------------------------
instance GFromJSValFields U1 where
  gReadFields      _ = pure (Just U1)
  gReadPositionals _ = pure (Just U1)
  gIsRecordF       _ = False
  gFieldCountF     _ = 0
  {-# INLINE gReadFields      #-}
  {-# INLINE gReadPositionals #-}
  {-# INLINE gIsRecordF       #-}
  {-# INLINE gFieldCountF     #-}
-----------------------------------------------------------------------------
instance (GFromJSValFields f, GFromJSValFields g) => GFromJSValFields (f :*: g) where
  gIsRecordF   (x :*: _) = gIsRecordF x
  gFieldCountF (x :*: y) = gFieldCountF x + gFieldCountF y
  gReadFields o = runMaybeT $
    (:*:) <$> MaybeT (gReadFields o) <*> MaybeT (gReadFields o)
  gReadPositionals vs = runMaybeT $
    (:*:) <$> MaybeT (gReadPositionals (Prelude.take n vs))
          <*> MaybeT (gReadPositionals (Prelude.drop n vs))
    where n = gFieldCountF (undefined :: f ())
  {-# INLINE gIsRecordF       #-}
  {-# INLINE gFieldCountF     #-}
  {-# INLINE gReadFields      #-}
  {-# INLINE gReadPositionals #-}
-----------------------------------------------------------------------------
instance (FromJSVal a, Selector s) => GFromJSValFields (S1 s (K1 i a)) where
  gIsRecordF   _ = not $ Prelude.null (selName (undefined :: S1 s (K1 i a) ()))
  gFieldCountF _ = 1
  gReadFields o  = fmap (M1 . K1) <$> (fromJSVal =<< getProp (ms name) o)
    where name = selName (undefined :: S1 s (K1 i a) ())
  gReadPositionals (v:_) = fmap (M1 . K1) <$> fromJSVal v
  gReadPositionals []    = pure Nothing
  {-# INLINE gIsRecordF       #-}
  {-# INLINE gFieldCountF     #-}
  {-# INLINE gReadFields      #-}
  {-# INLINE gReadPositionals #-}
-----------------------------------------------------------------------------
-- | Generic JS deserialization over a datatype's generic rep.
class GFromJSVal (f :: Type -> Type) where
  gFromJSVal :: JSVal -> IO (Maybe (f a))
-----------------------------------------------------------------------------
-- | Single-constructor datatype decoding (mirrors the encoding in 'GToJSVal'):
--
-- * 0 fields (nullary):  always succeeds with @U1@
-- * Record fields:       read named fields from the flat object
-- * 1 positional field:  decode the JSVal directly as the field
-- * N positional fields: read array elements by index
instance {-# OVERLAPPABLE #-} (Constructor c, GFromJSValFields f)
    => GFromJSVal (D1 i (C1 c f)) where
  gFromJSVal x
    | n == 0    = fmap (M1 . M1) <$> gReadPositionals []
    | isRec     = fmap (M1 . M1) <$> gReadFields (Object x)
    | n == 1    = fmap (M1 . M1) <$> gReadPositionals [x]
    | otherwise = do
        vs <- sequence [ x !! i | i <- [0 .. n - 1] ]
        fmap (M1 . M1) <$> gReadPositionals vs
    where
      undef = undefined :: f ()
      isRec = gIsRecordF undef
      n     = gFieldCountF undef
  {-# INLINE gFromJSVal #-}
-----------------------------------------------------------------------------
-- | Sum datatype: dispatch on the @"tag"@ field.
instance {-# OVERLAPPING #-} (GFromJSValSum f, GFromJSValSum g)
    => GFromJSVal (D1 i (f :+: g)) where
  gFromJSVal x = fmap M1 <$> gFromJSValSum x
  {-# INLINE gFromJSVal #-}
-----------------------------------------------------------------------------
instance GFromJSVal V1 where
  gFromJSVal _ = pure Nothing
  {-# INLINE gFromJSVal #-}
-----------------------------------------------------------------------------
-- | Decode one arm of a sum type by matching the @"tag"@ field.
class GFromJSValSum (f :: Type -> Type) where
  gFromJSValSum :: JSVal -> IO (Maybe (f a))
-----------------------------------------------------------------------------
instance (GFromJSValSum f, GFromJSValSum g) => GFromJSValSum (f :+: g) where
  gFromJSValSum x = do
    ml <- gFromJSValSum x
    case ml of
      Just l  -> pure (Just (L1 l))
      Nothing -> fmap R1 <$> gFromJSValSum x
  {-# INLINE gFromJSValSum #-}
-----------------------------------------------------------------------------
instance (Constructor c, GFromJSValFields f, FromJSVal MisoString) => GFromJSValSum (C1 c f) where
  gFromJSValSum x
    | gFieldCountF undef == 0 = do
        mtag <- (fromJSVal x :: IO (Maybe MisoString))
        case mtag of
          Just tag | tag == ms ctorName -> fmap M1 <$> gReadPositionals []
          _                             -> pure Nothing
    | gIsRecordF undef = do
        tagVal <- x ! ms ("tag" :: String)
        mtag   <- (fromJSVal tagVal :: IO (Maybe MisoString))
        case mtag of
          Just tag | tag == ms ctorName -> fmap M1 <$> gReadFields (Object x)
          _                             -> pure Nothing
    | gFieldCountF undef == 1 = do
        tagVal <- x ! ms ("tag" :: String)
        mtag   <- (fromJSVal tagVal :: IO (Maybe MisoString))
        case mtag of
          Just tag | tag == ms ctorName -> do
            contents <- x ! ms ("contents" :: String)
            fmap M1 <$> gReadPositionals [contents]
          _ -> pure Nothing
    | otherwise = do
        tagVal <- x ! ms ("tag" :: String)
        mtag   <- (fromJSVal tagVal :: IO (Maybe MisoString))
        case mtag of
          Just tag | tag == ms ctorName -> do
            arr <- x ! ms ("contents" :: String)
            vs  <- sequence [ arr !! i | i <- [0 .. n - 1] ]
            fmap M1 <$> gReadPositionals vs
          _ -> pure Nothing
    where
      ctorName = conName (undefined :: C1 c f ())
      undef    = undefined :: f ()
      n        = gFieldCountF undef
  {-# INLINE gFromJSValSum #-}
-----------------------------------------------------------------------------
instance FromJSVal Int where
  fromJSVal = fromJSVal_Int
  fromJSValUnchecked = fromJSValUnchecked_Int
  {-# INLINE fromJSVal #-}
  {-# INLINE fromJSValUnchecked #-}
-----------------------------------------------------------------------------
instance FromJSVal Char where
  fromJSVal = fromJSVal_Char
  fromJSValUnchecked = fromJSValUnchecked_Char
  {-# INLINE fromJSVal #-}
  {-# INLINE fromJSValUnchecked #-}
-----------------------------------------------------------------------------
instance FromJSVal Float where
  fromJSVal = fromJSVal_Float
  fromJSValUnchecked = fromJSValUnchecked_Float
  {-# INLINE fromJSVal #-}
  {-# INLINE fromJSValUnchecked #-}
-----------------------------------------------------------------------------
instance FromJSVal Double where
  fromJSVal = fromJSVal_Double
  fromJSValUnchecked = fromJSValUnchecked_Double
  {-# INLINE fromJSVal #-}
  {-# INLINE fromJSValUnchecked #-}
-----------------------------------------------------------------------------
instance FromJSVal Text where
  fromJSVal = fromJSVal_Text
  fromJSValUnchecked = fromJSValUnchecked_Text
  {-# INLINE fromJSVal #-}
  {-# INLINE fromJSValUnchecked #-}
-----------------------------------------------------------------------------
instance ToObject Object where
  toObject = pure
  {-# INLINE toObject #-}
-----------------------------------------------------------------------------
instance ToJSVal Value where
  toJSVal = toJSVal_Value
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance ToJSVal Text where
  toJSVal = toJSVal_Text
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal () where
  fromJSVal _ = pure (Just ())
    -- if isUndefined_ffi x || isNull_ffi x
    --   then pure (Just ())
    --   else pure Nothing
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b) => ToJSVal (a,b) where
  toJSVal (y,z) = do
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ y_, z_ ]
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a,b,c) where
  toJSVal (x,y,z) = do
    x_ <- toJSVal x
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ x_, y_, z_ ]
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d) => ToJSVal (a,b,c,d) where
  toJSVal (w,x,y,z) = do
    w_ <- toJSVal w
    x_ <- toJSVal x
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ w_, x_, y_, z_ ]
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e) => ToJSVal (a,b,c,d,e) where
  toJSVal (v,w,x,y,z) = do
    v_ <- toJSVal v
    w_ <- toJSVal w
    x_ <- toJSVal x
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ v_, w_, x_, y_, z_ ]
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f) => ToJSVal (a,b,c,d,e,f) where
  toJSVal (u,v,w,x,y,z) = do
    u_ <- toJSVal u
    v_ <- toJSVal v
    w_ <- toJSVal w
    x_ <- toJSVal x
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ u_, v_, w_, x_, y_, z_ ]
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
-- | Retrieves a field from globalThis
jsg :: MisoString -> IO JSVal
jsg key = global ! key
{-# INLINABLE jsg #-}
-----------------------------------------------------------------------------
-- | Invokes a function with a specified argument list
jsgf :: ToArgs args => MisoString -> args -> IO JSVal
jsgf name = global # name
{-# INLINABLE jsgf #-}
-----------------------------------------------------------------------------
-- | Invokes a function with no argument
jsg0 :: MisoString -> IO JSVal
jsg0 name = jsgf name ([] :: [JSVal])
{-# INLINABLE jsg0 #-}
-----------------------------------------------------------------------------
-- | Invokes a function with 1 argument
jsg1 :: ToJSVal arg => MisoString -> arg -> IO JSVal
jsg1 name arg = jsgf name [arg]
{-# INLINABLE jsg1 #-}
-----------------------------------------------------------------------------
-- | Invokes a function with 2 arguments
jsg2 :: (ToJSVal arg1, ToJSVal arg2) => MisoString -> arg1 -> arg2 -> IO JSVal
jsg2 name arg1 arg2 = do
  arg1_ <- toJSVal arg1
  arg2_ <- toJSVal arg2
  jsgf name [arg1_, arg2_]
{-# INLINABLE jsg2 #-}
-----------------------------------------------------------------------------
-- | Invokes a function with 3 arguments
jsg3 :: (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3)
     => MisoString
     -> arg1
     -> arg2
     -> arg3
     -> IO JSVal
jsg3 name arg1 arg2 arg3 = do
  arg1_ <- toJSVal arg1
  arg2_ <- toJSVal arg2
  arg3_ <- toJSVal arg3
  jsgf name [arg1_, arg2_, arg3_]
{-# INLINABLE jsg3 #-}
-----------------------------------------------------------------------------
-- | Invokes a function with 4 arguments
jsg4 :: (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4)
     => MisoString
     -> arg1
     -> arg2
     -> arg3
     -> arg4
     -> IO JSVal
jsg4 name arg1 arg2 arg3 arg4 = do
  arg1_ <- toJSVal arg1
  arg2_ <- toJSVal arg2
  arg3_ <- toJSVal arg3
  arg4_ <- toJSVal arg4
  jsgf name [arg1_, arg2_, arg3_, arg4_]
{-# INLINABLE jsg4 #-}
-----------------------------------------------------------------------------
-- | Invokes a function with 5 arguments
jsg5 :: (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4, ToJSVal arg5)
     => MisoString
     -> arg1
     -> arg2
     -> arg3
     -> arg4
     -> arg5
     -> IO JSVal
jsg5 name arg1 arg2 arg3 arg4 arg5 = do
  arg1_ <- toJSVal arg1
  arg2_ <- toJSVal arg2
  arg3_ <- toJSVal arg3
  arg4_ <- toJSVal arg4
  arg5_ <- toJSVal arg5
  jsgf name [arg1_, arg2_, arg3_, arg4_, arg5_]
{-# INLINABLE jsg5 #-}
-----------------------------------------------------------------------------
-- | Sets a field on an Object at a specified field
setField :: (ToObject o, ToJSVal v) => o -> MisoString -> v -> IO ()
setField o k v = do
  o' <- toJSVal =<< toObject o
  v' <- toJSVal v
  setProp_ffi k v' o'
{-# INLINABLE setField #-}
-----------------------------------------------------------------------------
-- | Sets a field on an Object at a specified index
infixr 1 <##
(<##) :: (ToObject o, ToJSVal v) => o -> Int -> v -> IO ()
(<##) o k v = do
  o' <- toJSVal =<< toObject o
  v' <- toJSVal v
  setPropIndex_ffi k v' o'
{-# INLINABLE (<##) #-}
-----------------------------------------------------------------------------
-- | Retrieves a property from an Object
(!) :: ToObject o => o -> MisoString -> IO JSVal
(!) = flip getProp
{-# INLINABLE (!) #-}
-----------------------------------------------------------------------------
-- | Lists the properties on a JS Object.
listProps :: Object -> IO [MisoString]
listProps (Object jsval) = do
  keys <- fromJSValUnchecked =<< listProps_ffi jsval
  forM keys fromJSValUnchecked
{-# INLINABLE listProps #-}
-----------------------------------------------------------------------------
-- | Calls a JS function on an t'Object' at a field with specified arguments.
call :: (ToObject obj, ToObject this, ToArgs args) => obj -> this -> args -> IO JSVal
call o this args = do
  o' <- toJSVal =<< toObject o
  this' <- toJSVal =<< toObject this
  args' <- toJSVal =<< toArgs args
  invokeFunction o' this' args'
{-# INLINABLE call #-}
-----------------------------------------------------------------------------
-- | Calls a JS function on an t'Object' at a field with specified arguments.
infixr 2 #
(#) :: (ToObject object, ToArgs args) => object -> MisoString -> args -> IO JSVal
(#) o k args = do
  o' <- toJSVal =<< toObject o
  func <- getProp_ffi k o'
  args' <- toJSVal =<< toArgs args
  invokeFunction func o' args'
{-# INLINABLE (#) #-}
-----------------------------------------------------------------------------
apply :: (FromJSVal a, ToArgs args) => Function -> args -> IO a
apply (Function func) args = do
  o <- toJSVal global
  fromJSValUnchecked =<< do
    invokeFunction func o =<<
      toJSVal (toArgs args)
{-# INLINABLE apply #-}
-----------------------------------------------------------------------------
-- | Instantiates a new JS t'Object'.
new :: (ToObject constructor, ToArgs args) => constructor -> args -> IO JSVal
new constr args = do
  obj <- toJSVal =<< toObject constr
  argv <- toJSVal =<< toArgs args
  new_ffi obj argv
{-# INLINABLE new #-}
-----------------------------------------------------------------------------
-- | Creates a new JS t'Object'
create :: IO Object
create = Object <$> create_ffi
{-# INLINABLE create #-}
-----------------------------------------------------------------------------
-- | Creates a new JS t'Object' populated with key-value pairs specified
-- in the list. Meant for use with 'inline' JS functionality.
--
-- @
-- update = \case
--  Highlight domRef -> do
--    3 <- inline "hljs.highlight(domRef); return 3;" =<<
--      createWith [ "domRef" =: domRef ]
--    pure ()
-- @
--
createWith :: ToJSVal val => [(MisoString, val)] -> IO Object
createWith kvs = do
  o <- create
  forM_ kvs $ \(k,v) ->
    flip (setProp k) o =<< toJSVal v
  pure o
{-# INLINABLE createWith #-}
-----------------------------------------------------------------------------
-- | Sets a property on a JS t'Object'
setProp :: ToJSVal val => MisoString -> val -> Object -> IO ()
setProp k v (Object o) = flip (setProp_ffi k) o =<< toJSVal v
{-# INLINABLE setProp #-}
-----------------------------------------------------------------------------
-- | Retrieves a property from a JS t'Object'
getProp :: ToObject o => MisoString -> o -> IO JSVal
getProp k v = getProp_ffi k =<< toJSVal (toObject v)
{-# INLINABLE getProp #-}
-----------------------------------------------------------------------------
-- | Dynamically evaluates a JS string. See [eval](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval)
--
-- `eval()` is slower (not subject to JS engine optimizations) and also
-- has security vulnerabilities (can alter other local variables).
--
-- Consider using the more performant and secure (isolated) `inline` function.
--
eval :: MisoString -> IO JSVal
eval = eval_ffi
{-# INLINABLE eval #-}
-----------------------------------------------------------------------------
instance FromJSVal Bool where
  fromJSVal = fromJSVal_Bool
  fromJSValUnchecked = fromJSValUnchecked_Bool
  {-# INLINE fromJSVal #-}
  {-# INLINE fromJSValUnchecked #-}
-----------------------------------------------------------------------------
instance FromJSVal JSVal where
  fromJSVal = pure . Just
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal a => FromJSVal (Maybe a) where
  fromJSVal x = fromJSVal_Maybe x >>= \case
    Nothing -> pure Nothing
    Just Nothing -> pure (Just Nothing)
    Just (Just y) -> fmap Just <$> fromJSVal y
  fromJSValUnchecked x = fromJSValUnchecked_Maybe x >>= \case
    Nothing -> pure Nothing
    Just y -> Just <$> fromJSValUnchecked y
  {-# INLINE fromJSVal #-}
  {-# INLINE fromJSValUnchecked #-}
-----------------------------------------------------------------------------
-- | A class for creating arguments to a JS function
class ToArgs args where
  toArgs :: args -> IO [JSVal]
-----------------------------------------------------------------------------
instance ToArgs Double where
  toArgs x = (:[]) <$> toJSVal x
  {-# INLINE toArgs #-}
-----------------------------------------------------------------------------
instance ToArgs JSVal where
  toArgs val = pure [val]
  {-# INLINE toArgs #-}
-----------------------------------------------------------------------------
instance ToObject JSVal where
  toObject = pure . Object
  {-# INLINE toObject #-}
-----------------------------------------------------------------------------
-- | A class for creating JS objects.
class ToObject a where
  toObject :: a -> IO Object
  default toObject :: (Generic a, GToJSVal (Rep a)) => a -> IO Object
  toObject x = Object <$> gToJSVal (from x)
-----------------------------------------------------------------------------
instance ToJSVal a => ToObject (IO a) where
  toObject action = Object <$> (toJSVal =<< action)
  {-# INLINE toObject #-}
-----------------------------------------------------------------------------
instance ToArgs MisoString where
  toArgs arg = (:[]) <$> toJSVal arg
  {-# INLINE toArgs #-}
----------------------------------------------------------------------------
#ifndef VANILLA
----------------------------------------------------------------------------
instance ToJSVal MisoString where
  toJSVal = toJSVal_JSString
  {-# INLINE toJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal MisoString where
  fromJSVal = fromJSVal_JSString
  {-# INLINE fromJSVal #-}
----------------------------------------------------------------------------
#endif
----------------------------------------------------------------------------
instance ToJSVal arg => ToArgs [arg] where
    toArgs = mapM toJSVal
    {-# INLINE toArgs #-}
----------------------------------------------------------------------------
instance ToArgs () where
  toArgs _ = pure []
  {-# INLINE toArgs #-}
----------------------------------------------------------------------------
instance ToArgs Int where
  toArgs k = (:[]) <$> toJSVal k
  {-# INLINE toArgs #-}
-----------------------------------------------------------------------------
instance ToArgs Function where
  toArgs k = (:[]) <$> toJSVal k
  {-# INLINE toArgs #-}
-----------------------------------------------------------------------------
instance ToArgs Bool where
  toArgs k = (:[]) <$> toJSVal k
  {-# INLINE toArgs #-}
-----------------------------------------------------------------------------
instance ToArgs Object where
  toArgs k = (:[]) <$> toJSVal k
  {-# INLINE toArgs #-}
-----------------------------------------------------------------------------
instance ToArgs args => ToArgs (Maybe args) where
  toArgs (Just args) = toArgs args
  toArgs Nothing     = pure []
  {-# INLINE toArgs #-}
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2) => ToArgs (arg1, arg2) where
  toArgs (arg1, arg2) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    return [rarg1, rarg2]
  {-# INLINE toArgs #-}
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3) => ToArgs (arg1, arg2, arg3) where
  toArgs (arg1, arg2, arg3) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    return [rarg1, rarg2, rarg3]
  {-# INLINE toArgs #-}
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4) => ToArgs (arg1, arg2, arg3, arg4) where
  toArgs (arg1, arg2, arg3, arg4) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    rarg4 <- toJSVal arg4
    return [rarg1, rarg2, rarg3, rarg4]
  {-# INLINE toArgs #-}
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4, ToJSVal arg5) => ToArgs (arg1, arg2, arg3, arg4, arg5) where
  toArgs (arg1, arg2, arg3, arg4, arg5) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    rarg4 <- toJSVal arg4
    rarg5 <- toJSVal arg5
    return [rarg1, rarg2, rarg3, rarg4, rarg5]
  {-# INLINE toArgs #-}
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4, ToJSVal arg5, ToJSVal arg6) => ToArgs (arg1, arg2, arg3, arg4, arg5, arg6) where
  toArgs (arg1, arg2, arg3, arg4, arg5, arg6) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    rarg4 <- toJSVal arg4
    rarg5 <- toJSVal arg5
    rarg6 <- toJSVal arg6
    return [rarg1, rarg2, rarg3, rarg4, rarg5, rarg6]
  {-# INLINE toArgs #-}
----------------------------------------------------------------------------
-- | Frees references to a callback
freeFunction :: Function -> IO ()
freeFunction (Function x) = freeFunction_ffi x
{-# INLINABLE freeFunction #-}
-----------------------------------------------------------------------------
instance FromJSVal Function where
  fromJSVal = pure . Just . Function
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
instance FromJSVal Object where
  fromJSVal = pure . Just . Object
  {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
-- | Lookup a property based on its index
(!!) :: ToObject object => object -> Int -> IO JSVal
(!!) o k = getPropIndex_ffi k =<< toJSVal =<< toObject o
{-# INLINABLE (!!) #-}
-----------------------------------------------------------------------------
-- | Checks if a t'JSVal' is undefined
isUndefined :: ToJSVal val => val -> IO Bool
isUndefined val = isUndefined_ffi <$> toJSVal val
{-# INLINABLE isUndefined #-}
-----------------------------------------------------------------------------
-- | Checks if a t'JSVal' is null
isNull :: ToJSVal val => val -> IO Bool
isNull val = isNull_ffi <$> toJSVal val
{-# INLINABLE isNull #-}
-----------------------------------------------------------------------------
-- | A JS Object
newtype Object = Object { unObject :: JSVal } deriving newtype (ToJSVal, Eq)
-----------------------------------------------------------------------------
-- | A JS Functionn
newtype Function = Function { unFunction :: JSVal } deriving newtype (ToJSVal, Eq)
-----------------------------------------------------------------------------
instance (FromJSVal a, FromJSVal b) => FromJSVal (a,b) where
    fromJSVal r = runMaybeT $ (,) <$> jf r 0 <*> jf r 1
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c) => FromJSVal (a,b,c) where
    fromJSVal r = runMaybeT $ (,,) <$> jf r 0 <*> jf r 1 <*> jf r 2
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d) => FromJSVal (a,b,c,d) where
    fromJSVal r = runMaybeT $ (,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e) => FromJSVal (a,b,c,d,e) where
    fromJSVal r = runMaybeT $ (,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f) => FromJSVal (a,b,c,d,e,f) where
    fromJSVal r = runMaybeT $ (,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f, FromJSVal g) => FromJSVal (a,b,c,d,e,f,g) where
    fromJSVal r = runMaybeT $ (,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f, FromJSVal g, FromJSVal h) => FromJSVal (a,b,c,d,e,f,g,h) where
    fromJSVal r = runMaybeT $ (,,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6 <*> jf r 7
    {-# INLINE fromJSVal #-}
-----------------------------------------------------------------------------
jf :: FromJSVal a => JSVal -> Int -> MaybeT IO a
{-# INLINE jf #-}
jf r n = MaybeT $ do
  x <- getPropIndex_ffi n r
  if isUndefined_ffi r
    then return Nothing
    else fromJSVal x
-----------------------------------------------------------------------------
