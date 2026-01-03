-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE CPP                  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.DSL
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
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
  , FromJSVal (..)
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
  ) where
-----------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson (Value)
#ifndef VANILLA
import Data.Text (Text)
#endif
import Control.Monad
import Control.Monad.Trans.Maybe
import GHC.Generics
import Data.Kind
import Prelude hiding ((!!))
-----------------------------------------------------------------------------
import Miso.DSL.FFI
import Miso.String
-----------------------------------------------------------------------------
-- | A class for marhsaling Haskell values into JS
class ToJSVal a where
  toJSVal :: a -> IO JSVal
  default toJSVal :: (Generic a, GToJSVal (Rep a)) => a -> IO JSVal
  toJSVal x = do
    o <- create
    gToJSVal (from x) o
    toJSVal o
-----------------------------------------------------------------------------
class GToJSVal (f :: Type -> Type) where
  gToJSVal :: f a -> Object -> IO ()
-----------------------------------------------------------------------------
instance GToJSVal a => GToJSVal (D1 i a) where
  gToJSVal (M1 x) = gToJSVal x
-----------------------------------------------------------------------------
instance GToJSVal a => GToJSVal (C1 i a) where
  gToJSVal (M1 x) = gToJSVal x
-----------------------------------------------------------------------------
instance (GToJSVal a, GToJSVal b) => GToJSVal (a :*: b) where
  gToJSVal (x :*: y) o = gToJSVal x o >> gToJSVal y o
-----------------------------------------------------------------------------
instance (GToJSVal a, GToJSVal b) => GToJSVal (a :+: b) where
  gToJSVal = \case
    L1 x -> gToJSVal x
    R1 x -> gToJSVal x
-----------------------------------------------------------------------------
instance (ToJSVal a, Selector s) => GToJSVal (S1 s (K1 i a)) where
  gToJSVal m@(M1 (K1 x)) o = do
    setField o fieldName =<< toJSVal x
      where
        fieldName = ms (selName m)
-----------------------------------------------------------------------------
instance GToJSVal U1 where
  gToJSVal U1 _ = pure ()
-----------------------------------------------------------------------------
instance ToJSVal Bool where
  toJSVal = toJSVal_Bool
-----------------------------------------------------------------------------
instance ToJSVal Double where
  toJSVal = toJSVal_Double
-----------------------------------------------------------------------------
instance ToJSVal Float where
  toJSVal = toJSVal_Float
-----------------------------------------------------------------------------
instance ToJSVal a => ToJSVal (IO a) where
  toJSVal action = toJSVal =<< action
-----------------------------------------------------------------------------
instance ToJSVal () where
  toJSVal () = pure jsNull
-----------------------------------------------------------------------------
instance ToJSVal Char where
  toJSVal = toJSVal_Char
-----------------------------------------------------------------------------
instance ToJSVal Int where
  toJSVal = toJSVal_Int
-----------------------------------------------------------------------------
instance ToJSVal a => ToJSVal (Maybe a) where
  toJSVal = \case
    Nothing -> pure jsNull
    Just x -> toJSVal x
-----------------------------------------------------------------------------
instance FromJSVal a => FromJSVal [a] where
  fromJSVal jsval_ = do
    fromJSVal_List jsval_ >>= \case
      Nothing -> pure Nothing
      Just xs -> sequence <$> mapM fromJSVal xs
-----------------------------------------------------------------------------
instance ToJSVal a => ToJSVal [a] where
  toJSVal = toJSVal_List <=< mapM toJSVal
-----------------------------------------------------------------------------
instance ToJSVal JSVal where
  toJSVal = pure
-----------------------------------------------------------------------------
instance FromJSVal Value where
  fromJSVal = fromJSVal_Value
-----------------------------------------------------------------------------
-- | A class for marhsaling JS values into Haskell
class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)
  default fromJSVal :: (Generic a, GFromJSVal (Rep a)) => JSVal -> IO (Maybe a)
  fromJSVal x = fmap to <$> gFromJSVal (Object x)
  fromJSValUnchecked :: JSVal -> IO a
  fromJSValUnchecked x = do
    fromJSVal x >>= \case
      Nothing -> error "fromJSValUnchecked: failure"
      Just y -> pure y
-----------------------------------------------------------------------------
class GFromJSVal (f :: Type -> Type) where
  gFromJSVal :: Object -> IO (Maybe (f a))
-----------------------------------------------------------------------------
instance GFromJSVal a => GFromJSVal (D1 i a) where
  gFromJSVal o = fmap M1 <$> gFromJSVal o
-----------------------------------------------------------------------------
instance GFromJSVal a => GFromJSVal (C1 i a) where
  gFromJSVal o = fmap M1 <$> gFromJSVal o
-----------------------------------------------------------------------------
instance GFromJSVal U1 where
  gFromJSVal _ = pure (Just U1)
-----------------------------------------------------------------------------
instance (GFromJSVal a, GFromJSVal b) => GFromJSVal (a :*: b) where
  gFromJSVal o = runMaybeT $ (:*:) <$> MaybeT (gFromJSVal o) <*> MaybeT (gFromJSVal o)
-----------------------------------------------------------------------------
instance (GFromJSVal a, GFromJSVal b) => GFromJSVal (a :+: b) where
  gFromJSVal o = do
    x <- fmap L1 <$> gFromJSVal o
    case x of
      Nothing -> fmap R1 <$> gFromJSVal o
      Just y -> pure (Just y)
-----------------------------------------------------------------------------
instance (FromJSVal a, Selector s) => GFromJSVal (S1 s (K1 i a)) where
  gFromJSVal o = fmap (M1 . K1) <$> do fromJSVal =<< getProp (ms name) o
    where
      name = selName (undefined :: S1 s (K1 i a) ())
-----------------------------------------------------------------------------
instance FromJSVal Int where
  fromJSVal = fromJSVal_Int
  fromJSValUnchecked = fromJSValUnchecked_Int
-----------------------------------------------------------------------------
instance FromJSVal Char where
  fromJSVal = fromJSVal_Char
  fromJSValUnchecked = fromJSValUnchecked_Char
-----------------------------------------------------------------------------
instance FromJSVal Float where
  fromJSVal = fromJSVal_Float
  fromJSValUnchecked = fromJSValUnchecked_Float
-----------------------------------------------------------------------------
instance FromJSVal Double where
  fromJSVal = fromJSVal_Double
  fromJSValUnchecked = fromJSValUnchecked_Double
-----------------------------------------------------------------------------
instance FromJSVal Text where
  fromJSVal = fromJSVal_Text
  fromJSValUnchecked = fromJSValUnchecked_Text
-----------------------------------------------------------------------------
instance ToObject Object where
  toObject = pure
-----------------------------------------------------------------------------
instance ToJSVal Value where
  toJSVal = toJSVal_Value
-----------------------------------------------------------------------------
instance ToJSVal Text where
  toJSVal = toJSVal_Text
-----------------------------------------------------------------------------
instance FromJSVal () where
  fromJSVal x =
    if isUndefined_ffi x || isNull_ffi x
      then pure (Just ())
      else pure Nothing
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b) => ToJSVal (a,b) where
  toJSVal (y,z) = do
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ y_, z_ ]
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a,b,c) where
  toJSVal (x,y,z) = do
    x_ <- toJSVal x
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ x_, y_, z_ ]
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d) => ToJSVal (a,b,c,d) where
  toJSVal (w,x,y,z) = do
    w_ <- toJSVal w
    x_ <- toJSVal x
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ w_, x_, y_, z_ ]
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e) => ToJSVal (a,b,c,d,e) where
  toJSVal (v,w,x,y,z) = do
    v_ <- toJSVal v
    w_ <- toJSVal w
    x_ <- toJSVal x
    y_ <- toJSVal y
    z_ <- toJSVal z
    toJSVal_List [ v_, w_, x_, y_, z_ ]
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
-----------------------------------------------------------------------------
-- | Retrieves a field from globalThis
jsg :: MisoString -> IO JSVal
jsg key = global ! key
-----------------------------------------------------------------------------
-- | Invokes a function with a specified argument list
jsgf :: ToArgs args => MisoString -> args -> IO JSVal
jsgf name = global # name
-----------------------------------------------------------------------------
-- | Invokes a function with no argument
jsg0 :: MisoString -> IO JSVal
jsg0 name = jsgf name ([] :: [JSVal])
-----------------------------------------------------------------------------
-- | Invokes a function with 1 argument
jsg1 :: ToJSVal arg => MisoString -> arg -> IO JSVal
jsg1 name arg = jsgf name [arg]
-----------------------------------------------------------------------------
-- | Invokes a function with 2 arguments
jsg2 :: (ToJSVal arg1, ToJSVal arg2) => MisoString -> arg1 -> arg2 -> IO JSVal
jsg2 name arg1 arg2 = do
  arg1_ <- toJSVal arg1
  arg2_ <- toJSVal arg2
  jsgf name [arg1_, arg2_]
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
-----------------------------------------------------------------------------
-- | Sets a field on an Object at a specified field
setField :: (ToObject o, ToJSVal v) => o -> MisoString -> v -> IO ()
setField o k v = do
  o' <- toJSVal =<< toObject o
  v' <- toJSVal v
  setProp_ffi k v' o'
-----------------------------------------------------------------------------
-- | Sets a field on an Object at a specified index
infixr 1 <##
(<##) :: (ToObject o, ToJSVal v) => o -> Int -> v -> IO ()
(<##) o k v = do
  o' <- toJSVal =<< toObject o
  v' <- toJSVal v
  setPropIndex_ffi k v' o'
-----------------------------------------------------------------------------
-- | Retrieves a property from an Object
(!) :: ToObject o => o -> MisoString -> IO JSVal
(!) = flip getProp
-----------------------------------------------------------------------------
-- | Lists the properties on a JS Object.
listProps :: Object -> IO [MisoString]
listProps (Object jsval) = do
  keys <- fromJSValUnchecked =<< listProps_ffi jsval
  forM keys fromJSValUnchecked
-----------------------------------------------------------------------------
-- | Calls a JS function on an t'Object' at a field with specified arguments.
call :: (ToObject obj, ToObject this, ToArgs args) => obj -> this -> args -> IO JSVal
call o this args = do
  o' <- toJSVal =<< toObject o
  this' <- toJSVal =<< toObject this
  args' <- toJSVal =<< toArgs args
  invokeFunction o' this' args'
-----------------------------------------------------------------------------
-- | Calls a JS function on an t'Object' at a field with specified arguments.
infixr 2 #
(#) :: (ToObject object, ToArgs args) => object -> MisoString -> args -> IO JSVal
(#) o k args = do
  o' <- toJSVal =<< toObject o
  func <- getProp_ffi k o'
  args' <- toJSVal =<< toArgs args
  invokeFunction func o' args'
-----------------------------------------------------------------------------
-- | Instantiates a new JS t'Object'.
new :: (ToObject constructor, ToArgs args) => constructor -> args -> IO JSVal
new constr args = do
  obj <- toJSVal =<< toObject constr
  argv <- toJSVal =<< toArgs args
  new_ffi obj argv
-----------------------------------------------------------------------------
-- | Creates a new JS t'Object'
create :: IO Object
create = Object <$> create_ffi
-----------------------------------------------------------------------------
-- | Sets a property on a JS t'Object'
setProp :: ToJSVal val => MisoString -> val -> Object -> IO ()
setProp k v (Object o) = flip (setProp_ffi k) o =<< toJSVal v
-----------------------------------------------------------------------------
-- | Retrieves a property from a JS t'Object'
getProp :: ToObject o => MisoString -> o -> IO JSVal
getProp k v = getProp_ffi k =<< toJSVal (toObject v)
-----------------------------------------------------------------------------
-- | Dynamically evaluates a JS string. See [eval](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval)
--
eval :: MisoString -> IO JSVal
eval = eval_ffi
-----------------------------------------------------------------------------
instance FromJSVal Bool where
  fromJSVal = fromJSVal_Bool
  fromJSValUnchecked = fromJSValUnchecked_Bool
-----------------------------------------------------------------------------
instance FromJSVal JSVal where
  fromJSVal = pure . Just
-----------------------------------------------------------------------------
instance FromJSVal a => FromJSVal (Maybe a) where
  fromJSVal x = fromJSVal_Maybe x >>= \case
    Nothing -> pure Nothing
    Just Nothing -> pure (Just Nothing)
    Just (Just y) -> fmap Just <$> fromJSVal y
  fromJSValUnchecked x = fromJSValUnchecked_Maybe x >>= \case
    Nothing -> pure Nothing
    Just y -> Just <$> fromJSValUnchecked y
-----------------------------------------------------------------------------
-- | A class for creating arguments to a JS function
class ToArgs args where
  toArgs :: args -> IO [JSVal]
-----------------------------------------------------------------------------
instance ToArgs Double where
  toArgs x = (:[]) <$> toJSVal x
-----------------------------------------------------------------------------
instance ToArgs JSVal where
  toArgs val = pure [val]
-----------------------------------------------------------------------------
instance ToObject JSVal where
  toObject = pure . Object
-----------------------------------------------------------------------------
-- | A class for creating JS objects.
class ToObject a where
  toObject :: a -> IO Object
-----------------------------------------------------------------------------
instance ToJSVal a => ToObject (IO a) where
  toObject action = Object <$> (toJSVal =<< action)
-----------------------------------------------------------------------------
instance ToArgs MisoString where
  toArgs arg = (:[]) <$> toJSVal arg
----------------------------------------------------------------------------
#ifndef VANILLA
----------------------------------------------------------------------------
instance ToJSVal MisoString where
  toJSVal = toJSVal_JSString
-----------------------------------------------------------------------------
instance FromJSVal MisoString where
  fromJSVal = fromJSVal_JSString
----------------------------------------------------------------------------
#endif
----------------------------------------------------------------------------
instance ToJSVal arg => ToArgs [arg] where
    toArgs = mapM toJSVal
----------------------------------------------------------------------------
instance ToArgs () where
  toArgs _ = pure []
----------------------------------------------------------------------------
instance ToArgs Int where
  toArgs k = (:[]) <$> toJSVal k
-----------------------------------------------------------------------------
instance ToArgs Function where
  toArgs k = (:[]) <$> toJSVal k
-----------------------------------------------------------------------------
instance ToArgs Bool where
  toArgs k = (:[]) <$> toJSVal k
-----------------------------------------------------------------------------
instance ToArgs Object where
  toArgs k = (:[]) <$> toJSVal k
-----------------------------------------------------------------------------
instance ToArgs args => ToArgs (Maybe args) where
  toArgs (Just args) = toArgs args
  toArgs Nothing     = pure []
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2) => ToArgs (arg1, arg2) where
  toArgs (arg1, arg2) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    return [rarg1, rarg2]
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3) => ToArgs (arg1, arg2, arg3) where
  toArgs (arg1, arg2, arg3) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    return [rarg1, rarg2, rarg3]
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4) => ToArgs (arg1, arg2, arg3, arg4) where
  toArgs (arg1, arg2, arg3, arg4) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    rarg4 <- toJSVal arg4
    return [rarg1, rarg2, rarg3, rarg4]
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4, ToJSVal arg5) => ToArgs (arg1, arg2, arg3, arg4, arg5) where
  toArgs (arg1, arg2, arg3, arg4, arg5) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    rarg4 <- toJSVal arg4
    rarg5 <- toJSVal arg5
    return [rarg1, rarg2, rarg3, rarg4, rarg5]
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
----------------------------------------------------------------------------
-- | Frees references to a callback
freeFunction :: Function -> IO ()
freeFunction (Function x) = freeFunction_ffi x
-----------------------------------------------------------------------------
instance FromJSVal Function where
  fromJSVal = pure . Just . Function
-----------------------------------------------------------------------------
instance FromJSVal Object where
  fromJSVal = pure . Just . Object
-----------------------------------------------------------------------------
-- | Lookup a property based on its index
(!!) :: ToObject object => object -> Int -> IO JSVal
(!!) o k = getPropIndex_ffi k =<< toJSVal =<< toObject o
-----------------------------------------------------------------------------
-- | Checks if a t'JSVal' is undefined
isUndefined :: ToJSVal val => val -> IO Bool
isUndefined val = isUndefined_ffi <$> toJSVal val
-----------------------------------------------------------------------------
-- | Checks if a t'JSVal' is null
isNull :: ToJSVal val => val -> IO Bool
isNull val = isNull_ffi <$> toJSVal val
-----------------------------------------------------------------------------
-- | A JS Object
newtype Object = Object { unObject :: JSVal } deriving (ToJSVal, Generic)
-----------------------------------------------------------------------------
-- | A JS Function
newtype Function = Function { unFunction :: JSVal } deriving (ToJSVal, Generic)
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
jf r n = MaybeT $ do
  x <- getPropIndex_ffi n r
  if isUndefined_ffi r
    then return Nothing
    else fromJSVal x
-----------------------------------------------------------------------------
