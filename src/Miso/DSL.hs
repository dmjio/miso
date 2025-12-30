-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
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
  , jsg1
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
  , waitForAnimationFrame
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
import Data.Aeson (Value)
import Control.Monad
import Prelude hiding ((!!))
-----------------------------------------------------------------------------
import Miso.DSL.FFI
import Miso.String
-----------------------------------------------------------------------------
class ToJSVal a where
  toJSVal :: a -> IO JSVal
-----------------------------------------------------------------------------
instance ToJSVal Bool where
  toJSVal = toJSVal_Bool
-----------------------------------------------------------------------------
instance ToJSVal (IO JSVal) where
  toJSVal action = action
-----------------------------------------------------------------------------
instance ToJSVal Double where
  toJSVal = toJSVal_Double
-----------------------------------------------------------------------------
instance ToJSVal a => ToJSVal (IO a) where
  toJSVal action = toJSVal =<< action
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
class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)
  fromJSValUnchecked :: JSVal -> IO a
  fromJSValUnchecked x = do
    fromJSVal x >>= \case
      Nothing -> error "fromJSValUnchecked: failure"
      Just y -> pure y
-----------------------------------------------------------------------------
instance FromJSVal Int where
  fromJSVal = fromJSVal_Int
  fromJSValUnchecked = fromJSValUnchecked_Int
-----------------------------------------------------------------------------
instance FromJSVal Double where
  fromJSVal = fromJSVal_Double
  fromJSValUnchecked = fromJSValUnchecked_Double
-----------------------------------------------------------------------------
instance ToObject Object where
  toObject = pure
-----------------------------------------------------------------------------
instance ToJSVal Value where
  toJSVal = toJSVal_Value
-----------------------------------------------------------------------------
instance FromJSVal () where
  fromJSVal _ = pure (Just ())
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b) => ToJSVal (a,b) where
  toJSVal (x,y) = do
    x_ <- toJSVal x
    y_ <- toJSVal y
    toJSVal_List [ x_, y_ ]
-----------------------------------------------------------------------------
instance FromJSVal MisoString where
  fromJSVal = fromJSVal_JSString
-----------------------------------------------------------------------------
jsg :: MisoString -> IO JSVal
jsg key = global ! key
-----------------------------------------------------------------------------
jsgf :: ToArgs args => MisoString -> args -> IO JSVal
jsgf name = global # name
-----------------------------------------------------------------------------
jsg1 :: ToJSVal arg => MisoString -> arg -> IO JSVal
jsg1 name arg = jsgf name [arg]
-----------------------------------------------------------------------------
setField :: (ToObject o, ToJSVal v) => o -> MisoString -> v -> IO ()
setField o k v = do
  o' <- toJSVal =<< toObject o
  v' <- toJSVal v
  setProp_ffi k v' o'
-----------------------------------------------------------------------------
infixr 1 <##
(<##) :: (ToObject o, ToJSVal v) => o -> Int -> v -> IO ()
(<##) o k v = do
  o' <- toJSVal =<< toObject o
  v' <- toJSVal v
  setPropIndex_ffi k v' o'
-----------------------------------------------------------------------------
(!) :: ToObject o => o -> MisoString -> IO JSVal
(!) = flip getProp
-----------------------------------------------------------------------------
listProps :: Object -> IO [MisoString]
listProps (Object jsval) = do
  keys <- fromJSValUnchecked =<< listProps_ffi jsval
  forM keys fromJSValUnchecked
-----------------------------------------------------------------------------
call :: (ToObject obj, ToObject this, ToArgs args) => obj -> this -> args -> IO JSVal
call o this args = do
  o' <- toJSVal =<< toObject o
  this' <- toJSVal =<< toObject this
  args' <- toJSVal =<< toArgs args
  invokeFunction o' this' args'
-----------------------------------------------------------------------------
infixr 2 #
(#) :: (ToObject object, ToArgs args) => object -> MisoString -> args -> IO JSVal
(#) o k args = do
  o' <- toJSVal =<< toObject o
  func <- getProp_ffi k o'
  args' <- toJSVal =<< toArgs args
  invokeFunction func o' args'
-----------------------------------------------------------------------------
new :: (ToObject constructor, ToArgs args) => constructor -> args -> IO JSVal
new constr args = do
  obj <- toJSVal =<< toObject constr
  argv <- toJSVal =<< toArgs args
  new_ffi obj argv
-----------------------------------------------------------------------------
create :: IO Object
create = Object <$> create_ffi
-----------------------------------------------------------------------------
setProp :: ToJSVal val => MisoString -> val -> Object -> IO ()
setProp k v (Object o) = flip (setProp_ffi k) o =<< toJSVal v
-----------------------------------------------------------------------------
getProp :: ToObject o => MisoString -> o -> IO JSVal
getProp k v = getProp_ffi k =<< toJSVal (toObject v)
-----------------------------------------------------------------------------
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
class ToObject a where
  toObject :: a -> IO Object
-----------------------------------------------------------------------------
instance ToJSVal a => ToObject (IO a) where
  toObject action = Object <$> (toJSVal =<< action)
-----------------------------------------------------------------------------
instance ToArgs MisoString where
  toArgs arg = (:[]) <$> toJSVal arg
----------------------------------------------------------------------------
instance ToJSVal MisoString where
  toJSVal = toJSVal_JSString
----------------------------------------------------------------------------
instance ToJSVal arg => ToArgs [arg] where
    toArgs = mapM toJSVal
----------------------------------------------------------------------------
instance ToArgs () where
  toArgs _ = pure []
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
waitForAnimationFrame :: IO Double
waitForAnimationFrame = waitForAnimationFrame_ffi
----------------------------------------------------------------------------
freeFunction :: Function -> IO ()
freeFunction (Function x) = freeFunction_ffi x
-----------------------------------------------------------------------------
instance FromJSVal Function where
  fromJSVal = pure . Just . Function
-----------------------------------------------------------------------------
(!!) :: ToObject object => object -> Int -> IO JSVal
(!!) o k = do
  o' <- toJSVal =<< toObject o
  getPropIndex_ffi k o'
-----------------------------------------------------------------------------
isUndefined :: ToJSVal val => val -> IO Bool
isUndefined val = isUndefined_ffi <$> toJSVal val
-----------------------------------------------------------------------------
isNull :: ToJSVal val => val -> IO Bool
isNull val = isNull_ffi <$> toJSVal val
-----------------------------------------------------------------------------
newtype Object = Object { unObject :: JSVal } deriving ToJSVal
-----------------------------------------------------------------------------
newtype Function = Function { unFunction :: JSVal } deriving ToJSVal
-----------------------------------------------------------------------------
