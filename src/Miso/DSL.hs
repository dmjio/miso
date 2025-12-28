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
  , global
  , (#)
  , setField
  , (<##)
  , (!)
  , listProps
  , call
  , jsg1
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
    Nothing -> toJSVal_null
    Just x -> toJSVal x
-----------------------------------------------------------------------------
instance FromJSVal a => FromJSVal [a] where
  fromJSVal = undefined
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
      Nothing -> error "fromJSVal failure"
      Just y -> pure y
-----------------------------------------------------------------------------
instance FromJSVal Int where
  fromJSVal = fromJSVal_Int
-----------------------------------------------------------------------------
instance FromJSVal Double where
  fromJSVal = fromJSVal_Double
-----------------------------------------------------------------------------
instance ToObject Object where
  toObject = pure
-----------------------------------------------------------------------------
instance ToJSVal Value where
  toJSVal = toJSVal_Value
-----------------------------------------------------------------------------
instance FromJSVal () where
  fromJSVal _ = pure Nothing
-----------------------------------------------------------------------------
instance (ToJSVal a, ToJSVal b) => ToJSVal (a,b) where
  toJSVal (x,y) = do
    x_ <- toJSVal x
    y_ <- toJSVal y
    toJSVal_List [ x_, y_ ]
-----------------------------------------------------------------------------
instance FromJSVal MisoString where
  fromJSVal _ = pure Nothing
  fromJSValUnchecked = undefined
-----------------------------------------------------------------------------
jsg :: MisoString -> IO JSVal
jsg = undefined
-----------------------------------------------------------------------------
setField :: (ToObject o, ToJSVal v) => o -> MisoString -> v -> IO ()
setField = setField_ffi
-----------------------------------------------------------------------------
infixr 1 <##
(<##) :: (ToObject o, ToJSVal v) => o -> Int -> v -> IO ()
(<##) = undefined
-----------------------------------------------------------------------------
(!) :: ToObject o => o -> MisoString -> IO JSVal
(!) = flip getProp
-----------------------------------------------------------------------------
listProps :: Object -> IO [MisoString]
listProps = undefined
-----------------------------------------------------------------------------
call :: (ToObject obj, ToObject this, ToArgs args) => obj -> this -> args -> IO JSVal
call = undefined
-----------------------------------------------------------------------------
jsg1 :: ToJSVal a => MisoString -> a -> IO JSVal
jsg1 = undefined
-----------------------------------------------------------------------------
infixr 2 #
(#) :: (ToObject object, ToArgs args) => object -> MisoString -> args -> IO JSVal 
(#) = undefined
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
-- function :: (JSVal -> JSVal -> [JSVal] -> IO ()) -> IO Function
-- function = function_ffi
-----------------------------------------------------------------------------
-- asyncFunction :: (JSVal -> JSVal -> [JSVal] -> IO ()) -> IO Function
-- asyncFunction = asyncFunction_ffi
-----------------------------------------------------------------------------
eval :: MisoString -> IO JSVal
eval = eval_ffi
-----------------------------------------------------------------------------
instance FromJSVal Bool where
  fromJSVal = fromJSVal_Bool
-----------------------------------------------------------------------------
instance FromJSVal JSVal where
  fromJSVal = pure . Just
-----------------------------------------------------------------------------
instance FromJSVal a => FromJSVal (Maybe a) where
  fromJSVal = undefined
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
  toJSVal _ = undefined -- pure JSVal
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
waitForAnimationFrame :: IO ()
waitForAnimationFrame = undefined
----------------------------------------------------------------------------
freeFunction :: Function -> IO ()
freeFunction (Function x) = freeFunction_ffi x
-----------------------------------------------------------------------------
instance FromJSVal Function where
  fromJSVal = pure . Just . Function
-----------------------------------------------------------------------------
(!!) :: ToObject object => object -> Int -> IO JSVal
(!!) = getPropIndex_ffi
-----------------------------------------------------------------------------
isUndefined :: ToJSVal val => val -> IO Bool
isUndefined = isUndefined_ffi <=< toJSVal
-----------------------------------------------------------------------------
isNull :: ToJSVal val => val -> IO Bool
isNull = isNull_ffi <=< toJSVal
-----------------------------------------------------------------------------
newtype Object = Object { unObject :: JSVal } deriving ToJSVal
-----------------------------------------------------------------------------
newtype Function = Function { unFunction :: JSVal } deriving ToJSVal
-----------------------------------------------------------------------------
