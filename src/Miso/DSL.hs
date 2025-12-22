-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
module Miso.DSL
  ( module Miso.DSL
  , module Miso.DSL.JSVal
  , module Miso.DSL.JSString
  , jsNull
  ) where
-----------------------------------------------------------------------------
import Data.Aeson (Value)
import Control.Monad
-----------------------------------------------------------------------------
import Miso.DSL.JSVal
import Miso.DSL.JSString
import Miso.DSL.FFI
import Miso.String
-----------------------------------------------------------------------------
class ToJSVal a where
  toJSVal :: a -> IO JSVal
-----------------------------------------------------------------------------
instance ToJSVal Bool where
  toJSVal = toJSVal_Bool
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
  fromJSVal = fromJSVal
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
  makeObject = pure
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
new = new_ffi
-----------------------------------------------------------------------------
create :: IO Object
create = create_ffi
-----------------------------------------------------------------------------
setProp :: MisoString -> JSVal -> Object -> IO ()
setProp = setProp_ffi
-----------------------------------------------------------------------------
getProp :: ToObject o => MisoString -> o -> IO JSVal
getProp = getProp_ffi
-----------------------------------------------------------------------------
function :: (JSVal -> JSVal -> [JSVal] -> IO ()) -> IO Function
function = function_ffi
-----------------------------------------------------------------------------
asyncFunction :: (JSVal -> JSVal -> [JSVal] -> IO ()) -> IO Function
asyncFunction = asyncFunction_ffi
-----------------------------------------------------------------------------
global :: IO JSVal
global = jsg ("globalThis" :: MisoString)
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
newtype Object = Object { unObject :: JSVal }
  deriving ToJSVal
-----------------------------------------------------------------------------
class ToArgs args where
  makeArgs :: args -> IO [JSVal]
-----------------------------------------------------------------------------
instance ToArgs Double where
  makeArgs x = (:[]) <$> toJSVal x
-----------------------------------------------------------------------------
instance ToArgs JSVal where
  makeArgs val = pure [val]
-----------------------------------------------------------------------------
instance ToObject JSVal where
  makeObject = pure . Object
-----------------------------------------------------------------------------
class ToObject a where
  makeObject :: a -> IO Object
-----------------------------------------------------------------------------
instance ToJSVal a => ToObject (IO a) where
  makeObject action = Object <$> (toJSVal =<< action)
-----------------------------------------------------------------------------
instance ToArgs MisoString where
  makeArgs arg = (:[]) <$> toJSVal arg
----------------------------------------------------------------------------
instance ToJSVal MisoString where
  toJSVal _ = pure JSVal
----------------------------------------------------------------------------
newtype Function = Function JSVal
  deriving ToJSVal
----------------------------------------------------------------------------
instance ToJSVal arg => ToArgs [arg] where
    makeArgs = mapM toJSVal
----------------------------------------------------------------------------
instance ToArgs () where
  makeArgs _ = pure []
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2) => ToArgs (arg1, arg2) where
  makeArgs (arg1, arg2) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    return [rarg1, rarg2]
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3) => ToArgs (arg1, arg2, arg3) where
  makeArgs (arg1, arg2, arg3) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    return [rarg1, rarg2, rarg3]
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4) => ToArgs (arg1, arg2, arg3, arg4) where
  makeArgs (arg1, arg2, arg3, arg4) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    rarg4 <- toJSVal arg4
    return [rarg1, rarg2, rarg3, rarg4]
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4, ToJSVal arg5) => ToArgs (arg1, arg2, arg3, arg4, arg5) where
  makeArgs (arg1, arg2, arg3, arg4, arg5) = do
    rarg1 <- toJSVal arg1
    rarg2 <- toJSVal arg2
    rarg3 <- toJSVal arg3
    rarg4 <- toJSVal arg4
    rarg5 <- toJSVal arg5
    return [rarg1, rarg2, rarg3, rarg4, rarg5]
----------------------------------------------------------------------------
instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4, ToJSVal arg5, ToJSVal arg6) => ToArgs (arg1, arg2, arg3, arg4, arg5, arg6) where
  makeArgs (arg1, arg2, arg3, arg4, arg5, arg6) = do
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
freeFunction = undefined
-----------------------------------------------------------------------------
instance FromJSVal Function where
  fromJSVal = pure . Just . Function
-----------------------------------------------------------------------------
(!!) :: ToObject object => object -> Int -> IO JSVal
(!!) = getPropIndex_ffi
-----------------------------------------------------------------------------
isUndefined :: JSVal -> IO Bool
isUndefined = toJSVal_isUndefined
-----------------------------------------------------------------------------
isNull :: JSVal -> IO Bool
isNull = toJSVal_isNull
-----------------------------------------------------------------------------

