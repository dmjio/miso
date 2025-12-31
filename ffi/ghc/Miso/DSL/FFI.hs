-----------------------------------------------------------------------------
module Miso.DSL.FFI where
-----------------------------------------------------------------------------
import           Data.Text (Text)
import           Data.Aeson
-----------------------------------------------------------------------------
data JSVal = JSVal
-----------------------------------------------------------------------------
toJSVal_Bool :: Bool -> IO JSVal
toJSVal_Bool = undefined
-----------------------------------------------------------------------------
toJSVal_Double :: Double -> IO JSVal
toJSVal_Double = undefined
-----------------------------------------------------------------------------
toJSVal_Int :: Int -> IO JSVal
toJSVal_Int = undefined
-----------------------------------------------------------------------------
toJSVal_List :: [JSVal] -> IO JSVal
toJSVal_List = undefined
-----------------------------------------------------------------------------
jsNull :: JSVal
jsNull = JSVal
-----------------------------------------------------------------------------
toJSVal_JSVal :: JSVal -> IO JSVal
toJSVal_JSVal = undefined
-----------------------------------------------------------------------------
toJSVal_Char :: Char -> IO JSVal
toJSVal_Char = undefined
-----------------------------------------------------------------------------
toJSVal_Float :: Float -> IO JSVal
toJSVal_Float = undefined
-----------------------------------------------------------------------------
toJSVal_Text :: Text -> IO JSVal
toJSVal_Text = undefined
-----------------------------------------------------------------------------
toJSVal_Value :: Value -> IO JSVal
toJSVal_Value = undefined
-----------------------------------------------------------------------------
fromJSVal_Text :: JSVal -> IO (Maybe Text)
fromJSVal_Text = undefined
-----------------------------------------------------------------------------
fromJSValUnchecked_Text :: JSVal -> IO Text
fromJSValUnchecked_Text = undefined
-----------------------------------------------------------------------------
fromJSVal_Char :: JSVal -> IO (Maybe Char)
fromJSVal_Char = undefined
-----------------------------------------------------------------------------
fromJSValUnchecked_Char :: JSVal -> IO Char
fromJSValUnchecked_Char = undefined
-----------------------------------------------------------------------------
fromJSVal_Float :: JSVal -> IO (Maybe Float)
fromJSVal_Float = undefined
-----------------------------------------------------------------------------
fromJSValUnchecked_Float :: JSVal -> IO Float
fromJSValUnchecked_Float = undefined
-----------------------------------------------------------------------------
fromJSVal_Bool :: JSVal -> IO (Maybe Bool)
fromJSVal_Bool = undefined
-----------------------------------------------------------------------------
fromJSVal_Value :: JSVal -> IO (Maybe Value)
fromJSVal_Value = undefined
-----------------------------------------------------------------------------
new_ffi :: JSVal -> JSVal -> IO JSVal
new_ffi = undefined
-----------------------------------------------------------------------------
eval_ffi :: Text -> IO JSVal
eval_ffi = undefined
-----------------------------------------------------------------------------
create_ffi :: IO JSVal
create_ffi = undefined
-----------------------------------------------------------------------------
getProp_ffi :: Text -> JSVal -> IO JSVal
getProp_ffi = undefined
-----------------------------------------------------------------------------
setProp_ffi :: Text -> JSVal -> JSVal -> IO ()
setProp_ffi = undefined
-----------------------------------------------------------------------------
setField_ffi :: JSVal -> Text -> JSVal -> IO ()
setField_ffi = undefined
-----------------------------------------------------------------------------
fromJSVal_Int :: JSVal -> IO (Maybe Int)
fromJSVal_Int = undefined
-----------------------------------------------------------------------------
fromJSVal_Double :: JSVal -> IO (Maybe Double)
fromJSVal_Double  = undefined
-----------------------------------------------------------------------------
getPropIndex_ffi :: Int -> JSVal -> IO JSVal
getPropIndex_ffi  = undefined
-----------------------------------------------------------------------------
isNull_ffi :: JSVal -> Bool
isNull_ffi = undefined
-----------------------------------------------------------------------------
isUndefined_ffi :: JSVal -> Bool
isUndefined_ffi = undefined
-----------------------------------------------------------------------------
freeFunction_ffi :: JSVal -> IO ()
freeFunction_ffi = undefined
-----------------------------------------------------------------------------
waitForAnimationFrame_ffi :: IO Double
waitForAnimationFrame_ffi = undefined
-----------------------------------------------------------------------------
toJSVal_JSString :: Text -> IO JSVal
toJSVal_JSString = undefined
-----------------------------------------------------------------------------
fromJSValUnchecked_Maybe :: JSVal -> IO (Maybe JSVal)
fromJSValUnchecked_Maybe = undefined
-----------------------------------------------------------------------------
fromJSVal_Maybe :: JSVal -> IO (Maybe (Maybe JSVal))
fromJSVal_Maybe = undefined
-----------------------------------------------------------------------------
fromJSValUnchecked_Bool :: JSVal -> IO Bool
fromJSValUnchecked_Bool = undefined
-----------------------------------------------------------------------------
invokeFunction :: JSVal -> JSVal -> JSVal -> IO JSVal
invokeFunction = undefined
-----------------------------------------------------------------------------
listProps_ffi :: JSVal -> IO JSVal
listProps_ffi = undefined
-----------------------------------------------------------------------------
setPropIndex_ffi :: Int -> JSVal -> JSVal -> IO ()
setPropIndex_ffi = undefined
-----------------------------------------------------------------------------
global :: JSVal
global = undefined
-----------------------------------------------------------------------------
fromJSVal_List :: JSVal -> IO (Maybe [JSVal])
fromJSVal_List = undefined
-----------------------------------------------------------------------------
fromJSValUnchecked_Int :: JSVal -> IO Int
fromJSValUnchecked_Int = undefined
-----------------------------------------------------------------------------
fromJSValUnchecked_Double :: JSVal -> IO Double
fromJSValUnchecked_Double = undefined
-----------------------------------------------------------------------------
fromJSVal_JSString :: JSVal -> IO (Maybe Text)
fromJSVal_JSString = undefined
-----------------------------------------------------------------------------
asyncCallback :: IO () -> IO JSVal
asyncCallback = undefined
asyncCallback1 :: (JSVal -> IO ()) -> IO JSVal
asyncCallback1 = undefined
asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
asyncCallback2 = undefined
asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
asyncCallback3 = undefined
-----------------------------------------------------------------------------
syncCallback :: IO () -> IO JSVal
syncCallback = undefined
syncCallback1 :: (JSVal -> IO ()) -> IO JSVal
syncCallback1 = undefined
syncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
syncCallback2 = undefined
syncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
syncCallback3 = undefined
-----------------------------------------------------------------------------
syncCallback' :: IO JSVal -> IO JSVal
syncCallback' = undefined
syncCallback1' :: (JSVal -> IO JSVal) -> IO JSVal
syncCallback1' = undefined
syncCallback2' :: (JSVal -> JSVal -> IO JSVal) -> IO JSVal
syncCallback2' = undefined
syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO JSVal
syncCallback3' = undefined
-----------------------------------------------------------------------------
