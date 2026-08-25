-----------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE MultilineStrings         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI  #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
module Miso.DSL.FFI
  ( -- ** Types
    JSVal
  , JSString (..)
    -- ** Serialization FFI
    -- *** ToJSVal
  , toJSVal_Char
  , toJSVal_Bool
  , toJSVal_Double
  , toJSVal_Float
  , toJSVal_Int
  , toJSVal_List
  , toJSVal_JSString
  , toJSVal_Text
    -- *** FromJSVal
  , fromJSVal_Text
  , fromJSValUnchecked_Text
  , fromJSVal_Char
  , fromJSValUnchecked_Char
  , fromJSVal_Bool
  , fromJSValUnchecked_Bool
  , fromJSVal_Double
  , fromJSValUnchecked_Double
  , fromJSVal_Float
  , fromJSValUnchecked_Float
  , fromJSVal_Int
  , fromJSValUnchecked_Int
  , fromJSVal_List
  , fromJSValUnchecked_List
  , fromJSVal_JSString
  , fromJSVal_Maybe
  , fromJSValUnchecked_Maybe
  -- * Callback FFI
  , await
  , asyncCallback
  , asyncCallback1
  , asyncCallback2
  , asyncCallback3
  , syncCallback
  , syncCallback1
  , syncCallback2
  , syncCallback3
  , syncCallback'
  , syncCallback1'
  , syncCallback2'
  , syncCallback3'
  -- * DSL FFI
  , invokeFunction
  , setProp_ffi
  , new_ffi
  , getProp_ffi
  , eval_ffi
  , setPropIndex_ffi
  , getPropIndex_ffi
  , create_ffi
    -- *** Misc. FFI
  , global
  , isUndefined_ffi
  , isNull_ffi
  , jsNull
  , freeFunction_ffi
  , freeJSVal_ffi
  , requestAnimationFrame
  , cancelAnimationFrame
  , listProps_ffi
  -- *** String FFI
  , parseInt
  , parseDouble
  , parseWord
  , parseFloat
#ifdef MISO_TEXT
  , toString_Int
  , toString_Double
  , toString_Float
  , toString_Word
#endif
  , textFromJSString
  , textToJSString
  , JSException
  ) where
-----------------------------------------------------------------------------
import           Data.Text (Text, pack)
import           Control.Monad
import           Data.JSString (textFromJSString, textToJSString)
#ifdef MISO_TEXT
import qualified Data.JSString as JSS
import qualified Data.Text as T
import qualified Data.Text.Read as TR
#endif
import           Prelude hiding (length, head, tail, unlines, concat, null, drop, replicate, concatMap)
-----------------------------------------------------------------------------
import           GHC.Wasm.Prim
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 === $2;
  """ eq :: JSVal -> JSVal -> Bool
-----------------------------------------------------------------------------
instance Eq JSVal where
  (==) = eq
  {-# INLINE (==) #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  if ($1 === 0.0) return false;
  return true;
  """ toJSVal_Bool :: Bool -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ toJSVal_Double :: Double -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ toJSVal_Int :: Int -> IO JSVal
-----------------------------------------------------------------------------
toJSVal_List :: [JSVal] -> IO JSVal
toJSVal_List js = do
  arr <- newArray
  forM_ js (pushArray arr)
  pure arr
{-# INLINE toJSVal_List #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return [];
  """ newArray :: IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  $1.push($2)
  """ pushArray :: JSVal -> JSVal -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """
  toJSVal_Char :: Char -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """
  toJSVal_Float :: Float -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Float :: JSVal -> IO Float
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Char :: JSVal -> IO Char
-----------------------------------------------------------------------------
fromJSVal_Char :: JSVal -> IO (Maybe Char)
fromJSVal_Char x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Char x
{-# INLINE fromJSVal_Char #-}
-----------------------------------------------------------------------------
toJSVal_JSString :: JSString -> IO JSVal
toJSVal_JSString (JSString jsval) = pure jsval
{-# INLINE toJSVal_JSString #-}
-----------------------------------------------------------------------------
fromJSVal_Text :: JSVal -> IO (Maybe Text)
fromJSVal_Text x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Text x
{-# INLINE fromJSVal_Text #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Text :: JSVal -> IO Text
fromJSValUnchecked_Text t =
  pure $ textFromJSString (JSString t)
{-# INLINE fromJSValUnchecked_Text #-}
-----------------------------------------------------------------------------
toJSVal_Text :: Text -> IO JSVal
toJSVal_Text t =
  case textToJSString t of
    JSString jsval -> pure jsval
{-# INLINE toJSVal_Text #-}
-----------------------------------------------------------------------------
fromJSVal_Float :: JSVal -> IO (Maybe Float)
fromJSVal_Float x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Float x
{-# INLINE fromJSVal_Float #-}
-----------------------------------------------------------------------------
fromJSVal_Bool :: JSVal -> IO (Maybe Bool)
fromJSVal_Bool x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Bool x
{-# INLINE fromJSVal_Bool #-}
-----------------------------------------------------------------------------
fromJSVal_Int :: JSVal -> IO (Maybe Int)
fromJSVal_Int x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Int x
{-# INLINE fromJSVal_Int #-}
-----------------------------------------------------------------------------
fromJSVal_Double :: JSVal -> IO (Maybe Double)
fromJSVal_Double x =
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> fromJSValUnchecked_Double x
{-# INLINE fromJSVal_Double #-}
-----------------------------------------------------------------------------
fromJSVal_List :: JSVal -> IO (Maybe [JSVal])
fromJSVal_List x = do
  if isNullOrUndefined x
    then pure Nothing
    else do
      arrayLike <- isArray x
      if not arrayLike
        then pure Nothing
        else Just <$> fromJSValUnchecked_List x
{-# INLINE fromJSVal_List #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_List :: JSVal -> IO [JSVal]
fromJSValUnchecked_List x = do
   len <- length x
   forM [ 0 .. len - 1 ] (flip getPropIndex_ffi x)
{-# INLINE fromJSValUnchecked_List #-}
-----------------------------------------------------------------------------
fromJSVal_JSString :: JSVal -> IO (Maybe JSString)
fromJSVal_JSString x = do
  if isNullOrUndefined x
    then pure Nothing
    else Just <$> jsstringFromJSVal x
{-# INLINE fromJSVal_JSString #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $1" jsstringFromJSVal :: JSVal -> IO JSString
-----------------------------------------------------------------------------
isNullOrUndefined :: JSVal -> Bool
isNullOrUndefined x = isNull_ffi x || isUndefined_ffi x
{-# INLINE isNullOrUndefined #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 === undefined;
  """ isUndefined_ffi :: JSVal -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1 === null;
  """ isNull_ffi :: JSVal -> Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return null;
  """ jsNull :: JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "return globalThis" global :: JSVal
-----------------------------------------------------------------------------
-- | Awaits a JS Promise. If the promise rejects, it throws a t'JSException'.
--
-- @since 1.13.0.0
foreign import javascript interruptible "return await $1;"
  await :: JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper"
  asyncCallback
    :: IO ()
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper"
  asyncCallback1
    :: (JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper"
  asyncCallback2
    :: (JSVal -> JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper"
  asyncCallback3
    :: (JSVal -> JSVal -> JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback
    :: IO ()
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback1
    :: (JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback2
    :: (JSVal -> JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback3
    :: (JSVal -> JSVal -> JSVal -> IO ())
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback'
    :: IO JSVal
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback1'
    :: (JSVal -> IO JSVal)
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback2'
    :: (JSVal -> JSVal -> IO JSVal)
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript "wrapper sync"
  syncCallback3'
    :: (JSVal -> JSVal -> JSVal -> IO JSVal)
    -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return Object.keys($1);
  """
  listProps_ffi :: JSVal -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.apply($2, $3);
  """
  invokeFunction
    :: JSVal
    -- ^ Func
    -> JSVal
    -- ^ Obj
    -> JSVal
    -- ^ Args
    -> IO JSVal
    -- ^ Return value
-----------------------------------------------------------------------------
foreign import javascript unsafe
  "$3[$1]=$2"
  setPropIndex_ffi
    :: Int
    -- ^ Index
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe
  "$3[$1]=$2"
  setProp_ffi
    :: JSString
    -- ^ Field
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
-----------------------------------------------------------------------------
-- | Regular FFIs
-----------------------------------------------------------------------------
foreign import javascript unsafe
  "return new $1(...$2)"
  new_ffi
    :: JSVal
    -- ^ Constructor
    -> JSVal
    -- ^ Args
    -> IO JSVal
    -- ^ Return
-----------------------------------------------------------------------------
foreign import javascript unsafe "return {}" create_ffi :: IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $2[$1]"
  getProp_ffi
    :: JSString
    -- ^ Key
    -> JSVal
    -- ^ Value
    -> IO JSVal
    -- ^ Return
-----------------------------------------------------------------------------
-- | Unsafe JS eval, use at your own risk! You have been warned
foreign import javascript unsafe
  """
  return eval($1);
  """ eval_ffi :: JSString -> IO JSVal
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Int :: JSVal -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Double :: JSVal -> IO Double
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1
  """ fromJSValUnchecked_Bool :: JSVal -> IO Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe "return $2[$1]"
  getPropIndex_ffi
    :: Int
    -- ^ Key
    -> JSVal
    -- ^ Value
    -> IO JSVal
    -- ^ Return
-----------------------------------------------------------------------------
freeFunction_ffi :: JSVal -> IO ()
freeFunction_ffi = freeJSVal
{-# INLINE freeFunction_ffi #-}
-----------------------------------------------------------------------------
-- | Eagerly release a 'JSVal' handle. See 'Miso.DSL.freeJSVal'.
freeJSVal_ffi :: JSVal -> IO ()
freeJSVal_ffi = freeJSVal
{-# INLINE freeJSVal_ffi #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return requestAnimationFrame($1);
  """ requestAnimationFrame :: JSVal -> IO Int
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return cancelAnimationFrame($1);
  """ cancelAnimationFrame :: Int -> IO ()
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return Array.isArray($1);
  """ isArray :: JSVal -> IO Bool
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  return $1.length
  """ length :: JSVal -> IO Int
-----------------------------------------------------------------------------
fromJSVal_Maybe :: JSVal -> IO (Maybe (Maybe JSVal))
fromJSVal_Maybe jsval = do
  if isNullOrUndefined jsval
    then pure (Just Nothing)
    else pure $ Just (Just jsval)
{-# INLINE fromJSVal_Maybe #-}
-----------------------------------------------------------------------------
fromJSValUnchecked_Maybe :: JSVal -> IO (Maybe JSVal)
fromJSValUnchecked_Maybe jsval = do
  if isNullOrUndefined jsval
    then pure Nothing
    else pure (Just jsval)
{-# INLINE fromJSValUnchecked_Maybe #-}
-----------------------------------------------------------------------------
#ifdef MISO_TEXT
-- | Parses using 'Data.Text.Read' directly (no JS FFI round trip),
-- matching JS's @parseInt@ semantics: leading\/trailing whitespace and
-- trailing garbage are ignored, a leading @+\/-@ is allowed, and a
-- @0x@\/@0X@ prefix is read as hexadecimal.
parseInt :: Text -> Maybe Int
parseInt input =
  case T.stripPrefix (T.pack "0x") stripped `mplus` T.stripPrefix (T.pack "0X") stripped of
    Just hex -> hush (TR.hexadecimal hex)
    Nothing  -> hush (TR.signed TR.decimal stripped)
  where
    stripped = T.strip input
{-# INLINE parseInt #-}
#else
foreign import javascript unsafe
  """
  return parseInt($1);
  """
  parseInt_Unchecked :: JSString -> Double
-----------------------------------------------------------------------------
parseInt :: JSString -> Maybe Int
parseInt string =
  case parseInt_Unchecked string of
    double | isNaN double -> Nothing
           | otherwise -> Just (round double)
{-# INLINE parseInt #-}
#endif
-----------------------------------------------------------------------------
#ifdef MISO_TEXT
parseWord :: Text -> Maybe Word
#else
parseWord :: JSString -> Maybe Word
#endif
parseWord string = fromIntegral <$> parseInt string
{-# INLINE parseWord #-}
-----------------------------------------------------------------------------
#ifdef MISO_TEXT
-- | Parses using 'Data.Text.Read' directly (no JS FFI round trip),
-- matching JS's @parseFloat@ semantics: leading\/trailing whitespace and
-- trailing garbage are ignored, and a leading @+\/-@ is allowed.
parseDouble :: Text -> Maybe Double
parseDouble = hush . TR.double . T.strip
{-# INLINE parseDouble #-}
-----------------------------------------------------------------------------
hush :: Either String (a, Text) -> Maybe a
hush = either (const Nothing) (Just . fst)
{-# INLINE hush #-}
#else
foreign import javascript unsafe
  """
  return parseFloat($1);
  """
  parseDouble_Unchecked :: JSString -> Double
-----------------------------------------------------------------------------
parseDouble :: JSString -> Maybe Double
parseDouble string =
  case parseDouble_Unchecked string of
    double | isNaN double -> Nothing
           | otherwise -> Just double
{-# INLINE parseDouble #-}
#endif
-----------------------------------------------------------------------------
#ifdef MISO_TEXT
parseFloat :: Text -> Maybe Float
#else
parseFloat :: JSString -> Maybe Float
#endif
parseFloat string = realToFrac <$> parseDouble string
{-# INLINE parseFloat #-}
-----------------------------------------------------------------------------
#ifdef MISO_TEXT
-- | 'show' agrees with JS's native number formatting for 'Int', so this
-- avoids allocating a throwaway 'JSVal' via the FFI just to convert it
-- straight back to 'Text'.
toString_Int :: Int -> Text
toString_Int = pack . show
{-# INLINE toString_Int #-}
-----------------------------------------------------------------------------
toString_Double :: Double -> Text
toString_Double = pack . show
{-# INLINE toString_Double #-}
-----------------------------------------------------------------------------
toString_Float :: Float -> Text
toString_Float = pack . show
{-# INLINE toString_Float #-}
-----------------------------------------------------------------------------
-- | See 'toString_Int': 'show' matches JS formatting for 'Word' too.
toString_Word :: Word -> Text
toString_Word = pack . show
{-# INLINE toString_Word #-}
#endif
-----------------------------------------------------------------------------
