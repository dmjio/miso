-----------------------------------------------------------------------------
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
  , setPropText_ffi
  , populateClass_ffi
  , withTextPtr
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
  , toString_Int
  , toString_Word
  , toString_Float
  , toString_Double
  , JSException
  ) where
-----------------------------------------------------------------------------
import           Data.Text (Text)
import           Control.Monad
import           Data.JSString (textFromJSString, textToJSString)
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Data.Text.Read as TR
import           Data.Word (Word8)
import           Foreign.Ptr (Ptr)
import           Numeric (showFFloat)
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
-----------------------------------------------------------------------------
-- Note [Passing strings by pointer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Every 'JSVal' handle on this backend carries a weak pointer that the GC
-- must evacuate at every collection (see 'Miso.DSL.freeJSVal'), and a
-- @JSString@ /is/ a 'JSVal'. Property keys, style values, text nodes and
-- class names are the bulk of what crosses the FFI while building a vtree,
-- so instead of minting a JavaScript string handle for each one we hand
-- JavaScript a (pointer, length) pair into wasm linear memory and let
-- @TextDecoder@ read the UTF-8 bytes directly. No handle is ever created,
-- so there is nothing for the GC to track. 'Data.Text' is UTF-8 internally,
-- so this is a copy into a pinned buffer and one decode.
-----------------------------------------------------------------------------
-- | Run an action with a pointer to the UTF-8 bytes of a 'Text'.
withTextPtr :: Text -> (Ptr Word8 -> Int -> IO a) -> IO a
withTextPtr t k = TF.useAsPtr t (\p n -> k p (fromIntegral n))
{-# INLINE withTextPtr #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
  "$4[(globalThis.__miso_td || (globalThis.__miso_td = new TextDecoder())).decode(new Uint8Array(__exports.memory.buffer, $1, $2))] = $3"
  setPropPtr_ffi :: Ptr Word8 -> Int -> JSVal -> JSVal -> IO ()
-----------------------------------------------------------------------------
setProp_ffi
    :: Text
    -- ^ Field
    -> JSVal
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
setProp_ffi k v o = withTextPtr k $ \p n -> setPropPtr_ffi p n v o
{-# INLINE setProp_ffi #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
  """
  const td = (globalThis.__miso_td || (globalThis.__miso_td = new TextDecoder()));
  $5[td.decode(new Uint8Array(__exports.memory.buffer, $1, $2))] =
    td.decode(new Uint8Array(__exports.memory.buffer, $3, $4));
  """
  setPropTextPtr_ffi :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> JSVal -> IO ()
-----------------------------------------------------------------------------
-- | Set a string-valued property without allocating a 'JSVal' for either
-- the key or the value. See Note [Passing strings by pointer].
setPropText_ffi
    :: Text
    -- ^ Field
    -> Text
    -- ^ Value
    -> JSVal
    -- ^ Object
    -> IO ()
setPropText_ffi k v o =
  withTextPtr k $ \kp kn ->
    withTextPtr v $ \vp vn ->
      setPropTextPtr_ffi kp kn vp vn o
{-# INLINE setPropText_ffi #-}
-----------------------------------------------------------------------------
foreign import javascript unsafe
  "globalThis.miso.populateClass($3, [(globalThis.__miso_td || (globalThis.__miso_td = new TextDecoder())).decode(new Uint8Array(__exports.memory.buffer, $1, $2))])"
  populateClassPtr_ffi :: Ptr Word8 -> Int -> JSVal -> IO ()
-----------------------------------------------------------------------------
-- | Populate a vnode's class set from space-separated class names, without
-- allocating a 'JSVal' for the string. See Note [Passing strings by pointer].
populateClass_ffi :: JSVal -> Text -> IO ()
populateClass_ffi node classes =
  withTextPtr classes $ \p n -> populateClassPtr_ffi p n node
{-# INLINE populateClass_ffi #-}
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
foreign import javascript unsafe
  "return $3[(globalThis.__miso_td || (globalThis.__miso_td = new TextDecoder())).decode(new Uint8Array(__exports.memory.buffer, $1, $2))]"
  getPropPtr_ffi :: Ptr Word8 -> Int -> JSVal -> IO JSVal
-----------------------------------------------------------------------------
getProp_ffi
    :: Text
    -- ^ Key
    -> JSVal
    -- ^ Value
    -> IO JSVal
    -- ^ Return
getProp_ffi k o = withTextPtr k $ \p n -> getPropPtr_ffi p n o
{-# INLINE getProp_ffi #-}
-----------------------------------------------------------------------------
-- | Unsafe JS eval, use at your own risk! You have been warned
foreign import javascript unsafe
  "return eval((globalThis.__miso_td || (globalThis.__miso_td = new TextDecoder())).decode(new Uint8Array(__exports.memory.buffer, $1, $2)))"
  evalPtr_ffi :: Ptr Word8 -> Int -> IO JSVal
-----------------------------------------------------------------------------
eval_ffi :: Text -> IO JSVal
eval_ffi src = withTextPtr src $ \p n -> evalPtr_ffi p n
{-# INLINE eval_ffi #-}
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
-- | Like JavaScript's @parseInt@: parses a leading (optionally signed)
-- decimal integer and ignores any trailing garbage.
parseInt :: Text -> Maybe Int
parseInt t =
  case TR.signed TR.decimal (T.stripStart t) of
    Right (n, _) -> Just n
    Left _ -> Nothing
{-# INLINE parseInt #-}
-----------------------------------------------------------------------------
parseWord :: Text -> Maybe Word
parseWord t =
  case TR.decimal (T.stripStart t) of
    Right (n, _) -> Just n
    Left _ -> Nothing
{-# INLINE parseWord #-}
-----------------------------------------------------------------------------
-- | Like JavaScript's @parseFloat@: parses a leading number and ignores any
-- trailing garbage.
parseDouble :: Text -> Maybe Double
parseDouble t =
  case TR.signed TR.double (T.stripStart t) of
    Right (d, _) -> Just d
    Left _ -> Nothing
{-# INLINE parseDouble #-}
-----------------------------------------------------------------------------
parseFloat :: Text -> Maybe Float
parseFloat string = realToFrac <$> parseDouble string
{-# INLINE parseFloat #-}
-----------------------------------------------------------------------------
toString_Int :: Int -> Text
toString_Int = T.pack . show
{-# INLINE toString_Int #-}
-----------------------------------------------------------------------------
toString_Word :: Word -> Text
toString_Word = T.pack . show
{-# INLINE toString_Word #-}
-----------------------------------------------------------------------------
toString_Float :: Float -> Text
toString_Float = toString_Double . realToFrac
{-# INLINE toString_Float #-}
-----------------------------------------------------------------------------
-- | Formats like JavaScript's @Number.prototype.toString@ for the common
-- cases: integral values print without a fractional part (@3@, not @3.0@)
-- and non-integral values print in plain positional notation (@0.01@, not
-- @1.0e-2@), which is what CSS and the DOM expect.
toString_Double :: Double -> Text
toString_Double d
  | isNaN d = T.pack "NaN"
  | isInfinite d = T.pack (if d > 0 then "Infinity" else "-Infinity")
  | d == fromIntegral (truncate d :: Int) && abs d < 1e15 = T.pack (show (truncate d :: Int))
  | abs d >= 1e-6 && abs d < 1e21 = T.pack (showFFloat Nothing d "")
  | otherwise = T.pack (show d)
{-# INLINE toString_Double #-}
-----------------------------------------------------------------------------
