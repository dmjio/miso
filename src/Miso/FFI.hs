{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes, GHCForeignImportPrim, DeriveDataTypeable, GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.FFI
   ( JSVal
   , ToJSVal (..)
   , FromJSVal (..)
   , IsJSVal (..)
   -- JSObject
   , JSObject (..)
   , getProp
   , setProp
   , newJSObject
   , jsNull
   -- JSArray
   , JSArray(..)
   , fromList
   , newJsArray
   , pushJsArray
   -- JSString
   , JSString
   , parseFloat
   , parseDouble
   , parseInt
   , parseWord
   , toJSString
   , fromJSString
   , textFromJSString
   , textToJSString
   , isEmpty
   -- JSBool
   , jsFalse
   , jsTrue
   -- JSCallbacks
   , JSCallback (..)
   , syncCallback
   , syncCallback'
   , syncCallback1'
   , asyncCallback
   , asyncCallback1
   , asyncCallback2
   , releaseCallback
   , releaseCallbacks
   -- ConsoleLog
   , ConsoleLog(..)
   -- VTree
   , VTree (..)
   -- * waitAnimationFrame
   , waitForAnimationFrame
   -- Conversion
   -- Utilities
   , isNullOrUndefined
   , addEventListener
   , windowAddEventListener
   , windowInnerHeight
   , windowInnerWidth
   , eventPreventDefault
   , eventStopPropagation
   , now
   , stringify
   , parse
   , clearBody
   , objectToJSON
   , set
   , getBody
   , getDoc
   , getElementById
   , diff'
   , integralToJSString
   , realFloatToJSString
   , jsStringToDouble
   , delegateEvent
   , undelegateEvent
   , copyDOMIntoVTree
   , swapCallbacks
   , registerCallback
   , focus
   , blur
   , scrollIntoView
   , alert
   , getComponent
    -- * History
   , getWindowLocationHref
   , go
   , back
   , forward
   , pushState
   , replaceState
   , getHistory
   -- * Event Source
   , EventSource(..)
   , newSSE
   , sseData
   -- * Storage
   , localStorage
   , sessionStorage
   , getItem
   , removeItem
   , setItem
   , storageLength
   , clearStorage
   -- * WebSocket
   , Socket(..)
   , create
   , socketState
   , send
   , close
   , wasClean
   , code
   , reason
   , websocketData
    -- eval
#ifdef WASM   
   , eval
#endif
   ) where

import           Control.Monad (forM_, foldM, (<=<))
import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..), Result(..), Object, fromJSON)
import           Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Foreign hiding (fromBool, new)
import           Foreign.C (peekCStringLen, withCStringLen)
import           GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import           Prelude hiding ((!!))
import           System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import           Text.StringLike (StringLike(..))

#ifndef WASM
import           GHC.Exts (Any, ByteArray#, Int#, Int(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Internal as T
import qualified Data.Text.Array as A
import           Control.DeepSeq (rnf)
#endif

#ifdef WASM
import           GHC.Wasm.Prim (JSVal, freeJSVal)
#endif

#ifdef GHCJS_OLD
import           GHCJS.Types (JSVal)
import qualified Data.HashMap.Strict as HM
import           Data.List (foldl')
import           Unsafe.Coerce
#else
import           Data.Aeson (Key)
import qualified Data.Aeson.Key as K
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
#endif

default (JSString)

foreign import javascript unsafe "window.requestAnimationFrame($1)"
  waitForAnimationFrame :: JSCallback a -> IO ()

foreign import javascript unsafe "[]" newJsArray :: IO JSArray
foreign import javascript unsafe "$1.push($2)" pushJsArray :: JSArray -> JSVal -> IO ()

fromList :: ToJSVal a => [a] -> IO JSArray
fromList xs = do
  jsArray <- newJsArray
  jvals <- mapM toJSVal xs
  forM_ jvals $ \jval ->
    pushJsArray jsArray jval
  pure jsArray

instance (FromJSVal a, FromJSVal b) => FromJSVal (a,b) where
  fromJSVal jval = do
    x <- fromJSVal =<< indexAt 0 (JSArray jval)
    y <- fromJSVal =<< indexAt 1 (JSArray jval)
    pure (x,y)

instance (ToJSVal a, ToJSVal b) => ToJSVal (a,b) where
  toJSVal (x,y) = do
    jvalx <- toJSVal x
    jvaly <- toJSVal y
    JSArray jarray <- fromList [jvalx, jvaly]
    pure jarray

instance ToJSVal a => ToJSVal [a] where
  toJSVal xs = do
    JSArray jval <- fromList =<< mapM toJSVal xs
    pure jval

instance ToJSVal a => ToJSVal (Maybe a) where
  toJSVal Nothing = pure jsNull
  toJSVal (Just x) = toJSVal x

instance ToJSVal JSString where
  toJSVal (JSString x) = pure x

instance {-# OVERLAPS #-} ToJSVal String where
  toJSVal s | JSString x <- toJSString s = pure x

#ifdef WASM
foreign import javascript unsafe "null" jsNull      :: JSVal
foreign import javascript unsafe "false" jsFalse    :: JSVal
foreign import javascript unsafe "true" jsTrue      :: JSVal
#else
foreign import javascript unsafe "(function (x) { return null; })" jsNull      :: JSVal
foreign import javascript unsafe "(function (x) { return false; })" jsFalse    :: JSVal
foreign import javascript unsafe "(function (x) { return true; })" jsTrue      :: JSVal
#endif
foreign import javascript unsafe "$1" toJSValInt    :: Int -> IO JSVal
foreign import javascript unsafe "$1" fromJSValInt  :: JSVal -> IO Int
foreign import javascript unsafe "$1" toJSValDouble :: Double -> IO JSVal
foreign import javascript unsafe "$1" toJSValChar   :: Char -> IO JSVal
foreign import javascript unsafe "$1" fromJSValChar :: JSVal -> IO Char

foreign import javascript unsafe "$1 === 1.0 ? true : false" toJSValBool:: Bool -> IO JSVal
foreign import javascript unsafe "$1" fromJSValBool :: JSVal -> IO Bool
instance ToJSVal Bool where toJSVal = toJSValBool

instance FromJSVal Bool where fromJSVal = fromJSValBool

instance FromJSVal String where
  fromJSVal jval = pure $ fromJSString (JSString jval)

instance FromJSVal Integer where -- TODO: BigInt me
  fromJSVal = undefined

instance ToJSVal Int where toJSVal = toJSValInt
instance ToJSVal Double where toJSVal = toJSValDouble
instance ToJSVal Char where toJSVal = toJSValChar
instance FromJSVal Char where fromJSVal = fromJSValChar

instance ToJSVal JSObject where
  toJSVal (JSObject x) = pure x

instance ToJSVal JSArray where
  toJSVal (JSArray x) = pure x

instance ToJSVal Text where
  toJSVal = toJSVal . textToJSString

instance ToJSVal Value where
  toJSVal Null = pure jsNull
  toJSVal (Bool x) = toJSVal x
  toJSVal (String x) = toJSVal x
  toJSVal (Number float) =
    toJSVal float
  toJSVal (Array vector) = do
    array <- newJsArray
    forM_ vector $ \v -> do
      jval <- toJSVal v
      pushJsArray array jval 
    pure (jsval array)
  toJSVal (Object keymap) = do
    obj <- newJSObject
#ifdef GHCJS_OLD
    forM_ (HM.toList keymap) $ \(k, v) -> do
      jkey <- toJSVal k
      jval <- toJSVal v
      setProp (JSString jkey) obj jval
    pure (jsval obj)
#else
    forM_ (KM.toList keymap) $ \(k, v) -> do
      jkey <- toJSVal k
      jval <- toJSVal v
      setProp (JSString jkey) obj jval
    pure (jsval obj)
#endif

#ifndef GHCJS_OLD
instance ToJSVal Key where
  toJSVal key =
    case textToJSString (K.toText key) of
      JSString j -> pure j
#endif

class ToJSVal a => ConsoleLog a where
  consoleLog :: a -> IO ()
  consoleLog = consoleLogJSVal <=< toJSVal

instance ConsoleLog JSObject
instance ConsoleLog JSArray
instance ConsoleLog JSVal
instance ConsoleLog JSString
instance ConsoleLog (JSCallback a)

class IsJSVal a where
  jsval :: a -> JSVal

instance IsJSVal JSVal where
  jsval = id

instance IsJSVal JSObject where
  jsval (JSObject x) = x

instance IsJSVal JSArray where
  jsval (JSArray x) = x

instance IsJSVal (JSCallback a) where
  jsval (JSCallback x) = x

instance IsJSVal JSString where
  jsval (JSString x) = x

#ifdef WASM
textToJSString :: Text -> JSString
textToJSString s =
  unsafeDupablePerformIO $
    T.withCStringLen s $ \(buf, len) -> js_toJSString buf len

toJSString :: String -> JSString
toJSString s =
  unsafeDupablePerformIO $
    withCStringLen s $ \(buf, len) -> js_toJSString buf len

foreign import javascript unsafe "(new TextDecoder('utf-8', {fatal: true})).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_toJSString :: Ptr a -> Int -> IO JSString

textFromJSString :: JSString -> T.Text
textFromJSString s = unsafeDupablePerformIO $ do
  l <- js_stringLength s
  fp <- mallocPlainForeignPtrBytes $ l * 3
  withForeignPtr fp $ \buf -> do
    l' <- js_encodeInto s buf $ l * 3
    T.peekCStringLen (buf, l')

fromJSString :: JSString -> String
fromJSString s = unsafeDupablePerformIO $ do
  l <- js_stringLength s
  fp <- mallocPlainForeignPtrBytes $ l * 3
  withForeignPtr fp $ \buf -> do
    l' <- js_encodeInto s buf $ l * 3
    peekCStringLen (buf, l')

foreign import javascript unsafe "(new TextEncoder()).encodeInto($1, new Uint8Array(__exports.memory.buffer, $2, $3)).written"
  js_encodeInto :: JSString -> Ptr a -> Int -> IO Int

#else

fromJSString :: JSString -> String
{-# INLINE fromJSString #-}
fromJSString = unsafeCoerce . js_fromJSString . jsval

foreign import javascript unsafe "h$toHsString"
  js_fromJSString :: JSVal -> Any

toJSString :: String -> JSString
{-# INLINE toJSString #-}
toJSString x = JSString $ js_toJSString (unsafeCoerce x)

foreign import javascript unsafe "h$fromHsString"
  js_toJSString :: Any -> JSVal

textToJSString :: T.Text -> JSString
{-# INLINE textToJSString #-}

#ifdef GHCJS_OLD
textToJSString (T.Text (A.Array ba) (I# offset) (I# length)) =
  js_toString ba offset length
#else
textToJSString (T.Text (A.ByteArray ba) (I# offset) (I# length)) =
  js_toString ba offset length
#endif

textFromJSString :: JSString -> T.Text
{-# INLINE  textFromJSString #-}
textFromJSString j =
  case js_fromString j of
    (# _ , 0#     #) -> T.empty
#ifdef GHCJS_OLD
    (# ba, length #) -> T.Text (A.Array ba) 0 (I# length)
#else
    (# ba, length #) -> T.Text (A.ByteArray ba) 0 (I# length)
#endif

lazyTextToJSString :: TL.Text -> JSString
{-# INLINE lazyTextToJSString #-}
lazyTextToJSString t = rnf t `seq` js_lazyTextToString (unsafeCoerce t)

lazyTextFromJSString :: JSString -> TL.Text
{-# INLINE lazyTextFromJSString #-}
lazyTextFromJSString = TL.fromStrict . textFromJSString

-- | returns the empty Text if not a string
textFromJSVal :: JSVal -> T.Text
{-# INLINE textFromJSVal #-}
textFromJSVal j = case js_fromString' j of
    (# _,  0#     #) -> T.empty
#ifdef GHCJS_OLD
    (# ba, length #) -> T.Text (A.Array ba) 0 (I# length)
#else
    (# ba, length #) -> T.Text (A.ByteArray ba) 0 (I# length)
#endif

-- | returns the empty Text if not a string
lazyTextFromJSVal :: JSVal -> TL.Text
{-# INLINE lazyTextFromJSVal #-}
lazyTextFromJSVal = TL.fromStrict . textFromJSVal

foreign import javascript unsafe "h$textToString"
  js_toString :: ByteArray# -> Int# -> Int# -> JSString

foreign import javascript unsafe "h$textFromString"
  js_fromString :: JSString -> (# ByteArray#, Int# #)

foreign import javascript unsafe "h$textFromString"
  js_fromString' :: JSVal -> (# ByteArray#, Int# #)

foreign import javascript unsafe "h$lazyTextToString"
  js_lazyTextToString :: Any -> JSString
#endif

foreign import javascript unsafe "$1.length"
  js_arrayLength :: JSArray -> IO Int

foreign import javascript unsafe "$1.length"
  js_stringLength :: JSString -> IO Int

instance ToJSVal (JSCallback a) where
  toJSVal (JSCallback x) = pure x

instance ToJSVal JSVal where
  toJSVal x = pure x

class ToJSVal a where
  toJSVal :: a -> IO JSVal

instance FromJSVal a => FromJSVal (Maybe a) where
  fromJSVal x = do
    result <- isNullOrUndefined x
    if result
      then pure Nothing
      else Just <$> fromJSVal x

foreign import javascript unsafe "($1 === null || $1 === undefined)"
  isNullOrUndefined :: JSVal -> IO Bool

foreign import javascript unsafe "$1" jsvalDouble :: JSVal -> IO Double

instance FromJSVal Double where
  fromJSVal = jsvalDouble 

instance FromJSVal Text where
  fromJSVal jval = pure $ textFromJSString (JSString jval)

instance ToJSVal Scientific where
  toJSVal = toJSVal @Double . toRealFloat

instance FromJSVal Scientific where
  fromJSVal jval = fromFloatDigits <$> fromJSVal @Double jval

foreign import javascript unsafe "$2[$1]"
  indexAt :: Int -> JSArray -> IO JSVal

instance {-# OVERLAPPABLE #-} FromJSVal a => FromJSVal [a] where
  fromJSVal jval = V.toList <$> fromJSVal jval

instance FromJSVal a => FromJSVal (Vector a) where
  fromJSVal jval = do
    len <- js_arrayLength (JSArray jval)
    mv <- MV.new len
    forM_ [ 0 .. len - 1 ] $ \idx -> do
      element <- fromJSVal =<< indexAt idx (JSArray jval)
      MV.write mv idx element
    V.unsafeFreeze mv

foreign import javascript unsafe "Object.keys($1)"
  js_object_keys :: JSObject -> IO JSArray

#ifdef GHCJS_OLD
instance FromJSVal Object where
  fromJSVal jval = do
    keysArray <- js_object_keys (JSObject jval)
    keysLength <- js_arrayLength keysArray
    foldM (accum keysArray) HM.empty [ 0 .. keysLength - 1 ]
      where
        accum keysArray m index = do
          keyJsval <- getByIndex index keysArray
          valJsval <- getProp (JSString keyJsval) (JSObject jval)
          key <- fromJSVal keyJsval
          val <- fromJSVal valJsval
          pure (HM.insert key val m)
#else
instance FromJSVal Key where
  fromJSVal jval = K.fromText <$> fromJSVal jval

instance FromJSVal (KeyMap Value) where
  fromJSVal jval = do
    keysArray <- js_object_keys (JSObject jval)
    keysLength <- js_arrayLength keysArray
    foldM (accum keysArray) KM.empty [ 0 .. keysLength - 1 ]
      where
        accum keysArray m index = do
          keyJsval <- getByIndex index keysArray
          valJsval <- getProp (JSString keyJsval) (JSObject jval)
          key <- fromJSVal keyJsval
          val <- fromJSVal valJsval
          pure (KM.insert key val m)
#endif

instance FromJSVal Value where
  fromJSVal jval
    | isNull jval   = pure Null
    | isNumber jval = Number <$> fromJSVal jval
    | isString jval = String <$> fromJSVal jval
    | isArray jval  = Array  <$> fromJSVal jval
    | isObject jval = Object <$> fromJSVal jval
    | isBoolean jval = Bool <$> fromJSVal jval
    | otherwise = error "Couldn't fromJSVal into Value"

foreign import javascript unsafe "typeof($1) === 'boolean'" isBoolean :: JSVal -> Bool
foreign import javascript unsafe "typeof($1) === 'number'" isNumber :: JSVal -> Bool
foreign import javascript unsafe "typeof($1) === 'string'" isString :: JSVal -> Bool
foreign import javascript unsafe "typeof($1) === 'object'" isObject :: JSVal -> Bool
foreign import javascript unsafe "Array.isArray($1)" isArray        :: JSVal -> Bool
foreign import javascript unsafe "null === $1" isNull               :: JSVal -> Bool

instance FromJSVal Int where
  fromJSVal = fromJSValInt

instance {-# OVERLAPS #-} FromJSVal JSString where
  fromJSVal jval = pure (JSString jval)

class FromJSVal a where
  fromJSVal :: JSVal -> IO a

newtype JSString     = JSString   { unJSString   :: JSVal }
newtype JSArray      = JSArray    { unJSArray    :: JSVal }
newtype JSObject     = JSObject   { unJSObject   :: JSVal }
newtype JSCallback a = JSCallback { unJSCallback :: JSVal }

-- | For objects
foreign import javascript unsafe "$2[$1]"
  getProp :: JSString -> JSObject -> IO JSVal

foreign import javascript unsafe "$2[$1]"
  getByIndex :: Int -> JSArray -> IO JSVal

foreign import javascript unsafe "{}" newJSObject :: IO JSObject

-- | Creates a synchronous callback function (no return value)
#ifdef WASM
foreign import javascript unsafe "wrapper sync"
  syncCallback :: IO () -> IO (JSCallback (IO ()))

foreign import javascript unsafe "wrapper sync"
  syncCallback' :: IO JSVal -> IO (JSCallback (IO JSVal))

foreign import javascript unsafe "wrapper sync"
  syncCallback1' :: (JSVal -> IO JSVal) -> IO (JSCallback (JSVal -> IO JSVal))

foreign import javascript unsafe "wrapper"
  asyncCallback :: IO () -> IO (JSCallback (IO ()))

foreign import javascript unsafe "wrapper"
  asyncCallback1 :: (JSVal -> IO ()) -> IO (JSCallback (JSVal -> IO ()))

foreign import javascript unsafe "wrapper"
  asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO (JSCallback (JSVal -> JSVal -> IO ()))

releaseCallback :: JSCallback a -> IO ()
releaseCallback = freeJSVal . jsval
#elif GHCJS_OLD || GHCJS_NEW

syncCallback':: IO JSVal -> IO (JSCallback (IO JSVal))
syncCallback' = js_syncCallback' . unsafeCoerce

foreign import javascript unsafe "window['syncCallback_']($1)"
  js_syncCallback' :: Any -> IO (JSCallback (IO JSVal))

syncCallback1' :: (JSVal -> IO JSVal) -> IO (JSCallback (JSVal -> IO JSVal))
syncCallback1' = js_syncCallback1' . unsafeCoerce

foreign import javascript unsafe "window['syncCallback1_']($1)"
   js_syncCallback1' :: Any -> IO (JSCallback a)

asyncCallback :: IO () -> IO (JSCallback (IO ()))
asyncCallback = js_asyncCallback . unsafeCoerce

foreign import javascript unsafe "window['asyncCallback']($1)"
  js_asyncCallback :: Any -> IO (JSCallback (IO a))

asyncCallback1 :: (JSVal -> IO ()) -> IO (JSCallback (JSVal -> IO ()))
asyncCallback1 x = js_asyncCallback1 (unsafeCoerce x)

foreign import javascript unsafe "window['asyncCallback1']($1)"
  js_asyncCallback1 :: Any -> IO (JSCallback a)

asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO (JSCallback (JSVal -> JSVal -> IO ()))
asyncCallback2 = js_asyncCallback2 . unsafeCoerce

foreign import javascript unsafe "window['asyncCallback2']($1)"
  js_asyncCallback2 :: Any -> IO (JSCallback a)

foreign import javascript unsafe "h$release"
  releaseCallback :: JSCallback a -> IO ()
#else
syncCallback1' :: (JSVal -> IO JSVal) -> IO (JSCallback (JSVal -> IO JSVal))
syncCallback1' = undefined

syncCallback' :: IO JSVal -> IO (JSCallback (IO JSVal))
syncCallback' = undefined

asyncCallback :: IO () -> IO (JSCallback (IO ()))
asyncCallback = undefined

asyncCallback1 :: (JSVal -> IO ()) -> IO (JSCallback (JSVal -> IO ()))
asyncCallback1 = undefined

asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO (JSCallback (JSVal -> JSVal -> IO ()))
asyncCallback2 = undefined

releaseCallback = undefined
#endif

-- | Set property on object
set :: ToJSVal v => JSString -> v -> JSObject -> IO ()
-- set (fromJSString -> "class") v obj = undefined
--   classSet <- ((JSS.pack "class") `Prelude.elem`) <$> listProps obj
--   if classSet
--     then do
--       classStr <- fromJSValUnchecked =<< getProp (JSS.pack "class") obj
--       vStr <- fromJSValUnchecked =<< toJSVal v
--       v' <- toJSVal (classStr <> JSS.pack " " <> vStr)
--       setProp (JSS.pack "class") v' obj
--     else do
--       v' <- toJSVal v
--       setProp (JSS.pack "class") v' obj
set k v obj = do
  v' <- toJSVal v
  setProp k obj v' 

foreign import javascript unsafe "$2[$1] = $3"
  setProp
    :: JSString
    -> JSObject
    -> JSVal
    -> IO ()

-- | Register an event listener on given target.
foreign import javascript unsafe "$1.addEventListener($1,$2,$3)"
  addEventListener
    :: JSVal      -- ^ Event target on which we want to register event listener
    -> JSString   -- ^ Type of event to listen to (e.g. "click")
    -> JSCallback a -- ^ async JSCallback which will be called when the event occurs, the event will be passed to it as a parameter.
    -> IO ()

-- | Registers an event listener on window
foreign import javascript unsafe "window.addEventListener($1,$2)"
  windowAddEventListener
      :: JSString
      -- ^ Type of event to listen to (e.g. "click")
      -> JSCallback a
      -- ^ async JSCallback which will be called when the event occurs, the event will be passed to it as a parameter.
      -> IO ()

-- | Stop propagation of events
foreign import javascript unsafe "$1.stopPropagation()" eventStopPropagation :: JSVal -> IO ()

-- | Prevent default event behavior
foreign import javascript unsafe "$1.preventDefault()" eventPreventDefault :: JSVal -> IO ()

-- | Retrieves the height (in pixels) of the browser window viewport including, if rendered, the horizontal scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerHeight>
foreign import javascript unsafe "window.innerHeight" windowInnerHeight :: IO Int

-- | Retrieves the width (in pixels) of the browser window viewport including, if rendered, the vertical scrollbar.
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Window/innerWidth>
foreign import javascript unsafe "window.innerWidth" windowInnerWidth :: IO Int

-- | Retrieve high resolution time stamp
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Performance/now>
foreign import javascript unsafe "performance.now()" now :: IO Double

-- | Outputs a message to the web console
--
-- See <https://developer.mozilla.org/en-US/docs/Web/API/Console/log>
foreign import javascript unsafe "console.log($1)" consoleLogJSVal :: JSVal -> IO ()

-- | Converts a JS object into a JSON string
stringify :: ToJSON json => json -> IO JSString
{-# INLINE stringify #-}
stringify value = jsonStringify =<< toJSVal (toJSON value)

foreign import javascript unsafe "JSON.stringify($1)" jsonStringify :: JSVal -> IO JSString

-- | Parses a JSString
parse :: FromJSON json => JSVal -> IO json
{-# INLINE parse #-}
parse js = do
  value <- fromJSVal =<< jsonParse js
  case fromJSON value of
    Success x -> pure x
    Error y -> error y

foreign import javascript unsafe "JSON.parse($1)"
  jsonParse :: JSVal -> IO JSVal

-- | Clear the document body. This is particularly useful to avoid
-- creating multiple copies of your app when running in GHCJSi.
-- dmj: this is legacy, figure out if we still need it
foreign import javascript unsafe "document.body.innerHtml = ''"
  clearBody :: IO ()

-- | Convert a Javascript Unsafe object to JSON
foreign import javascript unsafe "window['objectToJSON']($1,$2)"
  objectToJSON
     :: JSVal
     -- ^ decodeAt :: [JSString]
     -> JSVal
     -- ^ object with impure references to the DOM
     -> IO JSVal

foreign import javascript unsafe "document.querySelectorAll($1)" querySelectorAll :: JSString -> IO JSArray
foreign import javascript unsafe "$1[0]" getFirstItem :: JSArray -> IO JSVal

-- | Retrieves the component id
getComponent :: JSString -> IO JSVal
getComponent name = do
  nodeList <- querySelectorAll ("[data-component-id='" <> name <> "']")
  getFirstItem nodeList

#ifdef WASM
foreign import javascript unsafe "document.body" getBody :: IO JSVal
foreign import javascript unsafe "document" getDoc :: IO JSVal
#else
foreign import javascript unsafe "(function(x) { return document.body; })" getBody :: IO JSVal
foreign import javascript unsafe "(function(x) { return document; })" getDoc :: IO JSVal
#endif

foreign import javascript unsafe "document.getElementById($1)" getElementById :: JSString -> IO JSVal

-- | Diff two virtual DOMs
foreign import javascript unsafe "window['diff']($1,$2,$3,$4)"
  diff'
    :: JSObject -- ^ current object
    -> JSObject -- ^ new object
    -> JSVal  -- ^ parent node
    -> JSVal -- ^ document
    -> IO ()

-- | Helper function for converting Integral types to Javascript Unsafe strings
integralToJSString :: Integral a => a -> JSString
integralToJSString = toJSString . show . toInteger

-- | Helper function for converting RealFloat types to Javascript Unsafe strings
realFloatToJSString :: RealFloat a => a -> JSString
realFloatToJSString x = (toJSString . show) (realToFrac x :: Double)

-- | Helper function for converting RealFloat types to Javascript Unsafe strings
jsStringToDouble :: JSString -> Double
jsStringToDouble = read . fromJSString

-- | Initialize event delegation from a mount point.
delegateEvent :: JSVal -> JSVal -> IO JSVal -> IO ()
delegateEvent mountPoint events getVTree = do
  delegate mountPoint events =<< syncCallback' getVTree

-- | Virtual DOM implemented as a Javascript Unsafe `Object`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree = VTree { getTree :: JSObject }
  deriving newtype (IsJSVal, ToJSVal)

-- | deinitialize event delegation from a mount point.
undelegateEvent :: JSVal -> JSVal -> IO JSVal -> IO ()
undelegateEvent mountPoint events getVTree =
  undelegate mountPoint events =<< syncCallback' getVTree

-- | Call 'delegateEvent' Javascript Unsafe function
foreign import javascript unsafe "window['delegate']($1,$2,$3)"
  delegate :: JSVal -> JSVal -> JSCallback a -> IO ()

foreign import javascript unsafe "window['undelegate']($1,$2,$3)"
  undelegate :: JSVal -> JSVal -> JSCallback a -> IO ()

-- | Copies DOM pointers into virtual dom
-- entry point into isomorphic javascript
foreign import javascript unsafe "window['copyDOMIntoVTree']($1,$2,$3)"
  copyDOMIntoVTree :: Bool -> JSVal -> JSVal -> IO ()

-- | Pins down the current callbacks for clearing later
swapCallbacks :: IO ()
swapCallbacks = pure ()

-- | Releases callbacks registered by the virtual DOM.
releaseCallbacks :: IO ()
releaseCallbacks = pure ()

-- | Mock for callback registration
registerCallback :: JSVal -> IO ()
registerCallback _ = pure ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).focus()@.
foreign import javascript unsafe "window['callFocus']($1)" focus :: JSString -> IO ()

-- | Fails silently if the element is not found.
--
-- Analogous to @document.getElementById(id).blur()@
foreign import javascript unsafe "window['callBlur']($1)" blur :: JSString -> IO ()

-- | Calls @document.getElementById(id).scrollIntoView()@
scrollIntoView :: JSString -> IO ()
scrollIntoView name = do
  jval <- js_get_element_by_id name
  js_scroll_into_view jval

foreign import javascript unsafe "document.getElementById($1)" js_get_element_by_id :: JSString -> IO JSVal
foreign import javascript unsafe "$1.scrollIntoView()" js_scroll_into_view :: JSVal -> IO ()
foreign import javascript unsafe "alert($1)" alert :: JSString -> IO ()
foreign import javascript unsafe "$1.length === 0"  jsstring_null :: JSString -> Bool
foreign import javascript unsafe "parseInt($1)" jsstring_parseInt :: JSString -> IO Int
foreign import javascript unsafe "isNaN(parseInt($1))" jsstring_parseInt_isNaN :: JSString -> IO Bool
foreign import javascript unsafe "isNaN(parseFloat($1))" jsstring_parseFloat_isNaN :: JSString -> IO Bool
foreign import javascript unsafe "parseInt($1)" jsstring_parseFloat :: JSString -> IO Float
foreign import javascript unsafe "parseFloat($1)" jsstring_parseDouble :: JSString -> IO Double
foreign import javascript unsafe "$1 + $2" jsstring_concat :: JSString -> JSString -> JSString
foreign import javascript unsafe "$1 === $2" jsstring_eq :: JSString -> JSString -> Bool

parseFloat :: JSString -> Either String Float
parseFloat s = unsafePerformIO $ do
  isNan <- jsstring_parseFloat_isNaN s
  if isNan
    then
      pure $ Left ("Failed to parse: " <> fromJSString s)
    else
      Right <$> jsstring_parseFloat s

parseDouble :: JSString -> Either String Double
parseDouble s = unsafePerformIO $ do
  isNan <- jsstring_parseFloat_isNaN s
  if isNan
    then
      pure $ Left ("Failed to parse: " <> fromJSString s)
    else
      Right <$> jsstring_parseDouble s

parseWord :: JSString -> Either String Word
parseWord s = fromIntegral <$> parseInt s

parseInt :: JSString -> Either String Int
parseInt s = unsafePerformIO $ do
  isNan <- jsstring_parseInt_isNaN s
  if isNan
    then
      pure $ Left ("Failed to parse: " <> fromJSString s)
    else
      Right <$> jsstring_parseInt s

foreign import javascript unsafe "$1 > $2" jsstring_gt :: JSString -> JSString -> Bool
foreign import javascript unsafe "$1 >= $2" jsstring_gte :: JSString -> JSString -> Bool
foreign import javascript unsafe "$1 <= $2" jsstring_lte :: JSString -> JSString -> Bool
foreign import javascript unsafe "$1 < $2" jsstring_lt :: JSString -> JSString -> Bool

instance Eq JSString where
  (==) = jsstring_eq

instance Ord JSString where
  (<)  = jsstring_lt
  (>)  = jsstring_gt
  (<=) = jsstring_lte
  (>=) = jsstring_gte

instance IsString JSString where
  fromString = toJSString

instance Semigroup JSString where
  (<>) = jsstring_concat

instance Monoid JSString where
  mempty = emptyString

instance Show JSString where
  show = fromJSString

foreign import javascript unsafe "''" emptyString :: JSString
foreign import javascript unsafe "$1 === ''" isEmpty :: JSString -> Bool

-- dmj: this is slow, make it faster
-- jsffi for head and tail
jsstringUncons :: JSString -> Maybe (Char, JSString)
jsstringUncons s
  | s == empty = Nothing
  | otherwise =
      case fromJSString s of
        x : xs -> Just (x, toJSString xs)
        _ -> error "Empty string"

jsstringCons :: Char -> JSString -> JSString
jsstringCons c s = append (toJSString [c]) s

instance StringLike JSString where
  toString   = fromJSString
  fromChar x = toJSString [x]
  strConcat  = foldl' jsstring_concat empty
  empty      = emptyString
  strNull    = jsstring_null
  append     = jsstring_concat
  uncons     = jsstringUncons
  cons       = jsstringCons
  strMap f   = toJSString . fmap f . fromJSString

-- History
foreign import javascript unsafe "history" getHistory :: IO JSVal
foreign import javascript unsafe "window.location.href" getWindowLocationHref :: IO JSString
foreign import javascript unsafe "history.go($1)" go :: Int -> IO ()
foreign import javascript unsafe "history.back()" back :: IO ()
foreign import javascript unsafe "history.forward()" forward :: IO ()
foreign import javascript unsafe "history.pushState(null,null,$1)" pushState :: JSString -> IO ()
foreign import javascript unsafe "history.replaceState(null,null,$1)" replaceState :: JSString -> IO ()

-- SSE
newtype EventSource = EventSource JSVal
foreign import javascript unsafe "$1.data" sseData :: JSVal -> IO JSVal
foreign import javascript unsafe "new EventSource($1)" newSSE :: JSString -> IO EventSource

-- storage
newtype Storage = Storage JSVal
foreign import javascript unsafe "window.localStorage" localStorage     :: IO Storage
foreign import javascript unsafe "window.sessionStorage" sessionStorage :: IO Storage
foreign import javascript unsafe "$1.getItem($2)" getItem               :: Storage -> JSString -> IO JSVal
foreign import javascript unsafe "$1.removeItem($2)" removeItem         :: Storage -> JSString -> IO ()
foreign import javascript unsafe "$1.setItem($2,$3)" setItem            :: Storage -> JSString -> JSString -> IO ()
foreign import javascript unsafe "$1.length" storageLength              :: Storage -> IO Int
foreign import javascript unsafe "$1.clear()" clearStorage              :: Storage -> IO ()

-- web socket
newtype Socket = Socket JSVal

foreign import javascript unsafe "new WebSocket($1,$2)" create :: JSString -> JSVal -> IO Socket
foreign import javascript unsafe "$1.readyState" socketState   :: Socket -> IO Int
foreign import javascript unsafe "$1.send($2)" send            :: Socket -> JSString -> IO ()
foreign import javascript unsafe "$1.close()" close            :: Socket -> IO ()
foreign import javascript unsafe "$1.wasClean" wasClean        :: JSVal -> IO Bool
foreign import javascript unsafe "$1.code" code                :: JSVal -> IO Int
foreign import javascript unsafe "$1.reason" reason            :: JSVal -> IO JSString
foreign import javascript unsafe "$1.data" websocketData       :: JSVal -> IO JSVal

-- jseval
#ifdef WASM
foreign import javascript unsafe "(($1) => { return eval($1); })" eval :: JSString -> IO JSVal
#endif


