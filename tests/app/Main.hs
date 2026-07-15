-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifndef GHCJS_OLD
{-# LANGUAGE QuasiQuotes #-}
#endif
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.Reader
import           Control.Exception (try, evaluate)
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import           Data.Char (toLower)
import           Prelude hiding ((!!))
import           GHC.Generics
import           Control.Monad
import           Data.Either
import           Data.IORef
import           Data.Text (Text)
import           GHC.Natural (Natural)
import qualified Data.Text as T
import           Control.Monad.State
-----------------------------------------------------------------------------
import           Miso
import qualified Miso.JSON as JSON
import           Miso.Random
import           Miso.Router
import qualified Miso.String as S
import qualified Miso.Data.Map as MDM
import qualified Miso.Data.Set as MDS
import qualified Miso.Data.Array as Array
import qualified Miso.Date as D
#ifndef GHCJS_OLD
import           Miso.FFI.QQ (js)
#endif
import           Miso.Lens.Generic
import           Miso.Lens
import           Miso.Test
import           Miso.Html
import           Miso.JSON.Parser (decodePure)
import           Miso.Html.Property
import           Miso.Cookie (Cookie (..), cookieValue, defaultCookie, cookieSet_, cookieGet_, cookieDelete_, cookieDeleteWith_, cookieGetAll_)
import           Miso.Runtime.Internal (ComponentState (..), components, componentIds)
-----------------------------------------------------------------------------
-- | Clears the component state and DOM between each test
clearComponentState :: IO ()
clearComponentState = do
  writeIORef components mempty
  writeIORef componentIds initialComponentId
-----------------------------------------------------------------------------
clearBody :: IO ()
clearBody = void $ eval ("document.body.innerHTML = '';" :: MisoString)
-----------------------------------------------------------------------------
initialComponentId :: ComponentId
initialComponentId = 1
-----------------------------------------------------------------------------
nodeLength :: Test Int
nodeLength = do
  liftIO $
    fromJSValUnchecked =<< eval ("document.body.childNodes.length" :: MisoString)
-----------------------------------------------------------------------------
-- | Reads @n@ elements out of a JS array-like (e.g. a 'Uint8Array' or a
-- 'Uint8Array' view over an 'ArrayBuffer') via index access, for asserting
-- on exact byte content in Fetch tests.
readBytes :: JSVal -> Int -> IO [Int]
readBytes raw n = mapM (\i -> fromJSValUnchecked =<< raw !! i) [0 .. n - 1]
-----------------------------------------------------------------------------
-- | Reads a 'Blob'\'s content out as bytes, by bridging its asynchronous
-- @.arrayBuffer()@ Promise to a blocking call (same MVar idiom the sync
-- Fetch\/Cookie API uses internally to turn callbacks into blocking IO).
blobBytes :: JSVal -> IO [Int]
blobBytes raw = do
  mvar <- newEmptyMVar
  promise <- raw # "arrayBuffer" $ ()
  cb <- asyncCallback1 $ \buf -> do
    view <- new (jsg "Uint8Array") [buf]
    len <- fromJSValUnchecked =<< view ! "length"
    putMVar mvar =<< readBytes view len
  _ <- promise # "then" $ [cb]
  takeMVar mvar
-----------------------------------------------------------------------------
-- | Exact bytes served by the local echo server at @\/bytes\/16?seed=42@.
-- They match the original @https:\/\/httpbin.org\/bytes\/16?seed=42@ response
-- so the test can assert on exact content instead of just size\/length.
seededBytes :: [Int]
seededBytes = [0x39, 0x0c, 0x8c, 0x7d, 0x72, 0x47, 0x34, 0x2c, 0xd8, 0x10, 0x0f, 0x2f, 0x6f, 0x77, 0x0d, 0x65]
-----------------------------------------------------------------------------
-- | The PNG file signature — the first 8 bytes of any valid PNG file.
-- <https://en.wikipedia.org/wiki/PNG#File_header>
pngMagicBytes :: [Int]
pngMagicBytes = [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]
-----------------------------------------------------------------------------
-- | The local echo server occasionally needs a retry if the background
-- process hasn't fully started yet. Retries a Fetch call a couple of
-- times before giving up, to absorb that transient flakiness in tests
-- that expect a successful ('Right') response.
retryFetch :: Int -> IO (Either e a) -> IO (Either e a)
retryFetch n action
  | n <= 1 = action
  | otherwise = do
      result <- action
      case result of
        Right _ -> pure result
        Left _  -> retryFetch (n - 1) action
-----------------------------------------------------------------------------
mountedComponents :: Test Int
mountedComponents = IM.size <$> liftIO (readIORef components)
-----------------------------------------------------------------------------
getComponentById :: ComponentId -> Test (ComponentState p props m a)
getComponentById vcompId = (IM.! vcompId) <$> liftIO (readIORef components)
-----------------------------------------------------------------------------
testComponent :: Component parent props Int Action
testComponent = component (0 :: Int) update_ $ \_ _ -> button_ [ id_ "foo", onClick AddOne ] [ "click me " ]
  where
    update_ = \case
      AddOne -> this += 1
-----------------------------------------------------------------------------
data Action = AddOne
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif
-----------------------------------------------------------------------------
data Route
  = Index
  | Home
  | Widget (Capture "thing" Int) (Path "foo") (Capture "other" MisoString) (QueryParam "bar" Int) (QueryParam "lol" Int)
  deriving stock (Generic, Show, Eq)
  deriving anyclass Router
-----------------------------------------------------------------------------
data Person = Person { name :: MisoString, age :: Int }
  deriving stock Generic
  deriving anyclass (ToJSVal, ToObject)
----------------------------------------------------------------------------
-- Types for generic JSON encoding/decoding tests
----------------------------------------------------------------------------
-- Nullary sum
data Color = Red | Green | Blue
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
-- Single-constructor record (no tag)
data Point = Point { px :: Int, py :: Int }
  deriving stock (Generic, Show, Eq)

instance JSON.ToJSON Point where
  toJSON (Point x y) = JSON.object [("px", JSON.toJSON x), ("py", JSON.toJSON y)]

instance JSON.FromJSON Point where
  parseJSON = JSON.withObject "Point" $ \o ->
    Point <$> o JSON..: "px" <*> o JSON..: "py"
----------------------------------------------------------------------------
-- Single-constructor newtype-like (unwrapped)
data Wrapper = Wrapper Int
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
-- Single-constructor with a list field (exercises parseProd gFieldCount == 1 fix)
data WrapperList = WrapperList [MisoString]
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
-- Multi-constructor sum with positional fields
data Shape
  = Circle Double
  | Rectangle Double Double
  | Dot
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
-- Multi-constructor sum with record fields
data Animal
  = Cat { catName :: MisoString, lives :: Int }
  | Dog { dogName :: MisoString, tricks :: Int }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
-- Record with Maybe field
data Profile = Profile { handle :: MisoString, bio :: Maybe MisoString }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
-- Record with camelCase fields for fieldLabelModifier tests
data CamelRecord = CamelRecord { firstName :: MisoString, lastName :: MisoString }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
-- Sum type with a single-field constructor whose field type is a list
-- (exercises the parseTaggedCon gFieldCount == 1 fix)
data NestedList
  = NestedList [MisoString]
  | EmptyNested
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
-- Zero-field single constructor (exercises parseProd 0-field guard)
data Nullary = Nullary
  deriving stock (Generic, Show, Eq)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)
----------------------------------------------------------------------------
getAge :: Person -> IO Int
getAge = inline "return age;"
----------------------------------------------------------------------------
#ifndef GHCJS_OLD
factorial :: Int -> IO Int
factorial n = [js|
  let x = 1;
  for (i = 1; i <= ${n}; i++) {
    x *= i;
  }
  return x;
|]
-----------------------------------------------------------------------------
square :: Int -> IO Int
square n = [js|
  return ${n} * ${n};
|]
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = withJS $ do
  runTests $ beforeEach clearBody $ afterEach clearComponentState $ do
    describe "Miso.Lens.Generic tests" $ do
      it "Should update field generically" $ do
        let john = Person "john" 30
        (john ^. field @"name") `shouldBe` "john"
        let bob = john & field @"name" .~ "bob"
        (bob ^. field @"name") `shouldBe` "bob"
    describe "Miso.Storage tests" $ do
      it "Should get and set in localStorage" $ do
        (`shouldBe` 0) =<< liftIO localStorageLength
        liftIO (setLocalStorage "key1" "value1")
        (`shouldBe` (Just "value1")) =<< liftIO (getLocalStorage "key1")
        liftIO (setLocalStorage "key2" "value2")
        (`shouldBe` (Just "value2")) =<< liftIO (getLocalStorage "key2")
        (`shouldBe` Nothing) =<< liftIO (getLocalStorage "key3")
        (`shouldBe` 2) =<< liftIO localStorageLength
        liftIO (removeLocalStorage "key2")
        (`shouldBe` Nothing) =<< liftIO (getLocalStorage "key2")
        liftIO clearLocalStorage
        (`shouldBe` 0) =<< liftIO localStorageLength

      it "Should get and set in sessionStorage" $ do
        (`shouldBe` 0) =<< liftIO sessionStorageLength
        liftIO (setSessionStorage "key1" "value1")
        (`shouldBe` (Just "value1")) =<< liftIO (getSessionStorage "key1")
        liftIO (setSessionStorage "key2" "value2")
        (`shouldBe` (Just "value2")) =<< liftIO (getSessionStorage "key2")
        (`shouldBe` Nothing) =<< liftIO (getSessionStorage "key3")
        (`shouldBe` 2) =<< liftIO sessionStorageLength
        liftIO (removeSessionStorage "key2")
        (`shouldBe` Nothing) =<< liftIO (getSessionStorage "key2")
        liftIO clearSessionStorage
        (`shouldBe` 0) =<< liftIO sessionStorageLength

    describe "Miso.Cookie FFI tests" $ do
      it "Should set a cookie" $ do
        result <- liftIO (cookieSet_ (defaultCookie "miso-test-set" "hello"))
        result `shouldBe` Right ()

      it "Should get a cookie that exists" $ do
        Right _ <- liftIO (cookieSet_ (defaultCookie "miso-test-get" "world"))
        Right mc <- liftIO (cookieGet_ "miso-test-get")
        mc `shouldBe` Just "world"

      it "Should return Nothing for a missing cookie" $ do
        result <- liftIO (cookieGet_ "miso-nonexistent-xyzabc123")
        result `shouldBe` (Right Nothing :: Either MisoString (Maybe MisoString))

      it "Should get all cookies" $ do
        Right _ <- liftIO (cookieSet_ (defaultCookie "miso-test-getall" "allvalue"))
        Right cs <- liftIO cookieGetAll_
        cs `shouldSatisfy` (not . null)

      it "Should delete a cookie" $ do
        Right _ <- liftIO (cookieSet_ (defaultCookie "miso-test-delete" "bye"))
        Right _ <- liftIO (cookieDelete_ "miso-test-delete")
        result <- liftIO (cookieGet_ "miso-test-delete")
        result `shouldBe` (Right Nothing :: Either MisoString (Maybe MisoString))

      it "Should delete a cookie with cookieDeleteWith by path" $ do
        let cookie = (defaultCookie "miso-test-deletewith" "pathval") { cookiePath = "/" }
        Right _ <- liftIO (cookieSet_ cookie)
        Right _ <- liftIO (cookieDeleteWith_ cookie)
        result <- liftIO (cookieGet_ "miso-test-deletewith")
        result `shouldBe` (Right Nothing :: Either MisoString (Maybe MisoString))

      it "Should delete a cookie by name without a hardcoded path" $ do
        -- cookieDelete passes the name as a bare string, not { name, path: "/" },
        -- so the browser matches cookies on any path the current page can see.
        let cookie = (defaultCookie "miso-test-delete-nopath" "val") { cookiePath = "/" }
        Right _ <- liftIO (cookieSet_ cookie)
        Right _ <- liftIO (cookieDelete_ "miso-test-delete-nopath")
        result <- liftIO (cookieGet_ "miso-test-delete-nopath")
        result `shouldBe` (Right Nothing :: Either MisoString (Maybe MisoString))

    -- dmj: these hit a local echo server (echo-server.ts, port 8081) started
    -- by the playwright-wasm/playwright-js/playwright-ghcjs Nix scripts.
    -- The echo server mimics the httpbin.org endpoints used below and sets
    -- the appropriate CORS headers so the browser can reach it from the
    -- test page served on port 8080.
    describe "Miso.Fetch FFI tests" $ do
      it "Should getJSON_ successfully" $ do
        result <- liftIO (retryFetch 3 (getJSON_ "http://127.0.0.1:8081/get" []) :: IO (Either (Response JSON.Value) (Response JSON.Value)))
        case result of
          Right Response{..} -> do
            status `shouldBe` Just 200
            case body of
              JSON.Object o -> M.member "url" o `shouldBe` True
              _             -> False `shouldBe` True
          Left _ -> False `shouldBe` True

      it "Should getJSON_ report an HTTP error status" $ do
        result <- liftIO (getJSON_ "http://127.0.0.1:8081/status/404" [] :: IO (Either (Response JSON.Value) (Response JSON.Value)))
        case result of
          Left Response{..} -> status `shouldBe` Just 404
          Right _ -> False `shouldBe` True

      it "Should postJSON_ successfully" $ do
        result <- liftIO (retryFetch 3 (postJSON_ "http://127.0.0.1:8081/post" (Point 3 4) []) :: IO (Either (Response JSON.Value) (Response ())))
        case result of
          Right Response{..} -> status `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should postJSON'_ successfully and decode the echoed body" $ do
        result <- liftIO (retryFetch 3 (postJSON'_ "http://127.0.0.1:8081/post" (Point 3 4) []) :: IO (Either (Response JSON.Value) (Response JSON.Value)))
        case result of
          Right Response{..} -> do
            status `shouldBe` Just 200
            case body of
              JSON.Object o ->
                M.lookup "json" o `shouldBe`
                  Just (JSON.Object (M.fromList [("px", JSON.Number 3), ("py", JSON.Number 4)]))
              _ -> False `shouldBe` True
          Left _ -> False `shouldBe` True

      it "Should putJSON_ successfully" $ do
        result <- liftIO (retryFetch 3 (putJSON_ "http://127.0.0.1:8081/put" (Point 5 6) []) :: IO (Either (Response JSON.Value) (Response ())))
        case result of
          Right Response{..} -> status `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should getText_ successfully" $ do
        result <- liftIO (retryFetch 3 (getText_ "http://127.0.0.1:8081/robots.txt" []) :: IO (Either (Response JSON.Value) (Response MisoString)))
        case result of
          Right Response{..} -> do
            status `shouldBe` Just 200
            body `shouldSatisfy` S.isInfixOf "Disallow"
          Left _ -> False `shouldBe` True

      it "Should postText_ successfully" $ do
        result <- liftIO (retryFetch 3 (postText_ "http://127.0.0.1:8081/post" "hello miso" []) :: IO (Either (Response JSON.Value) (Response ())))
        case result of
          Right Response{..} -> status `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should putText_ successfully" $ do
        result <- liftIO (retryFetch 3 (putText_ "http://127.0.0.1:8081/put" "hello miso" []) :: IO (Either (Response JSON.Value) (Response ())))
        case result of
          Right Response{..} -> status `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      -- dmj: the Blob is fetched once and reused for postBlob_/putBlob_ below
      -- to avoid making extra round-trips to the local echo server.
      it "Should getBlob_, postBlob_, and putBlob_ successfully" $ do
        Right Response{..} <-
          liftIO (retryFetch 3 (getBlob_ "http://127.0.0.1:8081/image/png" []) :: IO (Either (Response JSON.Value) (Response Blob)))
        status `shouldBe` Just 200
        case body of
          Blob raw -> do
            size <- liftIO (fromJSValUnchecked =<< raw ! "size")
            size `shouldSatisfy` (> (0 :: Int))
            magic <- liftIO (take 8 <$> blobBytes raw)
            magic `shouldBe` pngMagicBytes
        postResult <- liftIO (retryFetch 3 (postBlob_ "http://127.0.0.1:8081/post" body []) :: IO (Either (Response JSON.Value) (Response ())))
        case postResult of
          Right Response{ status = postStatus } -> postStatus `shouldBe` Just 200
          Left _ -> False `shouldBe` True
        putResult <- liftIO (retryFetch 3 (putBlob_ "http://127.0.0.1:8081/put" body []) :: IO (Either (Response JSON.Value) (Response ())))
        case putResult of
          Right Response{ status = putStatus } -> putStatus `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should getArrayBuffer_, postArrayBuffer_, and putArrayBuffer_ successfully" $ do
        Right Response{..} <-
          liftIO (retryFetch 3 (getArrayBuffer_ "http://127.0.0.1:8081/bytes/16?seed=42" []) :: IO (Either (Response JSON.Value) (Response ArrayBuffer)))
        status `shouldBe` Just 200
        case body of
          ArrayBuffer raw -> do
            len <- liftIO (fromJSValUnchecked =<< raw ! "byteLength")
            len `shouldBe` (16 :: Int)
            view <- liftIO (new (jsg "Uint8Array") [raw])
            bytes <- liftIO (readBytes view 16)
            bytes `shouldBe` seededBytes
        postResult <- liftIO (retryFetch 3 (postArrayBuffer_ "http://127.0.0.1:8081/post" body []) :: IO (Either (Response JSON.Value) (Response ())))
        case postResult of
          Right Response{ status = postStatus } -> postStatus `shouldBe` Just 200
          Left _ -> False `shouldBe` True
        putResult <- liftIO (retryFetch 3 (putArrayBuffer_ "http://127.0.0.1:8081/put" body []) :: IO (Either (Response JSON.Value) (Response ())))
        case putResult of
          Right Response{ status = putStatus } -> putStatus `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should getUint8Array_, postUint8Array_, and putUint8Array_ successfully" $ do
        Right Response{..} <-
          liftIO (retryFetch 3 (getUint8Array_ "http://127.0.0.1:8081/bytes/16?seed=42" []) :: IO (Either (Response JSON.Value) (Response Uint8Array)))
        status `shouldBe` Just 200
        case body of
          Uint8Array raw -> do
            len <- liftIO (fromJSValUnchecked =<< raw ! "length")
            len `shouldBe` (16 :: Int)
            bytes <- liftIO (readBytes raw 16)
            bytes `shouldBe` seededBytes
        postResult <- liftIO (retryFetch 3 (postUint8Array_ "http://127.0.0.1:8081/post" body []) :: IO (Either (Response JSON.Value) (Response ())))
        case postResult of
          Right Response{ status = postStatus } -> postStatus `shouldBe` Just 200
          Left _ -> False `shouldBe` True
        putResult <- liftIO (retryFetch 3 (putUint8Array_ "http://127.0.0.1:8081/put" body []) :: IO (Either (Response JSON.Value) (Response ())))
        case putResult of
          Right Response{ status = putStatus } -> putStatus `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should getFormData_ report an HTTP error status" $ do
        -- dmj: the local echo server has no GET endpoint that responds with a
        -- real multipart/form-data body, so only the error path is exercised here.
        result <- liftIO (getFormData_ "http://127.0.0.1:8081/status/404" [] :: IO (Either (Response JSON.Value) (Response FormData)))
        case result of
          Left Response{..} -> status `shouldBe` Just 404
          Right _ -> False `shouldBe` True

      it "Should postFormData_ successfully" $ do
        fd <- liftIO $ do
          raw <- new (jsg "FormData") ()
          _ <- raw # "append" $ ("key" :: MisoString, "value" :: MisoString)
          pure (FormData raw)
        result <- liftIO (retryFetch 3 (postFormData_ "http://127.0.0.1:8081/post" fd []) :: IO (Either (Response JSON.Value) (Response ())))
        case result of
          Right Response{..} -> status `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should putFormData_ successfully" $ do
        fd <- liftIO $ do
          raw <- new (jsg "FormData") ()
          _ <- raw # "append" $ ("key" :: MisoString, "value" :: MisoString)
          pure (FormData raw)
        result <- liftIO (retryFetch 3 (putFormData_ "http://127.0.0.1:8081/put" fd []) :: IO (Either (Response JSON.Value) (Response ())))
        case result of
          Right Response{..} -> status `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should postImage_ successfully" $ do
        img <- liftIO (newImage "http://127.0.0.1:8081/image/png")
        result <- liftIO (retryFetch 3 (postImage_ "http://127.0.0.1:8081/post" img []) :: IO (Either (Response JSON.Value) (Response ())))
        case result of
          Right Response{..} -> status `shouldBe` Just 200
          Left _ -> False `shouldBe` True

      it "Should putImage_ successfully" $ do
        img <- liftIO (newImage "http://127.0.0.1:8081/image/png")
        result <- liftIO (retryFetch 3 (putImage_ "http://127.0.0.1:8081/put" img []) :: IO (Either (Response JSON.Value) (Response ())))
        case result of
          Right Response{..} -> status `shouldBe` Just 200
          Left _ -> False `shouldBe` True

    describe "Miso.Data.Array tests" $ do
      it "Should create a new array" $ do
        (`shouldBe` 0) =<< liftIO (Array.size =<< (Array.new :: IO (Array.Array Int)))

      it "Should create a new array from a list" $ do
        (`shouldBe` [1,2,3]) =<< liftIO (Array.toList =<< Array.fromList [1,2,3 :: Int])
        (`shouldBe` []) =<< liftIO (Array.toList =<< Array.fromList ([] :: [Int]))

      it "Should insert a new element into an array" $ do
        array <- liftIO $ Array.fromList [0 :: Int]
        liftIO (Array.insert 0 (1 :: Int) array)
        (`shouldBe` [1 :: Int]) =<<
          liftIO (Array.toList array)

      it "Should push an element onto an array" $ do
        array <- liftIO $ Array.fromList [1,2,3 :: Int]
        liftIO (Array.push (4 :: Int) array)
        (`shouldBe` [1,2,3 :: Int,4]) =<< liftIO (Array.toList array)

      it "Should lookup an element from an array" $ do
        array <- liftIO $ Array.fromList [10 :: Int]
        (`shouldBe` (Just 10)) =<< liftIO (Array.lookup (0 :: Int) array)
        (`shouldBe` Nothing) =<< liftIO (Array.lookup (1 :: Int) array)

      it "Should (!?) an element onto an array" $ do
        array <- liftIO $ Array.fromList [10 :: Int]
        (`shouldBe` 10) =<< liftIO ((0 :: Int) Array.!? array)

      it "Should size on an array" $ do
        array <- liftIO $ Array.fromList [10 :: Int]
        (`shouldBe` 1) =<< liftIO (Array.size array)

      it "Should null on an array" $ do
        x <- liftIO $ Array.fromList [10 :: Int]
        (`shouldBe` False) =<< liftIO (Array.null x)
        (`shouldBe` True) =<< liftIO (Array.null =<< Array.new)

      it "Should member on an array" $ do
        x <- liftIO $ Array.fromList [10 :: Int]
        (`shouldBe` True) =<<
          liftIO (Array.member (10 :: Int) x)

      it "Should singleton into an array" $ do
        x <- liftIO $ Array.singleton (10 :: Int)
        (`shouldBe` 1) =<< liftIO (Array.size x)

      it "Should pop an array" $ do
        arr <- liftIO $ Array.fromList [1,2,3 :: Int]
        (`shouldBe` Just 3) =<< liftIO (Array.pop arr)
        (`shouldBe` 2) =<< liftIO (Array.size arr)

        x :: Array.Array Int <- liftIO Array.new
        (`shouldBe` Nothing) =<< liftIO (Array.pop x)

      it "Should shift an array" $ do
        xs <- liftIO $ Array.fromList [1,2,3 :: Int]
        (`shouldBe` Just 1) =<< liftIO (Array.shift xs)
        (`shouldBe` Just 2) =<< liftIO (Array.shift xs)
        (`shouldBe` Just 3) =<< liftIO (Array.shift xs)
        (`shouldBe` Nothing) =<< liftIO (Array.shift xs)

      it "Should unshift an array" $ do
        xs <- liftIO Array.new
        liftIO $ Array.unshift (1::Int) xs
        liftIO $ Array.unshift (2::Int) xs
        liftIO $ Array.unshift (3::Int) xs
        (`shouldBe` [3,2,1]) =<< liftIO (Array.toList xs)

      it "Should reverse an array" $ do
        xs <- liftIO $ Array.fromList [1,2,3 :: Int]
        liftIO (Array.reverse xs)
        (`shouldBe` [3,2::Int, 1]) =<< liftIO (Array.toList xs)

      it "Should splice an array" $ do
        xs <- liftIO $ Array.fromList ["angel", "clown", "trumpet", "sturgeon" :: MisoString]
        ys <- liftIO (Array.splice 0 2 ["parrot", "anemone", "blue" :: MisoString] xs)
        (`shouldBe` ["parrot", "anemone", "blue", "trumpet", "sturgeon" :: MisoString])
          =<< liftIO (Array.toList xs)
        (`shouldBe` ["angel", "clown" :: MisoString]) =<< liftIO (Array.toList ys)

    describe "Miso.Date tests" $ do
      it "Should set and get UTC components" $ do
        date <- liftIO D.new
        _ <- liftIO $ D.setUTCFullYear 2020 (Just 0) (Just 2) date
        _ <- liftIO $ D.setUTCHours 3 (Just 4) (Just 5) (Just 6) date
        (`shouldBe` 2020) =<< liftIO (D.getUTCFullYear date)
        (`shouldBe` 0) =<< liftIO (D.getUTCMonth date)
        (`shouldBe` 2) =<< liftIO (D.getUTCDate date)
        (`shouldBe` 3) =<< liftIO (D.getUTCHours date)
        (`shouldBe` 4) =<< liftIO (D.getUTCMinutes date)
        (`shouldBe` 5) =<< liftIO (D.getUTCSeconds date)
        (`shouldBe` 6) =<< liftIO (D.getUTCMilliseconds date)

      it "Should set and get time in milliseconds" $ do
        date <- liftIO D.new
        _ <- liftIO $ D.setTime 0 date
        (`shouldBe` 0.0) =<< liftIO (D.getTime date)
        (`shouldBe` 0.0) =<< liftIO (D.valueOf date)

      it "Should produce string conversions" $ do
        date <- liftIO D.new
        dateString <- liftIO (D.toDateString date)
        isoString <- liftIO (D.toISOString date)
        utcString <- liftIO (D.toUTCString date)
        (`shouldSatisfy` (/= "")) dateString
        (`shouldSatisfy` (/= "")) isoString
        (`shouldSatisfy` (/= "")) utcString

    describe "Miso.JSON decodePure tests" $ do
      it "Should not decode \"foo\"" $ do
        decodePure "foo" `shouldSatisfy` isLeft
      it "Should decode an empty string \"\"" $ do
        decodePure "\"\"" `shouldBe` Right (JSON.String "")
      it "Should not decode empty input" $ do
        decodePure "" `shouldSatisfy` isLeft
      it "Should decode null" $ do
        decodePure "null" `shouldBe` Right JSON.Null
      it "Should decode true" $ do
        decodePure "true" `shouldBe` Right (JSON.Bool True)
      it "Should decode false" $ do
        decodePure "false" `shouldBe` Right (JSON.Bool False)
      it "Should decode a positive number" $ do
        decodePure "2" `shouldBe` Right (JSON.Number 2.0)
      it "Should decode a negative number" $ do
        decodePure "-2" `shouldBe` Right (JSON.Number (-2.0))
      it "Should decode a number w/ an exponential" $ do
        decodePure "2.2e4" `shouldBe` Right (JSON.Number 2.2e4)
        decodePure "2.2E4" `shouldBe` Right (JSON.Number 2.2e4)
        decodePure "2.2E+4" `shouldBe` Right (JSON.Number 2.2e+4)
        decodePure "2.2E-4" `shouldBe` Right (JSON.Number 2.2e-4)
        decodePure "2.2e+4" `shouldBe` Right (JSON.Number 2.2e+4)
        decodePure "2.2e-4" `shouldBe` Right (JSON.Number 2.2e-4)
      it "Should decode a string" $ do
        decodePure "\"a foo bar \"" `shouldBe` Right (JSON.String "a foo bar ")
      it "Should decode an empty list" $ do
        decodePure "[]" `shouldBe` Right (JSON.Array [])
      it "Should decode an empty with an empty string" $ do
        decodePure "[\"\"]" `shouldBe` Right (JSON.Array [ JSON.String "" ])
      it "Should decode an empty object" $ do
        decodePure "{}" `shouldBe` Right (JSON.Object mempty)
      it "Should decode a homogenous list" $ do
        decodePure "[1,2,3]" `shouldBe`
          Right (JSON.Array [JSON.Number 1, JSON.Number 2, JSON.Number 3])
      it "Should decode a heterogenous list" $ do
        decodePure "[1,true,null]" `shouldBe`
          Right (JSON.Array [JSON.Number 1, JSON.Bool True, JSON.Null])
        decodePure "{\"a\":true,\"b\":1.1}"
          `shouldBe` Right (JSON.Object (M.fromList [("a",JSON.Bool True),("b",JSON.Number 1.1)]))

    describe "Miso.JSON generic encoding tests" $ do
      -- Nullary sum constructors → bare String (allNullaryToStringTag = True)
      it "encodes nullary sum constructors as bare strings" $ do
        JSON.toJSON Red   `shouldBe` JSON.String "Red"
        JSON.toJSON Green `shouldBe` JSON.String "Green"
        JSON.toJSON Blue  `shouldBe` JSON.String "Blue"
      it "round-trips nullary sum constructors" $ do
        (JSON.fromJSON (JSON.toJSON Red)   :: JSON.Result Color) `shouldBe` JSON.Success Red
        (JSON.fromJSON (JSON.toJSON Green) :: JSON.Result Color) `shouldBe` JSON.Success Green
        (JSON.fromJSON (JSON.toJSON Blue)  :: JSON.Result Color) `shouldBe` JSON.Success Blue
      -- Single-constructor record → flat object, no tag
      it "encodes single-constructor records as flat objects" $ do
        JSON.toJSON (Point 3 4)
          `shouldBe` JSON.object [("px", JSON.Number 3), ("py", JSON.Number 4)]
      it "round-trips single-constructor records" $ do
        (JSON.fromJSON (JSON.toJSON (Point 3 4)) :: JSON.Result Point)
          `shouldBe` JSON.Success (Point 3 4)
      -- Single-constructor positional → unwrapped value
      it "encodes single-constructor positional as unwrapped value" $ do
        JSON.toJSON (Wrapper 42) `shouldBe` JSON.Number 42
      it "round-trips single-constructor positional" $ do
        (JSON.fromJSON (JSON.toJSON (Wrapper 42)) :: JSON.Result Wrapper)
          `shouldBe` JSON.Success (Wrapper 42)
      -- Single-constructor with a list field: the whole array is the field value,
      -- not spread as multiple positional elements (parseProd gFieldCount fix)
      it "encodes single-constructor list field as bare array" $ do
        JSON.toJSON (WrapperList ["x", "y", "z"])
          `shouldBe` JSON.Array [JSON.String "x", JSON.String "y", JSON.String "z"]
      it "round-trips single-constructor list field" $ do
        (JSON.fromJSON (JSON.toJSON (WrapperList ["x", "y", "z"])) :: JSON.Result WrapperList)
          `shouldBe` JSON.Success (WrapperList ["x", "y", "z"])
      it "round-trips single-constructor empty list field" $ do
        (JSON.fromJSON (JSON.toJSON (WrapperList [])) :: JSON.Result WrapperList)
          `shouldBe` JSON.Success (WrapperList [])
      -- Sum positional: 1 field → {"tag":"C","contents":v}
      it "encodes sum single-field constructor with contents" $ do
        JSON.toJSON (Circle 5.0)
          `shouldBe` JSON.object [("tag", JSON.String "Circle"), ("contents", JSON.Number 5.0)]
      -- Sum positional: 2 fields → {"tag":"C","contents":[v1,v2]}
      it "encodes sum multi-field positional constructor with contents array" $ do
        JSON.toJSON (Rectangle 3.0 4.0)
          `shouldBe` JSON.object
            [ ("tag", JSON.String "Rectangle")
            , ("contents", JSON.Array [JSON.Number 3.0, JSON.Number 4.0])
            ]
      -- Sum nullary → {"tag":"C"}
      it "encodes sum nullary constructor as tagged object" $ do
        JSON.toJSON Dot `shouldBe` JSON.object [("tag", JSON.String "Dot")]
      it "round-trips sum positional constructors" $ do
        (JSON.fromJSON (JSON.toJSON (Circle 5.0))       :: JSON.Result Shape) `shouldBe` JSON.Success (Circle 5.0)
        (JSON.fromJSON (JSON.toJSON (Rectangle 3.0 4.0)) :: JSON.Result Shape) `shouldBe` JSON.Success (Rectangle 3.0 4.0)
        (JSON.fromJSON (JSON.toJSON Dot)                :: JSON.Result Shape) `shouldBe` JSON.Success Dot
      -- Sum record → {"tag":"C","field1":v1,...}
      it "encodes sum record constructors with tag and flat fields" $ do
        JSON.toJSON (Cat "Mittens" 9)
          `shouldBe` JSON.object
            [ ("tag",     JSON.String "Cat")
            , ("catName", JSON.String "Mittens")
            , ("lives",   JSON.Number 9)
            ]
        JSON.toJSON (Dog "Rex" 3)
          `shouldBe` JSON.object
            [ ("tag",     JSON.String "Dog")
            , ("dogName", JSON.String "Rex")
            , ("tricks",  JSON.Number 3)
            ]
      it "round-trips sum record constructors" $ do
        (JSON.fromJSON (JSON.toJSON (Cat "Mittens" 9)) :: JSON.Result Animal)
          `shouldBe` JSON.Success (Cat "Mittens" 9)
        (JSON.fromJSON (JSON.toJSON (Dog "Rex" 3)) :: JSON.Result Animal)
          `shouldBe` JSON.Success (Dog "Rex" 3)
      -- Record with Maybe field present
      it "encodes record with present Maybe field" $ do
        JSON.toJSON (Profile "dmjio" (Just "Haskell hacker"))
          `shouldBe` JSON.object
            [ ("handle", JSON.String "dmjio")
            , ("bio",    JSON.String "Haskell hacker")
            ]
      -- Record with Maybe field absent → null
      it "encodes record with absent Maybe field as null" $ do
        JSON.toJSON (Profile "dmjio" Nothing)
          `shouldBe` JSON.object
            [ ("handle", JSON.String "dmjio")
            , ("bio",    JSON.Null)
            ]
      it "round-trips record with Maybe field present" $ do
        (JSON.fromJSON (JSON.toJSON (Profile "dmjio" (Just "Haskell hacker"))) :: JSON.Result Profile)
          `shouldBe` JSON.Success (Profile "dmjio" (Just "Haskell hacker"))
      it "round-trips record with Maybe field absent" $ do
        (JSON.fromJSON (JSON.toJSON (Profile "dmjio" Nothing)) :: JSON.Result Profile)
          `shouldBe` JSON.Success (Profile "dmjio" Nothing)
      -- omitNothingFields
      it "omits Nothing fields when omitNothingFields = True" $ do
        let opts = JSON.defaultOptions { JSON.omitNothingFields = True }
        JSON.genericToJSON opts (Profile "dmjio" Nothing)
          `shouldBe` JSON.object [("handle", JSON.String "dmjio")]
      it "keeps Nothing fields as null when omitNothingFields = False" $ do
        JSON.toJSON (Profile "dmjio" Nothing)
          `shouldBe` JSON.object
            [("handle", JSON.String "dmjio"), ("bio", JSON.Null)]
      -- constructorTagModifier
      it "applies constructorTagModifier to sum tags" $ do
        let opts = JSON.defaultOptions
              { JSON.constructorTagModifier = map toLower
              , JSON.allNullaryToStringTag  = False
              }
        JSON.genericToJSON opts Red   `shouldBe` JSON.object [("tag", JSON.String "red")]
        JSON.genericToJSON opts Green `shouldBe` JSON.object [("tag", JSON.String "green")]
      it "applies constructorTagModifier to nullary string tags" $ do
        let opts = JSON.defaultOptions
              { JSON.constructorTagModifier = map toLower }
        JSON.genericToJSON opts Red   `shouldBe` JSON.String "red"
        JSON.genericToJSON opts Green `shouldBe` JSON.String "green"
      -- Single-field constructor whose field is a list type.
      -- Exercises the parseTaggedCon gFieldCount == 1 fix: the whole
      -- "contents" array must be passed as one value, not spread.
      it "encodes single-field list constructor as {tag, contents:[...]}" $ do
        JSON.toJSON (NestedList ["a", "b", "c"])
          `shouldBe` JSON.object
            [ ("tag",      JSON.String "NestedList")
            , ("contents", JSON.Array [JSON.String "a", JSON.String "b", JSON.String "c"])
            ]
      it "round-trips single-field list constructor" $ do
        (JSON.fromJSON (JSON.toJSON (NestedList ["a", "b", "c"])) :: JSON.Result NestedList)
          `shouldBe` JSON.Success (NestedList ["a", "b", "c"])
      it "round-trips single-field empty-list constructor" $ do
        (JSON.fromJSON (JSON.toJSON (NestedList [])) :: JSON.Result NestedList)
          `shouldBe` JSON.Success (NestedList [])
      it "round-trips nullary sibling of single-field list constructor" $ do
        (JSON.fromJSON (JSON.toJSON EmptyNested) :: JSON.Result NestedList)
          `shouldBe` JSON.Success EmptyNested
#ifndef GHCJS_OLD
      -- #3: omitNothingFields = True decode — missing key decodes as Nothing
      it "decodes missing key as Nothing when omitNothingFields was used" $ do
        let opts = JSON.defaultOptions { JSON.omitNothingFields = True }
            encoded = JSON.genericToJSON opts (Profile "dmjio" Nothing)
        -- encoded = {"handle":"dmjio"} — no "bio" key at all
        (JSON.fromJSON encoded :: JSON.Result Profile)
          `shouldBe` JSON.Success (Profile "dmjio" Nothing)
#endif
      -- #5: fieldLabelModifier round-trip
      it "round-trips record with fieldLabelModifier (camelTo2 '_')" $ do
        let opts = JSON.defaultOptions { JSON.fieldLabelModifier = JSON.camelTo2 '_' }
            val  = JSON.genericToJSON opts (CamelRecord "John" "Doe")
        val `shouldBe` JSON.object
          [ ("first_name", JSON.String "John")
          , ("last_name",  JSON.String "Doe")
          ]
        -- decoding with default opts fails (looks for "firstName")
        (JSON.fromJSON val :: JSON.Result CamelRecord)
          `shouldBe` JSON.Error "Key not found: firstName"
        -- decoding with matching opts succeeds
        JSON.parseEither (JSON.genericParseJSON opts) val
          `shouldBe` Right (CamelRecord "John" "Doe")
      -- #7: full string round-trip via encodePure / decodePure
      it "round-trips Point through encodePure/decodePure" $ do
        let s = JSON.encodePure (Point 7 8)
            result = do
              v <- decodePure s
              case JSON.fromJSON v of
                JSON.Success x -> Right (x :: Point)
                JSON.Error e   -> Left (S.unpack e)
        result `shouldBe` Right (Point 7 8)
      it "round-trips Animal through encodePure/decodePure" $ do
        let s = JSON.encodePure (Cat "Mittens" 9)
            result = do
              v <- decodePure s
              case JSON.fromJSON v of
                JSON.Success x -> Right (x :: Animal)
                JSON.Error e   -> Left (S.unpack e)
        result `shouldBe` Right (Cat "Mittens" 9)
      it "round-trips NestedList through encodePure/decodePure" $ do
        let s = JSON.encodePure (NestedList ["a", "b"])
            result = do
              v <- decodePure s
              case JSON.fromJSON v of
                JSON.Success x -> Right (x :: NestedList)
                JSON.Error e   -> Left (S.unpack e)
        result `shouldBe` Right (NestedList ["a", "b"])
      -- #8: parseProd 0-field constructor only accepts Array []
      it "encodes 0-field constructor as Array []" $ do
        JSON.toJSON Nullary `shouldBe` JSON.Array []
      it "round-trips 0-field constructor" $ do
        (JSON.fromJSON (JSON.toJSON Nullary) :: JSON.Result Nullary)
          `shouldBe` JSON.Success Nullary
      it "rejects non-Array for 0-field constructor" $ do
        (JSON.fromJSON (JSON.String "Nullary") :: JSON.Result Nullary)
          `shouldSatisfy` \case
            JSON.Error _ -> True
            _            -> False
      -- #9: parseProd multi-field non-record rejects non-Array
      it "rejects non-Array for multi-field positional constructor" $ do
        (JSON.fromJSON (JSON.String "oops") :: JSON.Result Shape)
          `shouldSatisfy` \case
            JSON.Error _ -> True
            _            -> False
      -- #10: encodePure escapes special characters in strings (Bug 2 fix)
      it "encodePure escapes double quotes in strings" $ do
        JSON.encodePure (JSON.String "say \"hello\"")
          `shouldBe` "\"say \\\"hello\\\"\""
      it "encodePure escapes backslashes in strings" $ do
        JSON.encodePure (JSON.String "back\\slash")
          `shouldBe` "\"back\\\\slash\""
      it "encodePure escapes special chars in object keys" $ do
        JSON.encodePure (JSON.object [("ke\"y", JSON.Bool True)])
          `shouldBe` "{\"ke\\\"y\":true}"

    -- toJSONList: String (i.e. [Char]) serializes as a JSON string while
    -- other lists serialize as JSON arrays. This replaces the old
    -- OVERLAPPING/OVERLAPPABLE ToJSON String / ToJSON [a] instances.
    describe "Miso.JSON String vs list encoding (toJSONList) tests" $ do
      -- String ([Char]) encodes as a JSON string, not an array of chars
      it "encodes String as a JSON string" $ do
        JSON.toJSON ("hello" :: String) `shouldBe` JSON.String "hello"
      it "encodes a Char as a single-character JSON string" $ do
        JSON.toJSON 'a' `shouldBe` JSON.String "a"
      -- the empty String is "" (a string), not [] (an array): the Char
      -- toJSONList specialization wins over the default Array encoding
      it "encodes empty String as an empty JSON string, not an empty array" $ do
        JSON.toJSON ("" :: String) `shouldBe` JSON.String ""
      -- [a] for non-Char encodes as a JSON array (default toJSONList)
      it "encodes [Int] as a JSON array" $ do
        JSON.toJSON ([1,2,3] :: [Int])
          `shouldBe` JSON.Array [JSON.Number 1, JSON.Number 2, JSON.Number 3]
      it "encodes empty [Int] as an empty JSON array" $ do
        JSON.toJSON ([] :: [Int]) `shouldBe` JSON.Array []
      -- [String] is an array of JSON strings (each element via the Char path)
      it "encodes [String] as an array of JSON strings" $ do
        JSON.toJSON (["foo", "bar"] :: [String])
          `shouldBe` JSON.Array [JSON.String "foo", JSON.String "bar"]
      -- nested lists still nest as arrays
      it "encodes [[Int]] as a JSON array of arrays" $ do
        JSON.toJSON ([[1,2],[3]] :: [[Int]])
          `shouldBe` JSON.Array
            [ JSON.Array [JSON.Number 1, JSON.Number 2]
            , JSON.Array [JSON.Number 3]
            ]
      -- round-trips through toJSON/fromJSON
      it "round-trips String through toJSON/fromJSON" $ do
        (JSON.fromJSON (JSON.toJSON ("hello world" :: String)) :: JSON.Result String)
          `shouldBe` JSON.Success "hello world"
      it "round-trips empty String through toJSON/fromJSON" $ do
        (JSON.fromJSON (JSON.toJSON ("" :: String)) :: JSON.Result String)
          `shouldBe` JSON.Success ""
      it "round-trips [Int] through toJSON/fromJSON" $ do
        (JSON.fromJSON (JSON.toJSON ([1,2,3] :: [Int])) :: JSON.Result [Int])
          `shouldBe` JSON.Success [1,2,3]
      it "round-trips [String] through toJSON/fromJSON" $ do
        (JSON.fromJSON (JSON.toJSON (["foo","bar"] :: [String])) :: JSON.Result [String])
          `shouldBe` JSON.Success ["foo","bar"]
      it "round-trips [[Int]] through toJSON/fromJSON" $ do
        (JSON.fromJSON (JSON.toJSON ([[1,2],[3]] :: [[Int]])) :: JSON.Result [[Int]])
          `shouldBe` JSON.Success [[1,2],[3]]
      -- full string round-trips via encodePure / decodePure
      it "round-trips String through encodePure/decodePure" $ do
        let s = JSON.encodePure ("a \"quoted\" string" :: String)
            result = do
              v <- decodePure s
              case JSON.fromJSON v of
                JSON.Success x -> Right (x :: String)
                JSON.Error e   -> Left (S.unpack e)
        result `shouldBe` Right "a \"quoted\" string"
      it "round-trips [Int] through encodePure/decodePure" $ do
        let s = JSON.encodePure ([1,2,3] :: [Int])
            result = do
              v <- decodePure s
              case JSON.fromJSON v of
                JSON.Success x -> Right (x :: [Int])
                JSON.Error e   -> Left (S.unpack e)
        result `shouldBe` Right [1,2,3]

    describe "Miso.Data.Map tests" $ do
      it "should construct a Map from a list and perform operations" $ do
        m <- liftIO (MDM.fromList [(1 :: Int, "foo" :: MisoString), (2 :: Int, "bar" :: MisoString)])
        (`shouldBe` 2)            =<< liftIO (MDM.size m)
        (`shouldBe` (Just "foo")) =<< liftIO (MDM.lookup 1 m)
        (`shouldBe` (Just "bar")) =<< liftIO (MDM.lookup 2 m)
        (`shouldBe` Nothing)      =<< liftIO (MDM.lookup 3 m)
        (`shouldBe` True)         =<< liftIO (MDM.has 1 m)
        (`shouldBe` True)         =<< liftIO (MDM.has 2 m)
        (`shouldBe` False)        =<< liftIO (MDM.has 3 m)
        (`shouldBe` True)         =<< liftIO (MDM.delete 1 m)
        (`shouldBe` False)        =<< liftIO (MDM.has 1 m)
        (`shouldBe` ())           =<< liftIO (MDM.clear m)
        (`shouldBe` 0)            =<< liftIO (MDM.size m)

    describe "Miso.Data.Set tests" $ do
      it "should construct a Set from a list and perform operations" $ do
        m <- liftIO (MDS.fromList [1 :: Int .. 10])
        (`shouldBe` 10)    =<< liftIO (MDS.size m)
        (`shouldBe` 10)    =<< liftIO (MDS.size m)
        (`shouldBe` True)  =<< liftIO (MDS.member 1 m)
        (`shouldBe` False) =<< liftIO (MDS.member 11 m)
        (`shouldBe` True)  =<< liftIO (MDS.delete 1 m)
        (`shouldBe` 9)     =<< liftIO (MDS.size m)
        (`shouldBe` ())    =<< liftIO (MDS.clear m)
        (`shouldBe` 0)     =<< liftIO (MDS.size m)
        x <- liftIO (MDS.fromList [1 :: Int .. 10])
        y <- liftIO (MDS.fromList [11 :: Int .. 20])
        z <- liftIO (MDS.union x y)
        (`shouldBe` 20) =<< liftIO (MDS.size z)
        w <- liftIO (MDS.intersection x y)
        (`shouldBe` 0) =<< liftIO (MDS.size w)
        k <- liftIO (MDS.difference x y)
        (`shouldBe` 20) =<< liftIO (MDS.size k)
        (`shouldBe` True) =<< liftIO (MDS.isSubset k k)
        (`shouldBe` False) =<< liftIO (MDS.isSubset x y)
        (`shouldBe` True) =<< liftIO (MDS.isSuperset k k)
        (`shouldBe` False) =<< liftIO (MDS.isSuperset x y)
        (`shouldBe` False) =<< liftIO (MDS.isDisjoint k k)
        (`shouldBe` True) =<< liftIO (MDS.isDisjoint x y)

#ifndef GHCJS_OLD
    describe "inline JS QQ tests" $ do
      it "should use inline JS to calc factorial" $
        (`shouldBe` 120) =<< liftIO (factorial 5)
      it "should use inline JS to square a number" $
        (`shouldBe` 25) =<< liftIO (square 5)
#endif
    describe "Miso.Random tests" $ do
      it "Should generate some random numbers" $ do
        let xs = flip evalState (mkStdGen 0) $ replicateM 10 (state next)
        length xs `shouldBe` 10

      it "Should pick a random seed every time" $ do
        stdgen <- liftIO newStdGen
        let xs = flip evalState stdgen $ replicateM 10 (state next)
        stdgen' <- liftIO newStdGen
        let ys = flip evalState stdgen' $ replicateM 10 (state next)
        xs `shouldNotBe` ys

      it "Should generate the same random numbers using the same seed" $ do
        let xs = flip evalState (mkStdGen 0) $ replicateM 10 (state next)
        let ys = flip evalState (mkStdGen 0) $ replicateM 10 (state next)
        xs `shouldBe` ys

      it "Should generate different random numbers using different seeds" $ do
        let xs = flip evalState (mkStdGen 1) $ replicateM 10 (state next)
        let ys = flip evalState (mkStdGen 2) $ replicateM 10 (state next)
        xs `shouldNotBe` ys

    describe "Miso.Lens tests" $ do
      it "Should convert between VL and Miso.Lens.Lens" $ do
        (fromVL (toVL this) %~ (+1)) 0 `shouldBe` 1
      it "Should ^." $ do
        (0 ^. this) `shouldBe` 0
      it "Should at on IntMap" $ do
        execState (Miso.Lens.at 0 ?= 10) (mempty :: IntMap Int) `shouldBe`
          IM.singleton 0 10
        execState (Miso.Lens.at 0 .= Nothing) (IM.singleton 0 10 :: IntMap Int) `shouldBe`
          mempty
      it "Should at on Map" $ do
        execState (Miso.Lens.at 'a' ?= 10) (mempty :: Map Char Int) `shouldBe`
          M.singleton 'a' 10
        execState (Miso.Lens.at 'a' .= Nothing) (M.singleton 'a' 10 :: Map Char Int) `shouldBe`
          mempty
      it "Should _1" $ do
        ((1,2) ^. _1) `shouldBe` 1
      it "Should compose" $ do
        ((1,(2, 3)) ^. (_1 `compose` _2)) `shouldBe` 2
      it "Should _2" $ do
        ((1,2) ^. _2) `shouldBe` 2
      it "Should .~" $ do
        (this .~ 0) 1 `shouldBe` 0
      it "Should set" $ do
        (Miso.Lens.set this 0) 1 `shouldBe` 0
      it "Should %~" $ do
        (this %~ (+1)) 0 `shouldBe` 1
      it "Should over" $ do
        (over this (+1)) 0 `shouldBe` 1
      it "Should +~" $ do
        (this +~ 1) 0 `shouldBe` 1
      it "Should *~" $ do
        (this *~ 2) 2 `shouldBe` 4
      it "Should -~" $ do
        (this -~ 1) 0 `shouldBe` (-1)
      it "Should //~" $ do
        (this //~ 2) 4 `shouldBe` 2.0
      it "Should %=" $ do
        (execState (this %= (+1)) 0) `shouldBe` 1
      it "Should modifying" $ do
        (execState (modifying this (+1)) 0) `shouldBe` 1
      it "Should %?=" $ do
        (execState (this %?= (+1)) Nothing) `shouldBe` Nothing
      it "Should +=" $ do
        (execState (this += 1) 0) `shouldBe` 1
      it "Should -=" $ do
        (execState (this -= 1) 0) `shouldBe` (-1)
      it "Should %?=" $ do
        (execState (this %?= (+1)) (Just 0)) `shouldBe` (Just 1)
      it "Should use" $ do
        (execState (use this) 0) `shouldBe` 0
      it "Should view" $ do
        (runReader (Miso.Lens.view this) 0) `shouldBe` 0
      it "Should .=" $ do
        (execState (this .= 1) 0) `shouldBe` 1
      it "Should assign" $ do
        (execState (assign this 1) 0) `shouldBe` 1
      it "Should *=" $ do
        (execState (this *= 2) 2) `shouldBe` 4
      it "Should <~" $ do
        (execState (this <~ pure 10) 0) `shouldBe` 10
      -- here
      it "Should <%=" $ do
        (evalState (this <%= (+1)) 0) `shouldBe` 1
      it "Should <.=" $ do
        (evalState (this <.= 1) 0) `shouldBe` 1
      it "Should <?=" $ do
        (evalState (this <?= 10) Nothing) `shouldBe` 10
      it "Should <<.=" $ do
        (runState (this <<.= 10) 0) `shouldBe` (0,10)
      it "Should <<%=" $ do
        (runState (this <<%= (+1)) 10) `shouldBe` (10,11)
      it "Should <>~" $ do
        (this <>~ ['b']) ['a'] `shouldBe` ['a','b']
      it "Should ?~" $ do
        (this ?~ 0) (Just 1) `shouldBe` (Just 0)
      it "Should At [a]" $ do
        (['a','b','c'] ^. Miso.Lens.at 1) `shouldBe` Just 'b'
        (['a','b','c'] & Miso.Lens.at 1 .~ (Just 'd')) `shouldBe` ['a','d','c']
        (['a','b','c'] & Miso.Lens.at 1 .~ Nothing) `shouldBe` ['a','c']
      it "Should At [a] edge cases" $ do
          ([0..10::Int] ^. Miso.Lens.at 100) `shouldBe` Nothing
          ([0..10::Int] ^. Miso.Lens.at (-100)) `shouldBe` Nothing
          ([0..10::Int] & Miso.Lens.at 100 .~ Just 5) `shouldBe` [0..10]
          ([0..10::Int] & Miso.Lens.at (-100) .~ Just 5) `shouldBe` [0..10]
          ([0..10::Int] & Miso.Lens.at 100 .~ Nothing) `shouldBe` [0..10]
          ([0..10::Int] & Miso.Lens.at (-100) .~ Nothing) `shouldBe` [0..10]
    describe "Inline JS tests" $ do
     it "Should use inline js" $ do
       (`shouldBe` 42) =<< liftIO (getAge (Person "larry" 42))
    describe "Router tests" $ do
      it "should call fromRoute on Index" $ do
        fromRoute Index `shouldBe` [ IndexToken ]
      it "should call fromRoute on Home" $ do
        fromRoute Home `shouldBe` [ CaptureOrPathToken "home" ]
      it "should call fromRoute on Widget" $ do
        fromRoute (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe` [ CaptureOrPathToken "widget"
                     , CaptureOrPathToken "10"
                     , CaptureOrPathToken "foo"
                     , CaptureOrPathToken "other"
                     , QueryParamToken "bar" (Just "12")
                     , QueryParamToken "lol" (Just "11")
                     ]
      it "should call toURI on Index" $ do
        toURI Index `shouldBe` URI "" "" mempty
      it "should call toURI on Home" $ do
        toURI Home `shouldBe` URI "home" "" mempty
      it "should call toURI on Widget" $ do
        toURI (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe`
             URI { uriPath = "widget/10/foo/other"
                 , uriFragment = ""
                 , uriQueryString = M.fromList [("bar", Just "12"), ("lol", Just "11")]
                 }
      it "should call fromMisoString on Int" $ do
        S.fromMisoStringEither "10" `shouldBe` Right (10 :: Int)
      it "should call href on Index" $ do
        Miso.Router.href_ Index `shouldBe` Property "href" "/"
      it "should call href on Home" $ do
        Miso.Router.href_ Home `shouldBe` Property "href" "/home"
      it "should call href on Widget" $ do
        Miso.Router.href_ (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe` Property "href" "/widget/10/foo/other?bar=12&lol=11"
      it "should call prettyRoute on Index" $ do
        prettyRoute Index `shouldBe` "/"
      it "should call prettyRoute on Home" $ do
        prettyRoute Home `shouldBe` "/home"
      it "should call prettyRoute on Widget" $ do
        prettyRoute (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe` "/widget/10/foo/other?bar=12&lol=11"
      it "should call dumpURI on Index" $ do
        dumpURI Index `shouldBe` "URI {uriPath = \"\", uriFragment = \"\", uriQueryString = fromList []}"
      it "should call dumpURI on Home" $ do
        dumpURI Home `shouldBe` "URI {uriPath = \"home\", uriFragment = \"\", uriQueryString = fromList []}"
      it "should call dumpURI on Widget" $ do
        dumpURI (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))
          `shouldBe` "URI {uriPath = \"widget/10/foo/other\", uriFragment = \"\", uriQueryString = fromList [(\"bar\",Just \"12\"),(\"lol\",Just \"11\")]}"
      it "should call toRoute Index" $ do
        toRoute "/" `shouldBe` Right Index
      it "should call toRoute Home" $ do
        toRoute "/home" `shouldBe` Right Home

      -- dmj: fails w/ ghcjs86 ... Generics-related it seems
      -- it "should call toRoute Widget" $ do
      --   toRoute "/widget/10/foo/other?bar=12&lol=11"
      --     `shouldBe`
      --       Right (Widget (Capture 10) (Path "foo") (Capture "other") (QueryParam (Just 12)) (QueryParam (Just 11)))

      it "should lexTokens on query params/flags" $ do
        lexTokens "/foo?bar=12" `shouldBe`
          Right [CaptureOrPathToken "foo", QueryParamToken "bar" (Just "12")]
        lexTokens "/foo?bar" `shouldBe`
          Right [CaptureOrPathToken "foo", QueryParamToken "bar" Nothing]

      it "should lexTokens on fragments" $ do
        lexTokens "/foo?bar#cool" `shouldBe`
          Right [CaptureOrPathToken "foo", QueryParamToken "bar" Nothing, FragmentToken "cool"]

    describe "MisoString tests" $ do
      it "Should pack" $ do
        S.unpack (S.pack "foo") `shouldBe`
          T.unpack (T.pack "foo")
      it "Should unpack" $ do
        S.unpack "foo" `shouldBe` T.unpack "foo"
      it "Should make singleton" $ do
        S.unpack (S.singleton 'c') `shouldBe`
          T.unpack (T.singleton 'c')
      it "Should be empty" $ do
        S.unpack S.empty `shouldBe`
          T.unpack T.empty
      it "Should cons" $ do
        S.unpack (S.cons 'c' "foo") `shouldBe`
          T.unpack (T.cons 'c' "foo")
      it "Should snoc" $ do
        S.unpack (S.snoc "foo" 'l') `shouldBe`
          T.unpack (T.snoc "foo" 'l')
      it "Should append" $ do
        S.unpack (S.append "foo" "l") `shouldBe`
          T.unpack (T.append "foo" "l")
      it "Should uncons" $ do
        S.uncons "foo" `shouldBe`
          Just ('f', "oo")
        S.uncons "" `shouldBe`
          Nothing
      it "Should unsnoc" $ do
        S.unsnoc "foo" `shouldBe`
          Just ("fo", 'o')
        S.unsnoc "" `shouldBe`
          Nothing
      it "Should head" $ do
        S.head "foo" `shouldBe`
          'f'
      it "Should last" $ do
        S.last "foo" `shouldBe`
          'o'
      it "Should tail" $ do
        S.tail "foo" `shouldBe`
          "oo"
      it "Should init" $ do
        S.init "foo" `shouldBe`
          "fo"
      it "Should null" $ do
        S.null "" `shouldBe`
          True
        S.null "foo" `shouldBe`
          False
      it "Should length" $ do
        S.length "" `shouldBe` 0
        S.length "foo" `shouldBe` 3

      it "Should compareLength" $ do
        S.compareLength "" (-1) `shouldBe`
          T.compareLength "" (-1)
        S.compareLength "foo" 0 `shouldBe`
          T.compareLength "foo" 0
        S.compareLength "foo" 2 `shouldBe`
          T.compareLength "foo" 2
        S.compareLength "foo" 4 `shouldBe`
          T.compareLength "foo" 4

      it "Should map" $ do
        S.unpack (S.map succ "") `shouldBe`
          T.unpack (T.map succ "")
        S.unpack (S.map succ "aslkjdf9823@#SDF@#") `shouldBe`
          T.unpack (T.map succ "aslkjdf9823@#SDF@#")

      it "Should intercalate" $ do
        S.unpack (S.intercalate "," []) `shouldBe`
          T.unpack (T.intercalate "," [])
        S.unpack (S.intercalate "," ["aslkjdf9823@#SDF@#", "foo"]) `shouldBe`
          T.unpack (T.intercalate "," ["aslkjdf9823@#SDF@#","foo"])

      it "Should intersperse" $ do
        S.unpack (S.intersperse ',' "") `shouldBe`
          T.unpack (T.intersperse ',' "")
        S.unpack (S.intersperse ',' "aslkjdf9823@#SDF@#") `shouldBe`
          T.unpack (T.intersperse ',' "aslkjdf9823@#SDF@#")

      it "Should transpose" $ do
        (S.unpack <$> S.transpose ["a","b","c","d","e","fghi","klmn"])
          `shouldBe`
            (T.unpack <$> T.transpose ["a","b","c","d","e","fghi","klmn"])

      it "Should reverse" $ do
        S.unpack (S.reverse "") `shouldBe`
          T.unpack (T.reverse "")
        S.unpack (S.reverse "reverse") `shouldBe`
          T.unpack (T.reverse "reverse")

      it "Should replace" $ do
        S.unpack (S.replace "s" "k" "str") `shouldBe`
          T.unpack (T.replace "s" "k" "str")

      it "Should case fold" $ do
        S.unpack (S.toCaseFold "foobar") `shouldBe`
          T.unpack (T.toCaseFold "foobar")

      it "Should toLower" $ do
        S.unpack (S.toLower "LOL") `shouldBe`
          T.unpack (T.toLower "LOL")

      it "Should toUpper" $ do
        S.unpack (S.toUpper "lol") `shouldBe`
          T.unpack (T.toUpper "lol")

      it "Should toTitle" $ do
        S.unpack (S.toUpper "lol") `shouldBe`
          T.unpack (T.toUpper "lol")

      it "Should justifyLeft" $ do
        S.unpack (S.justifyLeft 2 'a' "bbb") `shouldBe`
          T.unpack (T.justifyLeft 2 'a' "bbb")

      it "Should justifyRight" $ do
        S.unpack (S.justifyRight 2 'a' "bbb") `shouldBe`
          T.unpack (T.justifyRight 2 'a' "bbb")

      it "Should center" $ do
        S.unpack (S.center 2 'a' "bbb") `shouldBe`
          T.unpack (T.center 2 'a' "bbb")

      it "Should foldl" $ do
        S.unpack (S.foldl S.snoc "x" "abc") `shouldBe`
          T.unpack (T.foldl T.snoc "x" "abc")

      it "Should foldl'" $ do
        S.unpack (S.foldl' S.snoc "x" "abc") `shouldBe`
          T.unpack (T.foldl' T.snoc "x" "abc")

      it "Should foldl1" $ do
        S.foldl1 const "abc" `shouldBe`
          T.foldl1 const "abc"

        S.foldl1 (\_ y -> y) "abc" `shouldBe`
          T.foldl1 (\_ y -> y) "abc"

      it "Should foldr" $ do
        S.unpack (S.foldr S.cons "x" "abc") `shouldBe`
          T.unpack (T.foldr T.cons "x" "abc")

      it "Should foldr1" $ do
        S.foldr1 const "abc" `shouldBe`
          T.foldr1 const "abc"

        S.foldr1 (\_ y -> y) "abc" `shouldBe`
          T.foldr1 (\_ y -> y) "abc"

      it "Should concat" $ do
        S.unpack (S.concat ["foo","bar"])
          `shouldBe`
             T.unpack (T.concat ["foo","bar"])

      it "Should concatMap" $ do
        S.unpack (S.concatMap S.singleton "okedoke")
         `shouldBe`
            T.unpack (T.concatMap T.singleton "okedoke")

      it "Should all" $ do
        S.all (=='a') "aaa" `shouldBe` True
        T.all (=='a') "aaa" `shouldBe` True

      it "Should any" $ do
        S.any (=='a') "aaa" `shouldBe` T.any (=='a') "aaa"

      it "Should maximum" $ do
        S.maximum "abc" `shouldBe` 'c'
        T.maximum "abc" `shouldBe` 'c'

      it "Should minimum" $ do
        S.minimum "abc" `shouldBe` 'a'
        T.minimum "abc" `shouldBe` 'a'

      it "Should scanl" $ do
        S.unpack (S.scanl (flip const) 'a' "foo") `shouldBe`
          T.unpack (T.scanl (flip const) 'a' "foo")

        S.unpack (S.scanl const 'a' "foo") `shouldBe`
          T.unpack (T.scanl const 'a' "foo")

      it "Should scanl1" $ do
        S.unpack (S.scanl1 (flip const) "foo") `shouldBe`
          T.unpack (T.scanl1 (flip const) "foo")

        S.unpack (S.scanl1 const "foo") `shouldBe`
          T.unpack (T.scanl1 const "foo")

      it "Should scanr" $ do
        S.unpack (S.scanr (flip const) 'a' "foo") `shouldBe`
          T.unpack (T.scanr (flip const) 'a' "foo")

        S.unpack (S.scanr const 'a' "foo") `shouldBe`
          T.unpack (T.scanr const 'a' "foo")

      it "Should scanr1" $ do
        S.unpack (S.scanr1 (flip const) "foo") `shouldBe`
          T.unpack (T.scanr1 (flip const) "foo")

        S.unpack (S.scanr1 const "foo") `shouldBe`
          T.unpack (T.scanr1 const "foo")

      it "Should mapAccumL" $ do
        (S.unpack <$> (S.mapAccumL (,) 'a' "foo")) `shouldBe`
          (T.unpack <$> (T.mapAccumL (,) 'a' "foo"))

      it "Should mapAccumR" $ do
        (S.unpack <$> (S.mapAccumR (,) 'a' "foo")) `shouldBe`
          (T.unpack <$> (T.mapAccumR (,) 'a' "foo"))

        (S.unpack <$> (S.mapAccumR (,) 'a' "")) `shouldBe`
          (T.unpack <$> (T.mapAccumR (,) 'a' ""))

      it "Should replicate" $ do
        S.unpack (S.replicate 10 "a") `shouldBe`
          T.unpack (T.replicate 10 "a")

      it "Should unfoldr" $ do
        S.unpack (S.unfoldr (\x -> if x < 10 then Just ('a', x+1) else Nothing) 0) `shouldBe`
          T.unpack (T.unfoldr (\x -> if x < 10 then Just ('a', x+1) else Nothing) 0)

      it "Should unfoldrN" $ do
        S.unpack (S.unfoldrN 5 (\x -> if x < 10 then Just ('a', x+1) else Nothing) 0)
          `shouldBe` T.unpack (T.unfoldrN 5 (\x -> if x < 10 then Just ('a', x+1) else Nothing) 0)

      it "Should take" $ do
        S.unpack (S.take 1 "foo") `shouldBe`
          T.unpack (T.take 1 "foo")

      it "Should takeEnd" $ do
        S.unpack (S.takeEnd 1 "foo") `shouldBe`
          T.unpack (T.takeEnd 1 "foo")

      it "Should drop" $ do
        S.unpack (S.drop 1 "foo") `shouldBe`
          T.unpack (T.drop 1 "foo")

      it "Should dropEnd" $ do
        S.unpack (S.dropEnd 1 "foo") `shouldBe`
          T.unpack (T.dropEnd 1 "foo")

      it "Should takeWhile" $ do
        S.unpack (S.takeWhile (=='f') "foo") `shouldBe`
          T.unpack (T.takeWhile (=='f') "foo")

      it "Should takeWhileEnd" $ do
        S.unpack (S.takeWhileEnd (=='o') "foo") `shouldBe`
          T.unpack (T.takeWhileEnd (=='o') "foo")

      it "Should dropWhile" $ do
        S.unpack (S.dropWhile (=='f') "foo") `shouldBe`
          T.unpack (T.dropWhile (=='f') "foo")

      it "Should dropWhileEnd" $ do
        S.unpack (S.dropWhileEnd (=='o') "foo") `shouldBe`
          T.unpack (T.dropWhileEnd (=='o') "foo")

      it "Should dropAround" $ do
        S.unpack (S.dropAround (=='o') "foo") `shouldBe`
          T.unpack (T.dropAround (=='o') "foo")

      it "Should strip" $ do
        S.unpack (S.strip "  foo  ") `shouldBe`
          T.unpack (T.strip "  foo ")

      it "Should stripStart" $ do
        S.unpack (S.strip "  foo") `shouldBe`
          T.unpack (T.strip "foo")

      it "Should stripEnd" $ do
        S.unpack (S.strip "foo") `shouldBe`
          T.unpack (T.strip "foo  ")

      it "Should splitAt" $ do
        let (x,y) = S.splitAt 3 "foobar"
        let (x',y') = T.splitAt 3 "foobar"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should breakOn" $ do
        let (x,y) = S.breakOn "b" "ababa"
        let (x',y') = T.breakOn "b" "ababa"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should breakOnEnd" $ do
        let (x,y) = S.breakOnEnd "b" "ababa"
        let (x',y') = T.breakOnEnd "b" "ababa"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should break" $ do
        let (x,y) = S.break (=='b') "ababa"
        let (x',y') = T.break (=='b') "ababa"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should span" $ do
        let (x,y) = S.break (=='b') "ababa"
        let (x',y') = T.break (=='b') "ababa"
        S.unpack x `shouldBe` T.unpack x'
        S.unpack y `shouldBe` T.unpack y'

      it "Should group" $ do
        fmap S.unpack (S.group "aabbcc") `shouldBe`
          fmap T.unpack (T.group "aabbcc")

      it "Should groupBy" $ do
        fmap S.unpack (S.groupBy (==) "aabbcc") `shouldBe`
          fmap T.unpack (T.groupBy (==) "aabbcc")

      it "Should inits" $ do
        fmap S.unpack (S.inits "foo bar") `shouldBe`
          fmap T.unpack (T.inits "foo bar")

      it "Should tails" $ do
        fmap S.unpack (S.tails "foo bar") `shouldBe`
          fmap T.unpack (T.tails "foo bar")

      it "Should splitOn" $ do
        fmap S.unpack (S.splitOn "a" "ababa") `shouldBe`
          fmap T.unpack (T.splitOn "a" "ababa")

      it "Should split" $ do
        fmap S.unpack (S.split (=='a') "aabbaca") `shouldBe`
          fmap T.unpack (T.split (=='a') "aabbaca")

      it "Should chunksOf" $ do
        fmap S.unpack (S.chunksOf 3 "foofoo") `shouldBe`
          fmap T.unpack (T.chunksOf 3 "foofoo")

      it "Should lines" $ do
        fmap S.unpack (S.lines "foo\nbar") `shouldBe`
          fmap T.unpack (T.lines "foo\nbar")

      it "Should unlines" $ do
        S.unpack (S.unlines ["foo", "bar"]) `shouldBe`
          T.unpack (T.unlines ["foo", "bar"])

      it "Should words" $ do
        fmap S.unpack (S.words "foo bar") `shouldBe`
          fmap T.unpack (T.words "foo bar")

      it "Should unwords" $ do
        S.unpack (S.unwords ["foo", "bar"]) `shouldBe`
          T.unpack (T.unwords ["foo", "bar"])

      it "Should isPrefixOf" $ do
        S.isPrefixOf "f" "foo" `shouldBe`
          T.isPrefixOf "f" "foo"

      it "Should isSuffixOf" $ do
        S.isSuffixOf "o" "foo" `shouldBe`
          T.isSuffixOf "o" "foo"

      it "Should isInfixOf" $ do
        S.isInfixOf "o" "foo" `shouldBe`
          T.isInfixOf "o" "foo"

      it "Should stripPrefix" $ do
        fmap S.unpack (S.stripPrefix "f" "foo") `shouldBe`
          fmap T.unpack (T.stripPrefix "f" "foo")

      it "Should stripSuffix" $ do
        fmap S.unpack (S.stripSuffix "o" "foo") `shouldBe`
          fmap T.unpack (T.stripSuffix "o" "foo")

      it "Should filter" $ do
        S.unpack (S.filter (=='a') "aba") `shouldBe`
          T.unpack (T.filter (=='a') "aba")

      it "Should find" $ do
        S.find (=='a') "aba" `shouldBe`
          T.find (=='a') "aba"

      it "Should index" $ do
        S.index "aba" 0 `shouldBe`
          T.index "aba" 0

      it "Should findIndex" $ do
        S.findIndex (=='a') "aba" `shouldBe`
          T.findIndex (=='a') "aba"

      it "Should count" $ do
        S.count "a" "aba" `shouldBe`
          T.count "a" "aba"

      it "Should partition" $ do
        let (l,r) = S.partition (=='o') "foobar"
        let (l',r') = T.partition (=='o') "foobar"
        S.unpack l `shouldBe` T.unpack l'
        S.unpack r `shouldBe` T.unpack r'

      it "Should zip" $ do
        S.zip "aaa" "bbb" `shouldBe`
          T.zip "aaa" "bbb"

      it "Should zipWith" $ do
        S.unpack (S.zipWith const "aaa" "bbb") `shouldBe`
          T.unpack (T.zipWith const "aaa" "bbb")

        S.unpack (S.zipWith (\_ y -> y)  "aaa" "bbb") `shouldBe`
          T.unpack (T.zipWith (\_ y -> y) "aaa" "bbb")

      it "Should perform Real / Integral conversions on MisoString" $ do
        S.fromMisoStringEither "3.14" `shouldBe` Right (3.14 :: Double)
        S.fromMisoStringEither @Double "foo" `shouldSatisfy` isLeft
        S.fromMisoStringEither "3.14" `shouldBe` Right (3.14 :: Float)
        S.fromMisoStringEither @Float "foo" `shouldSatisfy` isLeft
        S.fromMisoStringEither "3" `shouldBe` Right (3 :: Int)
        S.fromMisoStringEither @Int "foo" `shouldSatisfy` isLeft
        S.fromMisoStringEither "3" `shouldBe` Right (3 :: Word)
        S.fromMisoStringEither @Word "foo" `shouldSatisfy` isLeft

    describe "JS DSL tests" $ do
      it "Should get an set a property on an Object" $ do
        c <- liftIO create
        liftIO $ flip (setProp "foo") c =<< toJSVal True
        (`shouldBe` True) =<< liftIO (fromJSValUnchecked =<< getProp "foo" c)
      it "Should call eval" $ do
        (`shouldBe` (4 :: Int)) =<< liftIO (fromJSValUnchecked =<< eval "2+2")
    describe "Marshal tests" $ do
      it "Should marshal a Double to JSString" $ do
        toMisoString (3.14 :: Double) `shouldBe` "3.14"
      it "Should marshal a Float to JSString" $ do
        toMisoString (3.14 :: Float) `shouldBe` "3.14"
      it "Should marshal a Word to JSString" $ do
        toMisoString (3 :: Word) `shouldBe` "3"
      it "Should marshal a Int to JSString" $ do
        toMisoString ((-5) :: Int) `shouldBe` "-5"
      it "Should marshal a Value(Object)" $ do
        (`shouldBe` Just (JSON.object [ "foo" JSON..= True ])) =<< liftIO (fromJSVal =<< toJSVal (JSON.object [ "foo" JSON..= True ]))
      it "Should marshal a Value(Array)" $ do
        (`shouldBe` Just (JSON.Array [ JSON.Number 1.0, JSON.Number 2.0 ])) =<<
          liftIO (fromJSVal =<< toJSVal (JSON.Array [ JSON.Number 1.0, JSON.Number 2.0 ]))
      it "Should marshal a Value(Number)" $ do
        (`shouldBe` Just (JSON.Number pi)) =<< liftIO (fromJSVal =<< toJSVal (JSON.Number pi))
      it "Should marshal a Value(Bool(False))" $ do
        (`shouldBe` Just (JSON.Bool False)) =<< liftIO (fromJSVal =<< toJSVal (JSON.Bool False))
      it "Should marshal a Value(Bool(True))" $ do
        (`shouldBe` Just (JSON.Bool True)) =<< liftIO (fromJSVal =<< toJSVal (JSON.Bool True))
      it "Should marshal a Value(String)" $ do
        (`shouldBe` Just (JSON.String "foo")) =<< liftIO (fromJSVal =<< toJSVal (JSON.String "foo"))
      it "Should marshal a Value(Null)" $ do
        (`shouldBe` Just JSON.Null) =<< liftIO (fromJSVal =<< toJSVal JSON.Null)
      it "Should marshal a Bool(True)" $ do
        (`shouldBe` Just True) =<< liftIO (fromJSVal =<< toJSVal True)
      it "Should marshal a ()" $ do
        (`shouldBe` Just ()) =<< liftIO (fromJSVal =<< toJSVal ())
      it "Should marshal a Bool(False)" $ do
        (`shouldBe` Just False) =<< liftIO (fromJSVal =<< toJSVal False)
      it "Should marshal a Float" $ do
        (`shouldBe` Just (pi :: Float)) =<< liftIO (fromJSVal =<< toJSVal (pi :: Float))
        (`shouldBe` Just (-99.99 :: Float)) =<< liftIO (fromJSVal =<< toJSVal (-99.99 :: Float))
        (`shouldBe` Just (-0 :: Float)) =<< liftIO (fromJSVal =<< toJSVal (-0 :: Float))
      it "Should marshal a Double" $ do
        (`shouldBe` Just (pi :: Double)) =<< liftIO (fromJSVal =<< toJSVal (pi :: Double))
        (`shouldBe` Just (-99.99 :: Double)) =<< liftIO (fromJSVal =<< toJSVal (-99.99 :: Double))
        (`shouldBe` Just (-0 :: Double)) =<< liftIO (fromJSVal =<< toJSVal (-0 :: Double))
      it "Should marshal a Int" $ do
        (`shouldBe` Just (99 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (99 :: Int))
        (`shouldBe` Just (-99 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (-99 :: Int))
        (`shouldBe` Just (0 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (0 :: Int))
      it "Should marshal a Natural" $ do
        JSON.fromJSON (JSON.toJSON (99 :: Natural)) `shouldBe` (JSON.Success (99 :: Natural))
        JSON.fromJSON (JSON.toJSON (0 :: Natural)) `shouldBe` (JSON.Success (0 :: Natural))
        (JSON.fromJSON (JSON.Number $ -99.00) :: JSON.Result Natural) `shouldBe` JSON.Error "Cannot parse negative number as Natural: -99"
        ((JSON.fromJSON (JSON.Number $ 0/0)) :: JSON.Result Natural) `shouldBe` JSON.Error "Cannot parse NaN as Natural: NaN"
        (JSON.fromJSON (JSON.Number 15.24) :: JSON.Result Natural) `shouldBe` JSON.Success 15
      it "Should marshal a MisoString" $ do
        (`shouldBe` Just ("foo" :: MisoString)) =<< liftIO (fromJSVal =<< toJSVal ("foo" :: MisoString))
      it "Should marshal a (Maybe Bool)" $ do
        -- dmj: js backend bug
        -- (`shouldBe` (Nothing :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Nothing :: Maybe Bool))
        (`shouldBe` (Just True :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Just True :: Maybe Bool))
        (`shouldBe` (Just False :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Just False :: Maybe Bool))
      it "Should marshal a (Bool,Double)" $ do
        (`shouldBe` Just (True, pi :: Double)) =<< liftIO (fromJSVal =<< toJSVal (True, pi :: Double))
      it "Should marshal a [Double]" $ do
        (`shouldBe` Just [pi,pi :: Double]) =<< liftIO (fromJSVal =<< toJSVal [pi,pi :: Double])
        (`shouldBe` Just ([] :: [Bool])) =<< liftIO (fromJSVal =<< toJSVal ([] :: [Bool]))
      it "Should marshal a Char" $ do
        (`shouldBe` Just ('o' :: Char)) =<< liftIO (fromJSVal =<< toJSVal ('o' :: Char))
      it "Should marshal a String" $ do
        (`shouldBe` Just ("foo" :: String)) =<< liftIO (fromJSVal =<< toJSVal ("foo" :: String))
      it "Should marshal a Text" $ do
        (`shouldBe` Just ("foo" :: Text)) =<< liftIO (fromJSVal =<< toJSVal ("foo" :: Text))

    describe "DOM tests" $ do
      it "Should have access to document.body" $ do
        nodeLength >>= (`shouldBe` (0 :: Int))
      it "Should append a single node to document.body" $ do
          _ <- liftIO $ eval ("document.body.appendChild (document.createElement('div'));" :: MisoString)
          nodeLength >>= (`shouldBe` (1 :: Int))

    describe "Component tests" $ do
      it "Should mount one component" $ do
        liftIO (startApp mempty testComponent)
        mountedComponents >>= (`shouldBe` 1)

      it "Should have parent field present on VDOM nodes" $ do
        _ <- liftIO (startApp mempty testComponent)
        ComponentState {..} <- liftIO $ (IM.! 1) <$> readIORef components
        VTree (Object ref) <- liftIO (readIORef _componentVTree)
        parentDomRef <- liftIO (ref ! "domRef")
        childParentField <- liftIO (ref ! "children" !! 0 ! "parent")
        childParentFieldDOMRef <- liftIO (childParentField ! "domRef")
        parentFieldNull <- liftIO (isNull childParentField)
        parentFieldNull `shouldBe` False
        parentFieldUndefined <- liftIO (isUndefined childParentField)
        parentFieldUndefined `shouldBe` False

      it "Should mount 1000 components" $ do
        liftIO $ startApp mempty $
          component (0 :: Int) noop $ \_ _ ->
            div_ [] (replicate 999 (mount_ testComponent))
        mountedComponents >>= (`shouldBe` 1000)

    describe "Miso.DSL `await` tests" $ do
      it "Successful Promise resolution should result in a value" $ do
        -- Create a Promise and immediately resolve it with `42`
        val <- liftIO $ do
          promise <- jsg ("Promise" :: MisoString)
          p <- promise # ("resolve" :: MisoString) $ (42 :: Int)
          result <- await p
          fromJSValUnchecked result

        val `shouldBe` (42 :: Int)

      it "A rejected Promise will throw an error we can catch" $ do
        result <- liftIO $ do
          promise <- jsg ("Promise" :: MisoString)
          p <- promise # ("reject" :: MisoString) $ ("TEST_ERROR" :: MisoString)

          -- await p returns a thunk, without evaluate here the exception would
          -- only be thrown when the result is evaluated, so we force its evaluation here
          try @JSException (await p >>= evaluate)

        Data.Either.isLeft result `shouldBe` True
