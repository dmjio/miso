{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# OPTIONS_GHC -fno-warn-orphans    #-}
module Main where

import           Control.Exception hiding (assert)
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.State
import           Data.Aeson hiding (Result(..))
import           Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import           Data.Proxy
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Debug.Trace
import           System.IO.Unsafe
import           Test.QuickCheck hiding (total)
import           Test.QuickCheck.Monadic

import           Miso.FFI
import           Miso.Storage
import           Miso.String

default (JSString, Int)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

newtype SizedValue = SizedValue Value
  deriving (Show, Eq)

instance Arbitrary SizedValue where
  arbitrary = SizedValue <$> sized sizedArbitraryValue

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

instance Arbitrary JSString where
  arbitrary = toJSString <$> arbitrary

instance Arbitrary Text where
  arbitrary = textFromJSString <$> arbitrary

instance Arbitrary Array where
  arbitrary = do
    ns <- choose (1,1)
    vs <- sizedArbitraryValue ns
    pure (V.fromList [vs])

sizedArbitraryValue :: Int -> Gen Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure Null, bool, string, number, array, object']
  where
    n' = n `div` 2
    bool = Bool <$> arbitrary
    number = Number <$> arbitrary
    string = String <$> arbitrary
    array = Array <$> arbitrary
    object' = Object <$> arbitrary

compareValue :: Value -> Value -> Bool
compareValue (Object x) (Object y) =
  and $ zipWith compareValue (KM.elems x) (KM.elems y)
compareValue (Array x) (Array y)   =
  and $ zipWith compareValue (V.toList x) (V.toList y)
compareValue (String x) (String y) = x == y
compareValue (Bool x) (Bool y)     = x == y
compareValue Null Null             = True
compareValue (Number x) (Number y) = closeEnough x y
compareValue _ _ = False

closeEnough x y
  = let d = max (abs x) (abs y)
        relDiff = if (d == 0.0) then d else abs (x - y) / d
    in relDiff <= 0.00001

main = do
  -- print =<< through ('a' :: Char)
  -- print =<< through ('z' :: Char)
  -- print =<< through ("foo" :: String)
  -- print =<< through ("foo" :: Value)
  -- print =<< through ("abc" :: [Char])
  -- print =<< through (234.44 :: Scientific)
  -- print =<< through (234.44 :: Scientific, "foo" :: String)
  -- print =<< through [234.44 :: Scientific]

  -- putStrLn "aeson tests!"
  print =<< through Null
  primitives
  -- print =<< through (Bool True)
  -- print =<< through (Bool False)
  -- print =<< through (String "hiya")
  -- print =<< through (Number 123.123)
  -- print =<< through (Array (V.fromList [Bool True, Bool False]))
  -- print =<< through (object [ ("hey", "lou") ])

  -- boundedNumeric (Proxy @Int)
  -- print =<< through ("foo" :: String)
  -- print =<< through True
  -- print =<< through False
  -- pure ()
  -- boundedNumeric (Proxy @Word)
  -- boundedNumeric (Proxy @Integer)
  -- runTests $ do
  -- --   -- storageTests
  --   roundTripJSVal

through :: (ToJSVal a, FromJSVal a) => a -> IO a
through = fromJSVal <=< toJSVal

boundedNumeric
  :: forall a
  . (Show a, FromJSVal a, ToJSVal a, Num a, Bounded a)
  => Proxy a
  -> IO ()
boundedNumeric Proxy = do
  consoleLog "== round trip single values =="
  print =<< through (1 :: a)
  print =<< through (0 :: a)
  print =<< through ((-1) :: a)
  print =<< through (maxBound :: a)
  print =<< through (minBound :: a)

primitives :: IO ()
primitives = do
  consoleLog "== null tests =="
  consoleLog jsNull
  consoleLog "== array tests =="
  arr <- newJsArray
  consoleLog arr
  pushJsArray arr jsTrue
  consoleLog arr
  consoleLog =<< fromList [1,2,3]
  consoleLog "== object tests =="
  consoleLog =<< newJSObject
  consoleLog "== bool tests =="
  consoleLog jsTrue
  consoleLog jsFalse

  consoleLog "== sync callback tests =="

  cb0 <- syncCallback $ do
    consoleLog "inside syncCallback - the one that fails"
    pure ()

  consoleLog =<< invokeSyncCallback cb0
  consoleLog "invoked syncCallback"

  cb1 <- syncCallback1' $ \s -> do
    consoleLog "inside sync callback (that returns a value)"
    pure s

  consoleLog =<< invokeSyncCallback cb1

  cb2 <- syncCallback' $ do
    consoleLog "inside sync callback w/o args (that returns a value)"
    toJSVal "made it through again!"

  consoleLog =<< invokeSyncCallback cb2

  -- cb3 <- syncCallback $ do
  --   consoleLog "inside sync callback w/o args (that doesn't return a value)"

  -- consoleLog =<< invokeSyncCallback cb3

  consoleLog "== async callback tests =="
  cb <- asyncCallback $ consoleLog "inside an async call w/o args"
  invokeAsyncCallback cb

  cb <- asyncCallback1 $ \s -> do
    consoleLog "inside an async call w/ an arg (below)" 
    consoleLog s
  invokeAsyncCallback cb

foreign import javascript "$1('made it through')"
  invokeSyncCallback :: JSCallback a -> IO JSVal

-- foreign import javascript "wrapper sync"
--   syncCallback :: IO () -> IO JSCallback
--
-- dmj: ^ this one breaks



foreign import javascript "$1('some arg')" invokeAsyncCallback :: JSCallback a -> IO ()

storageTests :: TestM ()
storageTests = do
  it "should write to and read from local storage" $ do
    let obj = object [ "foo" .= ("bar" :: String) ]
    setLocalStorage "foo" obj
    Right r <- getLocalStorage "foo"
    r `shouldBe` obj
  it "should write to and read from session storage" $ do
    let obj = object [ "foo" .= ("bar" :: String) ]
    setSessionStorage "foo" obj
    Right r <- getLocalStorage "foo"
    r `shouldBe` obj

roundTripJSVal :: TestM ()
roundTripJSVal =
  it "Should round trip JSVal" $ do
    propTest iso_prop

roundTrip
  :: Value
  -> IO Bool
roundTrip x = do
  y <- toJSVal x
  z <- fromJSVal y
--  print ("x", x, "z", z)
  pure $ compareValue x z == True

iso_prop :: Value -> Property
iso_prop = monadicIO . run . roundTrip

propTest :: Testable prop => prop -> IO (Bool, MisoString)
propTest prop = do
  r <- flip quickCheckWithResult prop stdArgs { chatty = False, maxSuccess = 125 }
  pure $ case r of
    Success { output = o } -> (True, ms o)
    GaveUp { output = o } -> (False, ms o)
    Failure { output = o } -> (False, ms o)
    NoExpectedFailure { output = o } -> (False, ms o)

foreign import javascript unsafe "window.global_test_results = $1;"
  writeToGlobalObject :: JSVal -> IO ()

foreign import javascript "console.log($1)" consoleLogJSVal :: JSVal -> IO ()

runTests :: TestM () -> IO ()
runTests t = do
  results <- toJSVal =<< toResult <$> execStateT t []
  consoleLogJSVal results
  writeToGlobalObject results
    where
      toResult :: [Test] -> TestResult
      toResult xs = TestResult failed' passed' total' duration' xs
        where
          passed'   = length (filter result xs)
          failed'   = length (filter (not . result) xs)
          total'    = length xs
          duration' = sum (map duration xs)

instance ToJSVal TestResult where
  toJSVal t = do
    o@(JSObject j) <- newJSObject
    set "passed" (passed t) o
    set "failed" (failed t) o
    set "total" (total t) o
    set "duration" (duration' t) o
    set "tests" (tests t) o
    pure j

instance ToJSVal Test where
  toJSVal t = do
    o@(JSObject j) <- newJSObject
    set "name" (name t) o
    set "result" (result t) o
    set "message" (message t) o
    set "duration" (duration t) o
    pure j

it :: JSString -> IO (Bool, JSString) -> TestM ()
it name test = do
  (result, msg, time) <- liftIO $ (do
     ((x,msg),t) <- clock test
     pure (x,msg,t))
      `catch` (\(e :: SomeException) ->
         pure (False, ms (show e), 0))
  modify (Test name result msg time:)

data TestResult
  = TestResult
  { failed :: Int
  , passed :: Int
  , total :: Int
  , duration' :: Double
  , tests :: [Test]
  } deriving (Show, Eq)

data Test
  = Test
  { name :: JSString
  , result :: Bool
  , message :: JSString
  , duration :: Double
  } deriving (Show, Eq)

type TestM a = StateT [Test] IO a

shouldBe
  :: (ToMisoString a, Show a, Eq a, Applicative f) => a -> a -> f (Bool, JSString)
shouldBe x y =
  pure $
    if x == y
      then (True, mempty)
      else
        (False, "Expecting: "
          <> ms y
          <> " but got: "
          <> ms x)

infix 0 `shouldBe`

-- | Measure in seconds
clock :: IO a -> IO (a, Double)
clock action = do
  start <- now
  x <- action
  stop <- now
  pure (x, (stop - start) / 1000)
