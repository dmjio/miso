-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Exception hiding (assert)
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.State
import           Data.Aeson                 hiding (Result(..))
import qualified Data.HashMap.Strict        as H
import           Data.Scientific
import qualified Data.Vector                as V
import           Debug.Trace
import           GHCJS.Marshal
import           GHCJS.Types
import qualified JavaScript.Object.Internal as OI
import           System.IO.Unsafe
import           Test.QuickCheck hiding (total)
import           Test.QuickCheck.Instances
import           Test.QuickCheck.Monadic
----------------------------------------------------------------------------
import           Miso hiding (run, (.=))
----------------------------------------------------------------------------
instance Arbitrary Value where
  arbitrary = sized sizedArbitraryValue
----------------------------------------------------------------------------
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
----------------------------------------------------------------------------
compareValue :: Value -> Value -> Bool
compareValue (Object x) (Object y) = and $ zipWith compareValue (H.elems x) (H.elems y)
compareValue (Array x) (Array y)   = and $ zipWith compareValue (V.toList x) (V.toList y)
compareValue (String x) (String y) = x == y
compareValue (Bool x) (Bool y)     = x == y
compareValue Null Null             = True
compareValue (Number x) (Number y) = closeEnough x y
compareValue _ _ = False
----------------------------------------------------------------------------
closeEnough :: (Fractional a, Ord a) => a -> a -> Bool
closeEnough x y
  = let d = max (abs x) (abs y)
        relDiff = if (d == 0.0) then d else abs (x - y) / d
    in relDiff <= 0.00001
----------------------------------------------------------------------------
main :: IO ()
main = runTests $ do
  storageTests
  roundTripJSVal
----------------------------------------------------------------------------
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
----------------------------------------------------------------------------
roundTripJSVal :: TestM ()
roundTripJSVal =
  it "Should round trip JSVal" $ do
    propTest iso_prop
----------------------------------------------------------------------------
roundTrip
  :: Value
  -> IO Bool
roundTrip x = do
  Just y <- fromJSVal =<< toJSVal x
  pure $ compareValue x y == True
----------------------------------------------------------------------------
iso_prop :: Value -> Property
iso_prop = monadicIO . run . roundTrip
----------------------------------------------------------------------------
propTest :: Testable prop => prop -> IO (Bool, String)
propTest prop = do
  r <- quickCheckWithResult prop { chatty = False } stdArgs
  pure $ case r of
    Success { output = o } -> (True, o)
    GaveUp { output = o } -> (False, o)
    Failure { output = o } -> (False, o)
    NoExpectedFailure { output = o } -> (False, o)
----------------------------------------------------------------------------
foreign import javascript unsafe "window.global_test_results = $1;"
  writeToGlobalObject :: JSVal -> IO ()
----------------------------------------------------------------------------
runTests :: TestM () -> IO ()
runTests t = do
  results <- toJSVal =<< toResult <$> execStateT t []
  consoleLog' results
  writeToGlobalObject results
    where
      toResult :: [Test] -> TestResult
      toResult xs = TestResult failed' passed' total' duration' xs
        where
          passed'   = length (filter result xs)
          failed'   = length (filter (not . result) xs)
          total'    = length xs
          duration' = sum (map duration xs)
----------------------------------------------------------------------------
instance ToJSVal TestResult where
  toJSVal t = do
    o@(OI.Object j) <- OI.create
    flip (OI.setProp "passed") o =<< toJSVal (passed t)
    flip (OI.setProp "failed") o =<< toJSVal (failed t)
    flip (OI.setProp "total") o =<< toJSVal (total t)
    flip (OI.setProp "duration") o =<< toJSVal (duration' t)
    flip (OI.setProp "tests") o =<< toJSVal (tests t)
    pure j
----------------------------------------------------------------------------
instance ToJSVal Test where
  toJSVal t = do
    o@(OI.Object j) <- OI.create
    flip (OI.setProp "name") o =<< toJSVal (name t)
    flip (OI.setProp "result") o =<< toJSVal (result t)
    flip (OI.setProp "message") o =<< toJSVal (message t)
    flip (OI.setProp "duration") o =<< toJSVal (duration t)
    pure j
----------------------------------------------------------------------------
it :: String -> IO (Bool, String) -> TestM ()
it name test = do
  (result, msg, time) <- liftIO $ (do
     ((x,msg),t) <- clock test
     pure (x,msg,t))
      `catch` (\(e :: SomeException) ->
         pure (False, show e, 0))
  modify (Test name result msg time:)
----------------------------------------------------------------------------
data TestResult
  = TestResult
  { failed :: Int
  , passed :: Int
  , total :: Int
  , duration' :: Double
  , tests :: [Test]
  } deriving (Show, Eq)
----------------------------------------------------------------------------
data Test
  = Test
  { name :: String
  , result :: Bool
  , message :: String
  , duration :: Double
  } deriving (Show, Eq)
----------------------------------------------------------------------------
type TestM a = StateT [Test] IO a
----------------------------------------------------------------------------
shouldBe :: (Show a, Eq a, Applicative f) => a -> a -> f (Bool, String)
shouldBe x y =
  pure $
    if x == y
      then (True, mempty)
      else (False, "Expecting: " ++ show y ++ " but got: " ++ show x)
----------------------------------------------------------------------------
infix 0 `shouldBe`
----------------------------------------------------------------------------
-- | Measure in seconds
clock :: IO a -> IO (a, Double)
clock action = do
  start <- now
  x <- action
  stop <- now
  pure (x, (stop - start) / 1000)
----------------------------------------------------------------------------
