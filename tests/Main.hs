{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict       as H
import           Data.Scientific
import qualified Data.Vector               as V
import           Debug.Trace
import           GHCJS.Marshal
import           Test.Hspec                (it, hspec, describe, shouldSatisfy, shouldBe, Spec)
import           Test.Hspec.Core.Runner    (hspecResult, Summary(..))
import           Test.QuickCheck
import           Test.QuickCheck.Instances

import           Miso
import           Miso.FFI
import           System.IO.Unsafe

instance Arbitrary Value where
  arbitrary = sized sizedArbitraryValue

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
compareValue (Object x) (Object y) = and $ zipWith compareValue (H.elems x) (H.elems y)
compareValue (Array x) (Array y)   = and $ zipWith compareValue (V.toList x) (V.toList y)
compareValue (String x) (String y) = x == y
compareValue (Bool x) (Bool y)     = x == y
compareValue Null Null             = True
compareValue (Number x) (Number y) = closeEnough x y
compareValue _ _ = False

closeEnough x y
  = let d = max (abs x) (abs y)
        relDiff = if (d == 0.0) then d else abs (x - y) / d
    in relDiff <= 0.00001

main :: IO ()
main = do
  Summary { summaryFailures } <- hspecResult tests
  phantomExit summaryFailures

tests :: Spec
tests = do
  storageTests
  roundTripJSVal

storageTests :: Spec
storageTests = describe "Storage tests" $ do
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

roundTripJSVal =
 describe "Serialization tests" $ do
  it "Should round trip JSVal" $ do
    property $ (\(x :: Value) -> do
      Just y <- jsvalToValue =<< toJSVal x
      compareValue x y `shouldBe` True)

phantomExit :: Int -> IO ()
phantomExit x
  | x <= 0 = phantomExitSuccess
  | otherwise = phantomExitFail

foreign import javascript unsafe "phantom.exit(0);"
  phantomExitSuccess :: IO ()

foreign import javascript unsafe "phantom.exit(1);"
  phantomExitFail :: IO ()
