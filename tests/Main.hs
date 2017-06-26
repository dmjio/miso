{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}


module Main where

import Test.Hspec (it, hspec, describe, shouldSatisfy, shouldBe, Spec)
import Test.Hspec.Core.Runner (hspecResult, Summary(..))
import Data.Aeson

import Miso

main :: IO ()
main = do
  Summary { summaryFailures } <- hspecResult tests
  phantomExit summaryFailures

tests :: Spec
tests = do
  storageTests

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

phantomExit :: Int -> IO ()
phantomExit x
  | x <= 0 = phantomExitSuccess
  | otherwise = phantomExitFail

foreign import javascript unsafe "phantom.exit(0);"
  phantomExitSuccess :: IO ()

foreign import javascript unsafe "phantom.exit(1);"
  phantomExitFail :: IO ()
