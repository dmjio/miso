{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Test.Hspec (it, hspec, describe, shouldSatisfy, shouldBe, Spec)
import Test.Hspec.Core.Runner (hspecResult, Summary(..))

main :: IO ()
main = do
  Summary { summaryFailures } <- hspecResult tests
  phantomExit summaryFailures

tests :: Spec
tests =
  describe "Miso tests" $ do
    it "1 + 1 = 2" $ do
      1 + 1 `shouldBe` 2

phantomExit :: Int -> IO ()
phantomExit x
  | x <= 0 = phantomExitSuccess
  | otherwise = phantomExitFail

foreign import javascript unsafe "phantom.exit(0);"
  phantomExitSuccess :: IO ()

foreign import javascript unsafe "phantom.exit(1);"
  phantomExitFail :: IO ()
