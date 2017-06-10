{-# LANGUAGE RecordWildCards #-}
module Main where

import Test.Hspec             (it, hspec, describe, shouldSatisfy, shouldBe)
import Test.Hspec.Core.Runner (hspecResult, Summary(..))

foreign import javascript unsafe "phantom.exit();"
  phantomExit :: IO ()

main :: IO ()
main = do
  result <- hspecResult $ do
    describe "Miso tests" $ do
      it "Should do a thing" $ do
        1 + 1 `shouldBe` 2
  handle result
  phantomExit

handle :: Summary -> IO ()
handle Summary{..} =
 case summaryFailures of
    x | x > 0     -> putStrLn "error"
      | otherwise -> putStrLn "done"
