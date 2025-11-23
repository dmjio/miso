-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle.Monad
import           Data.IORef
import           Control.Monad.State
import qualified Data.IntMap.Strict as IM
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso
import           Miso.Lens
import           Miso.Test
import           Miso.Html
import           Miso.Runtime.Internal (components)
-----------------------------------------------------------------------------
-- | Clears the component state and DOM between each test
clearState :: JSM ()
clearState = do
  _ <- eval ("document.body.innerHTML = '';" :: MisoString)
  liftIO (writeIORef components mempty )
-----------------------------------------------------------------------------
nodeLength :: Test Int
nodeLength = do
  jsm $ fromJSValUnchecked =<< eval ("document.body.childNodes.length" :: MisoString)
-----------------------------------------------------------------------------
mountedComponents :: Test Int
mountedComponents = IM.size <$> liftIO (readIORef components)
-----------------------------------------------------------------------------
testComponent :: Component parent Int Action
testComponent = component (0 :: Int) update_ $ \_ -> button_ [ onClick AddOne ] [ "click me " ]
  where
    update_ = \case
      AddOne -> this += 1
-----------------------------------------------------------------------------
data Action = AddOne
  deriving (Show, Eq)
-----------------------------------------------------------------------------
main :: IO ()
main = do
  runTests $ do
    describe "DOM tests" $ before clearState $ do
      it "Should have access to document.body" $ do
        len <- nodeLength
        len `shouldBe` (0 :: Int)
      it "Should append a single node to document.body" $ do
          _ <- jsm $ eval ("document.body.appendChild (document.createElement('div'));" :: MisoString)
          len <- nodeLength
          len `shouldBe` (1 :: Int)
    describe "Component tests" $ do
      it "Should mount one component" $ do
        _ <- jsm (startApp testComponent)
        mountedComponents >>= (`shouldBe` 1)
    describe "Component tests" $ do
      it "Should mount 100 components" $ do
        _ <- jsm $ do
          startApp $
            component (0 :: Int) noop $ \_ ->
              div_ [] (replicate 100 (div_ [] +> testComponent))
        mountedComponents >>= (`shouldBe` 100)
