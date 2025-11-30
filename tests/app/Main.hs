-----------------------------------------------------------------------------
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Prelude hiding ((!!))
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           Language.Javascript.JSaddle.Monad
import           Data.IORef
import           Control.Monad.State
import qualified Data.IntMap.Strict as IM
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import           Miso
import           Miso.Html.Property
import           Miso.Lens
import           Miso.Test
import           Miso.Html
import           Miso.Runtime.Internal (ComponentState (..), components, componentIds)
-----------------------------------------------------------------------------
-- | Clears the component state and DOM between each test
clearComponentState :: JSM ()
clearComponentState = do
  liftIO $ do
    writeIORef components mempty
    writeIORef componentIds initialComponentId
-----------------------------------------------------------------------------
clearBody :: JSM ()
clearBody = void $ eval ("document.body.innerHTML = '';" :: MisoString)
-----------------------------------------------------------------------------
initialComponentId :: ComponentId
initialComponentId = 1
-----------------------------------------------------------------------------
nodeLength :: Test Int
nodeLength = do
  jsm $ fromJSValUnchecked =<< eval ("document.body.childNodes.length" :: MisoString)
-----------------------------------------------------------------------------
mountedComponents :: Test Int
mountedComponents = IM.size <$> liftIO (readIORef components)
-----------------------------------------------------------------------------
getComponentById :: ComponentId -> Test (ComponentState m a)
getComponentById vcompId = (IM.! vcompId) <$> liftIO (readIORef components)
-----------------------------------------------------------------------------
testComponent :: Component parent Int Action
testComponent = component (0 :: Int) update_ $ \_ -> button_ [ id_ "foo", onClick AddOne ] [ "click me " ]
  where
    update_ = \case
      AddOne -> this += 1
-----------------------------------------------------------------------------
data Action = AddOne
  deriving (Show, Eq)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = do
  runTests $ beforeEach clearBody $ afterEach clearComponentState $ do
    describe "DOM tests" $ do
      it "Should have access to document.body" $ do
        nodeLength >>= (`shouldBe` (0 :: Int))
      it "Should append a single node to document.body" $ do
          _ <- jsm $ eval ("document.body.appendChild (document.createElement('div'));" :: MisoString)
          nodeLength >>= (`shouldBe` (1 :: Int))
    describe "Component tests" $ do
      it "Should mount one component" $ do
        _ <- jsm (startApp testComponent)
        mountedComponents >>= (`shouldBe` 1)

      it "Should have parent field present on VDOM nodes" $ do
        _ <- jsm (startApp testComponent)
        ComponentState {..} <- (IM.! 1) <$> liftIO (readIORef components)
        VTree (Object ref) <- liftIO (readIORef componentVTree)
        parentDomRef <- jsm $ ref ! "domRef"
        childParentField <- jsm $ ref ! "children" !! 0 ! "parent"
        childParentFieldDOMRef <- jsm $ childParentField ! "domRef"
        parentFieldNull <- jsm $ ghcjsPure (isNull childParentField)
        parentFieldNull `shouldBe` False
        parentFieldUndefined <- jsm $ ghcjsPure (isUndefined childParentField)
        parentFieldUndefined `shouldBe` False

#ifndef WASM
      it "Should mount 10,000 components" $ do
        _ <- jsm $ do
          startApp $
            component (0 :: Int) noop $ \_ ->
              div_ [] (replicate 9999 (div_ [] +> testComponent))
        mountedComponents >>= (`shouldBe` 10000)
#endif
-----------------------------------------------------------------------------

