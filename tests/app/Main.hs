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
import           Data.IORef
import           Control.Monad.State
import qualified Data.IntMap.Strict as IM
-----------------------------------------------------------------------------
import           Miso
import           Miso.DSL
import           Miso.Lens
import           Miso.Test
import           Miso.Html
import           Miso.Html.Property
import           Miso.Runtime.Internal (ComponentState (..), components, componentIds)
-----------------------------------------------------------------------------
-- | Clears the component state and DOM between each test
clearComponentState :: IO ()
clearComponentState = do
  liftIO $ do
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
          _ <- liftIO $ eval ("document.body.appendChild (document.createElement('div'));" :: MisoString)
          nodeLength >>= (`shouldBe` (1 :: Int))
    describe "Component tests" $ do
      it "Should mount one component" $ do
        liftIO (startApp testComponent)
        mountedComponents >>= (`shouldBe` 1)

      it "Should have parent field present on VDOM nodes" $ do
        _ <- liftIO (startApp testComponent)
        ComponentState {..} <- liftIO $ (IM.! 1) <$> readIORef components
        VTree (Object ref) <- liftIO (readIORef componentVTree)
        parentDomRef <- liftIO (ref ! "domRef")
        childParentField <- liftIO (ref ! "children" !! 0 ! "parent")
        childParentFieldDOMRef <- liftIO (childParentField ! "domRef")
        parentFieldNull <- liftIO (isNull childParentField)
        parentFieldNull `shouldBe` False
        parentFieldUndefined <- liftIO (isUndefined childParentField)
        parentFieldUndefined `shouldBe` False

#ifndef WASM
      it "Should mount 10,000 components" $ do
        liftIO $ startApp $
          component (0 :: Int) noop $ \_ ->
            div_ [] (replicate 9999 (mount testComponent))
        mountedComponents >>= (`shouldBe` 10000)
#endif
-----------------------------------------------------------------------------

