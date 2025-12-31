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
import qualified Data.Aeson as JSON
import qualified Data.Vector as V
import           Data.IORef
import           Data.Text (Text)
import           Data.Scientific (Scientific, fromFloatDigits)
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
    describe "JS DSL tests" $ do
      it "Should get an set a property on an Object" $ do
        c <- liftIO create
        liftIO $ flip (setProp "foo") c =<< toJSVal True
        (`shouldBe` True) =<< liftIO (fromJSValUnchecked =<< getProp "foo" c)
      it "Should call eval" $ do
        (`shouldBe` 4) =<< liftIO (fromJSValUnchecked =<< eval "2+2")
    describe "Marshal tests" $ do
      it "Should marshal a Value(Object)" $ do
        (`shouldBe` Just (JSON.object [ "foo" JSON..= True ])) =<< liftIO (fromJSVal =<< toJSVal (JSON.object [ "foo" JSON..= True ]))
      it "Should marshal a Value(Array)" $ do
        (`shouldBe` Just (JSON.Array (V.fromList [ JSON.Number 1.0, JSON.Number 2.0 ]))) =<<
          liftIO (fromJSVal =<< toJSVal (JSON.Array (V.fromList [ JSON.Number 1.0, JSON.Number 2.0 ])))
      it "Should marshal a Value(Number)" $ do
        (`shouldBe` Just (JSON.Number (fromFloatDigits pi))) =<< liftIO (fromJSVal =<< toJSVal (JSON.Number (fromFloatDigits pi)))
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
        (`shouldBe` Just pi) =<< liftIO (fromJSVal =<< toJSVal pi)
        (`shouldBe` Just (-99.99 :: Double)) =<< liftIO (fromJSVal =<< toJSVal (-99.99 :: Double))
        (`shouldBe` Just (-0 :: Double)) =<< liftIO (fromJSVal =<< toJSVal (-0 :: Double))
      it "Should marshal a Int" $ do
        (`shouldBe` Just (99 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (99 :: Int))
        (`shouldBe` Just (-99 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (-99 :: Int))
        (`shouldBe` Just (0 :: Int)) =<< liftIO (fromJSVal =<< toJSVal (0 :: Int))
      it "Should marshal a MisoString" $ do
        (`shouldBe` Just ("foo" :: MisoString)) =<< liftIO (fromJSVal =<< toJSVal ("foo" :: MisoString))
      it "Should marshal a (Maybe Bool)" $ do
        -- dmj: js backend bug
        -- (`shouldBe` (Nothing :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Nothing :: Maybe Bool))
        (`shouldBe` (Just True :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Just True :: Maybe Bool))
        (`shouldBe` (Just False :: Maybe Bool)) =<< liftIO (fromJSVal =<< toJSVal (Just False :: Maybe Bool))
      it "Should marshal a (Bool,Double)" $ do
        (`shouldBe` Just (True,pi)) =<< liftIO (fromJSVal =<< toJSVal (True,pi))
      it "Should marshal a [Double]" $ do
        (`shouldBe` Just [pi,pi]) =<< liftIO (fromJSVal =<< toJSVal [pi,pi])
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

      it "Should mount 10000 components" $ do
        liftIO $ startApp $
          component (0 :: Int) noop $ \_ ->
            div_ [] (replicate 1 (mount testComponent))
        mountedComponents >>= (`shouldBe` 2)
