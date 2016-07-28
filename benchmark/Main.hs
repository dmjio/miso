{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
module Main where

import           Control.Monad
-- import           Data.Text                  (Text)
import qualified Data.Text                  as T
-- import           GHC.Generics
-- import           GHCJS.DOM.Node
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Internal
import           GHCJS.Types
-- import qualified GHCJS.Types                as G
import           JavaScript.Object
import           JavaScript.Object.Internal
import           Miso

foreign import javascript unsafe "console.log($1)"
  loggin :: JSVal -> IO ()

main :: IO ()
main = do
  putStrLn "VTree-ing..."
  let v = VNode "div" [  ] (Just $ Key "ff") Nothing [ VNode "div" [  ] (Just $ Key "key") Nothing [] ] :: VTree action 
  jval <- toJSVal v
  loggin jval
  Just (k :: VTree action) <- fromJSVal jval
  print k
  setRenderCallback =<< initRenderCallback
  setUpdateCallback =<< initUpdateCallback

foreign import javascript unsafe "renderNode = $1"
  setRenderCallback :: Callback a -> IO ()

foreign import javascript unsafe "updateNode = $1"
  setUpdateCallback :: Callback a -> IO ()

initRenderCallback :: IO (Callback (JSVal -> JSVal -> IO ()))
initRenderCallback = syncCallback2 ContinueAsync renderTree'

initUpdateCallback :: IO (Callback (JSVal -> JSVal -> IO ()))
initUpdateCallback = syncCallback2 ContinueAsync updateTree'

updateTree' :: JSVal -> JSVal -> IO ()
updateTree' currentObj newObj = do
  putStrLn "curr object.."
  loggin currentObj
  putStrLn "new obj.."
  loggin newObj
  currVTree <- unsafeGetProp "vtree" (Object currentObj)
  putStrLn "curr vtree.."
  loggin currVTree
  Just currentTree@(VNode a b c _ cs) <- fromJSVal currVTree
  putStrLn "here"
  print (a,b,c,cs, "here")
  putStrLn "deserialized"
  resultantTree <- case isNull newObj of
    False -> do
      putStrLn "current tree isn't null"
      newTree@(VNode _ _ _ _ cs') <- renderTree newObj
      print (length cs, length cs', "lengths")
      print (newTree, "newTree")
      print (currentTree, "currTree")
      currentTree `datch` newTree 
    True -> do
      putStrLn "current tree is null"
      currentTree `datch` VEmpty
  toJSVal resultantTree >>= \t -> unsafeSetProp "vtree" t (Object currentObj)

renderTree' :: JSVal -> JSVal -> IO ()
renderTree' o jval = do
  vtree <- renderTree jval
  initdVtree@(VNode _ _ _ (Just element) cs) <- VEmpty `datch` vtree
  putStrLn "das tree" >> print (length cs)
  toJSVal element >>= \v -> unsafeSetProp "element" v (Object o)
  toJSVal initdVtree >>= \t -> unsafeSetProp "vtree" t (Object o)

renderTree :: JSVal -> IO (VTree action)
renderTree = go
    where
      go :: JSVal -> IO (VTree a)
      go jsval' = do
        nodes <- fromJSValUncheckedListOf jsval'
        cs <- forM nodes $ \n -> do
          key :: Int <- pFromJSVal <$> unsafeGetProp "key" (Object n)
          let k = Just $ Key $ T.pack $ show key
          children <- unsafeGetProp "children" (Object n)
          case isNull children of
            True -> do
              pure $ VNode "span" [] k Nothing []
            False -> do
              cs <- mapM go =<< fromJSValUncheckedListOf children
              pure $ VNode "div" [] k Nothing cs
        pure $ VNode "div" [] Nothing Nothing cs

      

