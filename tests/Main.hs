{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Main where

import           Control.Monad
import           Data.Text          (Text)
import qualified Data.Text          as T
import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.Element
import           GHCJS.DOM.Node
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Types
--import           Test.QuickCheck

import           Miso

foreign import javascript unsafe "console.log($1)"
 loggin :: G.JSVal -> IO ()

main :: IO ()
main = do
  let v = VNode "div" [] [ VText "foo" Nothing ] Nothing Nothing :: VTreeBase a (Maybe Node)
  jval <- toJSVal v
  Just k <- fromJSVal jval
  print $ v == k
