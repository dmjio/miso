{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L
import           Text.Jasmine

main :: IO ()
main = do
  diffJs       <- minifyFile "jsbits/diff.js"
  isomorphicJs <- minifyFile "jsbits/isomorphic.js"
  utilJs       <- minifyFile "jsbits/util.js"
  delegateJs   <- minifyFile "jsbits/delegate.js"
  L.writeFile "JSBits.hs" $
     L.intercalate "\n"
     [ "{-# LANGUAGE OverloadedStrings #-}\n"
     , "module Miso.JSBits where\n"
     , "import Data.Text\n"
     , "diffJs, isomorphicJs, utilJs, delegateJs :: Text\n"
     , "diffJs = " <> "\"" <> diffJs <> "\"\n"
     , "isomorphicJs = " <> "\"" <> isomorphicJs <> "\"\n"
     , "utilJs = " <> "\"" <> utilJs <> "\"\n"
     , "delegateJs = " <> "\"" <> delegateJs <> "\"\n"
     ]

