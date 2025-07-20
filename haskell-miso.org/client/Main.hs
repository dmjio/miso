{-# LANGUAGE CPP              #-}

module Main where

import Common (haskellMisoComponent)
import Miso (startComponent, run)
import Miso.String

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run (startComponent haskellMisoComponent)
