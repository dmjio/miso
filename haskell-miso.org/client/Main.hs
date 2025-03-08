{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP             #-}
module Main where

import Common (haskellMisoComponent)
import Miso (misoComponent, run)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run (misoComponent haskellMisoComponent)
