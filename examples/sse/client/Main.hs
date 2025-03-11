{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common (sseComponent)

import Miso (run, misoComponent)

main :: IO ()
main = run (misoComponent sseComponent)
