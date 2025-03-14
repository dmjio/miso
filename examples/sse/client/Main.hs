{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Common (sseComponent)

import Miso (misoComponent, run)

main :: IO ()
main = run (misoComponent sseComponent)
