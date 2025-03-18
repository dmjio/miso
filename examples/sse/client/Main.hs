{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Common (sseComponent)

import Miso (miso, run)

main :: IO ()
main = run (miso sseComponent)
