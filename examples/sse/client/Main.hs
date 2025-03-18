{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Common (sse)

import Miso (miso, run)

main :: IO ()
main = run (miso sse)
