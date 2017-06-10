module Main where

import System.Process
import System.Environment
import Control.Concurrent
import System.IO

main :: IO ()
main = getArgs >>= callProcess "phantomjs"
