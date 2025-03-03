{-# LANGUAGE CPP #-}
module Miso.Runner (run) where

import Data.Maybe
import System.Environment
import Text.Read

-- | Entry point for a miso application
run :: IO () -> IO ()
run = id
-- run action = do
--   port <- (readMaybe =<<) <$> lookupEnv "PORT"
--   isGhci <- (== "<interactive>") <$> getProgName
--   (if isGhci then J.debug else J.run) (fromMaybe 8008 port) action

