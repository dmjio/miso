{-# LANGUAGE CPP #-}
module Miso.Runner (run) where

#ifdef WASM
import qualified Language.Javascript.JSaddle.Wasm as J
#endif

#ifndef GHCJS_BOTH
import           Data.Maybe
import           System.Environment
import           Text.Read
import qualified Language.Javascript.JSaddle.Warp as J
#endif

import           Language.Javascript.JSaddle

-- | Entry point for a miso application
run :: JSM () -> IO ()
#ifdef WASM
run = J.run
#elif GHCJS_BOTH
run = id
#else
run action = do
    port <- (readMaybe =<<) <$> lookupEnv "PORT"
    isGhci <- (== "<interactive>") <$> getProgName
    (if isGhci then J.debug else J.run) (fromMaybe 8008 port) action
#endif
