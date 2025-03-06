{-# LANGUAGE CPP #-}
module Miso.Runner (run) where

#if WASM
import qualified Language.Javascript.JSaddle.Wasm as J
#else
import qualified Language.Javascript.JSaddle.Warp as J
#if !GHCJS_BOTH
import           Data.Maybe
import           System.Environment
import           Text.Read
#endif
#endif

import           Language.Javascript.JSaddle

-- | Entry point for a miso application
run :: JSM () -> IO ()
#if WASM
run = J.run
#elif GHCJS_BOTH
run = J.run (error "unused argument")
#else
run action = do
    port <- (readMaybe =<<) <$> lookupEnv "PORT"
    isGhci <- (== "<interactive>") <$> getProgName
    (if isGhci then J.debug else J.run) (fromMaybe 8008 port) action
#endif
