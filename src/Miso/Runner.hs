{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Miso.Runner (run) where

#if defined(wasm32_HOST_ARCH)
import qualified Language.Javascript.JSaddle.Wasm as J
#elif defined (ghcjs_HOST_OS)
#else
import qualified Language.Javascript.JSaddle.Warp as J
import           Data.Maybe
import           System.Environment
import           Text.Read
#endif

import           Language.Javascript.JSaddle

-- | Entry point for a miso application
run :: JSM () -> IO ()
#if defined(wasm32_HOST_ARCH)
run = J.run
#elif defined(ghcjs_HOST_OS)
run = id
#else
run action = do
    port <- (readMaybe =<<) <$> lookupEnv "PORT"
    isGhci <- (== "<interactive>") <$> getProgName
    (if isGhci then J.debug else J.run) (fromMaybe 8008 port) action
#endif
