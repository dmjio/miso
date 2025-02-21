{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Miso.Runner (run) where

#if defined(wasm32_HOST_ARCH)
import qualified Language.Javascript.JSaddle.Wasm as J
#else
import qualified Language.Javascript.JSaddle.Warp as J
import           System.Environment
#endif

import           Language.Javascript.JSaddle

-- | Entry point for a miso application
#if defined(wasm32_HOST_ARCH)
run :: JSM () -> IO ()
run = J.run
#else
run :: JSM () -> IO ()
#ifndef ghcjs_HOST_OS
run x = getProgName >>= \case
    "<interactive>" -> J.debug 8008 x
    _ -> J.run 8008 x
#else
run = J.run 8008
#endif
#endif
