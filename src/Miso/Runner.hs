{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Miso.Runner (run) where

#if defined(wasm32_HOST_ARCH)
import qualified Language.Javascript.JSaddle.Wasm as J
#else
import qualified Language.Javascript.JSaddle.Warp as J
#endif

import           Language.Javascript.JSaddle
import           System.Environment

-- | Entry point for a miso application
#if defined(wasm32_HOST_ARCH)
run :: JSM () -> IO ()
run = J.run
#else
run :: JSM () -> IO ()
run x = getProgName >>= \case
    "<interactive>" -> J.debug 8008 x
    _ -> J.run 8008 x
#endif
