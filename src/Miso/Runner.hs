{-# LANGUAGE CPP #-}
module Miso.Runner
  ( -- ** Live reload
    run
  )
where

#ifdef WASM
import qualified Language.Javascript.JSaddle.Wasm as J
#elif !GHCJS_BOTH
import           Data.Maybe
import           System.Environment
import           Text.Read
import qualified Language.Javascript.JSaddle.Warp as J
#endif

import           Language.Javascript.JSaddle

-- | Entry point for a miso application
-- When compiling with jsaddle on native platforms
-- 'run' will start a web server for live reload
-- of your miso application.
--
-- When compiling to WASM use 'jsaddle-wasm'.
-- When compiling to JS no special package is required (simply the 'id' function).
-- JSM becomes a type synonym for IO
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
