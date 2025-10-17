-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Run
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Run
  ( -- ** Live reload
    run
  ) where
-----------------------------------------------------------------------------
#ifdef WASM
import qualified Language.Javascript.JSaddle.Wasm as J
#elif !GHCJS_BOTH
import           Data.Maybe
import           System.Environment
import           Text.Read
import qualified Language.Javascript.JSaddle.Warp as J
import           Network.Wai.Middleware.Static (static)
import           Network.Wai.Handler.Warp (defaultSettings, setTimeout, setPort, runSettings)
import           Network.WebSockets (defaultConnectionOptions)
import           Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleOr, jsaddleAppWithJs, jsaddleJs)
#endif
import           Language.Javascript.JSaddle
-----------------------------------------------------------------------------
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
    port <- fromMaybe 8008 . (readMaybe =<<) <$> lookupEnv "PORT"
    isGhci <- (== "<interactive>") <$> getProgName
    putStrLn $ "Running on port " <> show port <> "..."
    if isGhci
      then debugMiso port action
      else J.run port action
-----------------------------------------------------------------------------
-- | Start or restart the server, with a static 'Middlware' policy.
--
-- dmj: This is like `debug` from `jsaddle-warp`, except it uses a static
-- middleware for static file hosting.
--
-- This means that usage of `url('mario.png')` will "just work" when developing
-- from GHCi.
--
debugMiso :: Int -> JSM () -> IO ()
debugMiso port f = do
  debugWrapper $ \withRefresh registerContext ->
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr
        defaultConnectionOptions
        (registerContext >> f >> syncPoint)
        (static $ withRefresh $ jsaddleAppWithJs $ jsaddleJs True)
#endif
-----------------------------------------------------------------------------
