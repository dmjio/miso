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
--
-- Support for running and live-reloading of miso applications.
----------------------------------------------------------------------------
module Miso.Run
  ( -- ** Live reload
    run
  , reload
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
-- | Entry point for a miso application.
--
-- * When compiling with GHC (native), this starts a web server for live reload, using [jsaddle](https://hackage.haskell.org/package/jsaddle).
-- * When compiling to WASM, this uses [jsaddle-wasm](https://hackage.haskell.org/package/jsaddle-wasm).
-- * When compiling to JS (GHCJS), this is simply 'id'.
run
  :: JSM ()
  -- ^ A JSM action typically created using 'Miso.miso' or 'Miso.startApp'
  -> IO ()
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
    else
      runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (action >> syncPoint)
        (static J.jsaddleApp)
-----------------------------------------------------------------------------
-- | Like 'run', but clears the <body> and <head> on each reload.
--
-- Meant to be used with WASM browser mode
--
-- @since 1.9.0.0
reload
  :: JSM ()
  -- ^ A JSM action typically created using 'Miso.miso' or 'Miso.startApp'
  -> IO ()
reload action = do
  clearBody
  clearHead
  action
-----------------------------------------------------------------------------
-- | Start or restart the server, with a static Middleware policy.
--
-- dmj: This is like @debug@ from `jsaddle-warp`, except it uses a static
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
