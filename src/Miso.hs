{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}

#ifdef IOS
#else
{-# LANGUAGE TemplateHaskell     #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso
  ( miso
  , startApp
  , sink
  , notify
  , module Miso.Effect
  , module Miso.Event
  , module Miso.Html
  , module Miso.Subscription
#ifndef ghcjs_HOST_OS
  , module Miso.TypeLevel
#endif
  , module Miso.Types
  , module Miso.Router
  , module Miso.Util
  , module Miso.FFI
  , module Miso.WebSocket
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import qualified JavaScript.Object.Internal as OI
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.Html
import           Miso.Router
import           Miso.Subscription
#ifndef ghcjs_HOST_OS
import           Miso.TypeLevel
#endif
import           Miso.Types hiding (Component(..))
import           Miso.Util
import           Miso.Internal
import           Miso.WebSocket

-- | Runs an isomorphic miso application.
-- Assumes the pre-rendered DOM is already present
miso :: Eq model => (URI -> App model action) -> JSM ()
miso f = void $ do
  app@App {..} <- f <$> getCurrentURI
  common app $ \snk -> do
    VTree (OI.Object iv) <- runView (view model) snk
    registerSink app snk
    e <- mountElement mountPoint
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) e iv
    -- Create virtual dom, perform initial diff
    liftIO $ newIORef $ VTree (OI.Object iv)

-- | Runs a miso application
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} = void $
  common app $ \snk -> do
    vtree <- runView (view model) snk
    registerSink app snk
    consoleLog "diffing in start app"
    diff mountPoint Nothing (Just vtree)
    liftIO (newIORef vtree)
