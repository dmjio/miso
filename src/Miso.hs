{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE TemplateHaskell     #-}
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
  , run
  , mail
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
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer.Strict (tell)
import           Data.IORef
import qualified Data.Map.Strict as M
import qualified JavaScript.Object.Internal as OI
import           Miso.Diff
import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.Html
import           Miso.Router
import           Miso.Runner (run)
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
    e <- mountElement mountPoint
    -- Initial diff can be bypassed, just copy DOM into VTree
    copyDOMIntoVTree (logLevel == DebugPrerender) e iv
    -- Create virtual dom, perform initial diff
    ref <- liftIO $ newIORef $ VTree (OI.Object iv)
    registerSink app ref snk
    pure ref

-- | Runs a miso application
startApp :: Eq model => App model action -> JSM ()
startApp app@App {..} = void $
  common app $ \snk -> do
    vtree <- runView (view model) snk
    diff mountPoint Nothing (Just vtree)
    ref <- liftIO (newIORef vtree)
    registerSink app ref snk
    pure ref

-- | 'send' will send an 'action' to a different 'App'
-- Like 'notify' for 'Effect' interface, but doesn't require the model as an argument
mail
  :: App m a
  -> a
  -> Transition action model ()
mail app action = lift $ tell [ \_ -> io ]
  where
    io = liftIO $ do
      dispatch <- liftIO (readIORef componentMap)
      forM_ (M.lookup (mountPoint app) dispatch) $ \(_, _, f) ->
        f action
