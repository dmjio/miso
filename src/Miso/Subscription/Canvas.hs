-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Subscription.Canvas
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Miso.Subscription.Canvas
  ( -- ** Subscriptions
    canvasSub
  ) where
-----------------------------------------------------------------------------
import Control.Monad.Reader (runReaderT)
import Control.Monad (void)
import Data.IORef
-----------------------------------------------------------------------------
import Miso.Canvas
import Miso.DSL
import Miso.Effect
import Miso.String
import Miso.Subscription.Util
-----------------------------------------------------------------------------
-- | 'Sub' for canvas operations, meant to be used with 'onCreated' / 'onDestroyed'
--
-- Example usage below
--
-- @
-- import Miso.Canvas
--
-- data Action = InitCanvas DOMRef | StopCanvas
--
-- canvasComponent :: 'Component' context props model action
-- canvasComponent = 'component' m u v
--   where
--     m = ()
--     u = \case
--       InitCanvas domRef ->
--         startSub "galaxy" $ canvasSub domRef "2d" $ \_timeStamp currentModel -> do
--           drawScene currentModel
--       StopCanvas ->
--         stopSub "galaxy"
--     v _context _props () =
--       'canvas_' [ onCreatedWith InitCanvas, onDestroyed StopCanvas ] []
--
-- drawScene :: Model -> 'Canvas' ()
-- drawScene m = do
--   'clearRect' (0, 0, 800, 480)
--   'fillStyle' ('color' Color.'Miso.CSS.Color.cornflowerblue')
--   'fillRect'  (0, 0, 800, 480)
--   'fillStyle' ('color' Color.'Miso.CSS.Color.white')
--   'font'      \"24px sans-serif\"
--   'fillText'  (\"Hello, miso!\", 32, 48)
-- @
--
-- 'canvasSub' is meant to bypass virtual DOM creation, creating a more efficient canvas
-- draw. This works by calling requestAnimationFrame in a tight loop around a freshly
-- initialized canvas (per 'onCreated').
--
-- The difference between 'canvasSub' and "Miso.Canvas" is that this operates in a tight
-- rAF loop. The latter operates on a discrete event basis and the draw is called during
-- the diffing process.
--
canvasSub
  :: DOMRef
  -- ^ The canvas 'JSVal' (meant to be consumed from 'onCreatedWith')
  -> MisoString
  -- ^ "2d", "webgpu", "webgl2"
  -> (Double -> model -> Canvas state)
  -- ^ Canvas callback in 60fps, high precision timestamp, model snapshot
  -- as args to Canvas DSL templating
  -> Sub model action
canvasSub canvasRef dim builder snk getModel = do
  createSub acquire release snk getModel
    where
      acquire = do
        ctx <- canvasRef # "getContext" $ dim
        cbRef <- newIORef (error "canvasSub: uninitialized, impossible")
        idRef <- newIORef (0 :: Int)
        callback <-
          syncCallback1 $ \jsval -> do
            void . flip runReaderT ctx =<<
              builder <$> fromJSValUnchecked jsval <*> getModel
            writeIORef idRef =<< requestAnimationFrame =<< readIORef cbRef
        writeIORef cbRef callback
        writeIORef idRef =<< requestAnimationFrame callback
        pure (callback, idRef)
  
      -- N.B. the queued frame must be cancelled before the callback is
      -- freed: the browser holds a reference to it, and invoking a freed
      -- callback on the next frame crashes the runtime.
      release (callback, idRef) = do
        cancelAnimationFrame =<< readIORef idRef
        freeFunction (Function callback)
-----------------------------------------------------------------------------
