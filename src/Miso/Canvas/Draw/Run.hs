-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Canvas.Draw.Run
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The 'Draw' instance that executes through "Miso.Canvas": one call per
-- operation, with the entity loop in Haskell.  This is the general path;
-- see "Miso.Canvas.Draw" for why it is a separate module.
----------------------------------------------------------------------------
module Miso.Canvas.Draw.Run
  ( Run
  , RunEnv (..)
  , runDraw
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peekElemOff)
#ifdef __MHS__
import           Data.Double (doubleToInt)
#endif
-----------------------------------------------------------------------------
import           Miso.Canvas (Canvas)
import qualified Miso.Canvas as Canvas
import           Miso.Canvas.Draw
import qualified Miso.CSS.Color as Color
import           Miso.String (ms)
-----------------------------------------------------------------------------
-- | What a frame needs: the entity array and the inputs.
data RunEnv
  = RunEnv
  { reEntities :: Ptr Double
  , reCount    :: Int
  , reTime     :: Double
  , reFps      :: Double
  }
-----------------------------------------------------------------------------
-- | A drawing executed through the 'Canvas' API, one call per operation.
newtype Run a = Run (RunEnv -> Canvas a)
-----------------------------------------------------------------------------
runDraw :: Run () -> RunEnv -> Canvas ()
runDraw (Run f) = f
-----------------------------------------------------------------------------
unRun :: Run a -> RunEnv -> Canvas a
unRun (Run f) = f
-----------------------------------------------------------------------------
pureR :: a -> Run a
pureR x = Run (\_ -> pure x)
-----------------------------------------------------------------------------
lift2 :: (a -> b -> c) -> Run a -> Run b -> Run c
lift2 op (Run f) (Run g) = Run (\e -> op <$> f e <*> g e)
-----------------------------------------------------------------------------
instance Num (Run Double) where
  (+) = lift2 (+)
  (-) = lift2 (-)
  (*) = lift2 (*)
  abs (Run f) = Run (fmap abs . f)
  signum (Run f) = Run (fmap signum . f)
  fromInteger = pureR . fromInteger
-----------------------------------------------------------------------------
instance Fractional (Run Double) where
  (/) = lift2 (/)
  fromRational = pureR . fromRational
-----------------------------------------------------------------------------
-- | 'Double' to 'Int' without going through 'Integer' (slow on MicroHs).
toInt :: Double -> Int
#ifdef __MHS__
toInt = doubleToInt
#else
toInt = truncate
#endif
-----------------------------------------------------------------------------
instance Draw Run where
  Run a >. Run b = Run (\e -> a e >> b e)
  done = pureR ()
  input Time = Run (pure . reTime)
  input Fps = Run (pure . reFps)
  input Count = Run (pure . fromIntegral . reCount)
  fmod_ = lift2 (\x m -> let r = x - fromIntegral (toInt (x / m)) * m in if r < 0 then r + m else r)
  str = pureR
  showFixed n (Run f) = Run $ \e -> do
    x <- f e
    let k = 10 ^ n :: Int
        i = toInt (x * fromIntegral k)
        (whole, frac) = i `quotRem` k
    pure $ show whole ++ (if n == 0 then "" else "." ++ pad n (show (abs frac)))
    where pad w s = replicate (w - length s) '0' ++ s
  cat parts = Run $ \e -> concat <$> mapM (`unRun` e) parts
  fillStyleRGB r g b = Run $ \e -> do
    r' <- unRun r e; g' <- unRun g e; b' <- unRun b e
    Canvas.fillStyle (Canvas.color (Color.rgb (toInt r') (toInt g') (toInt b')))
  fillRect x y w h = Run $ \e -> do
    x' <- unRun x e; y' <- unRun y e; w' <- unRun w e; h' <- unRun h e
    Canvas.fillRect (x', y', w', h')
  save = Run (\_ -> Canvas.save ())
  restore = Run (\_ -> Canvas.restore ())
  translate x y = Run $ \e -> do
    x' <- unRun x e; y' <- unRun y e
    Canvas.translate (x', y')
  font f = Run (\_ -> Canvas.font (ms f))
  fillText s x y = Run $ \e -> do
    s' <- unRun s e; x' <- unRun x e; y' <- unRun y e
    Canvas.fillText (ms s', x', y')
  forEntities body = Run $ \e ->
    forM_ [0 .. reCount e - 1] $ \i -> do
      let field k = Run (\env -> liftIO (peekElemOff (reEntities env) (i * entityStride + k)))
      unRun (body (Entity (field 0) (field 1) (field 2) (field 3) (field 4) (field 5))) e
-----------------------------------------------------------------------------
