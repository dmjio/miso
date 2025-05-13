-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Canvas
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Canvas
  ( Canvas (..)
  , canvas_
  ) where
-----------------------------------------------------------------------------
import           Control.Monad
import           Data.Kind
import           Language.Javascript.JSaddle (JSM, JSVal, (#))
-----------------------------------------------------------------------------
import qualified Miso.FFI as FFI
import           Miso.Html.Types
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
data Canvas :: Type -> Type where
  Bind :: Canvas a -> (a -> Canvas b) -> Canvas b
  Return :: a -> Canvas a
  Method :: Method -> Canvas ()
  Image :: MisoString -> Canvas JSVal
-----------------------------------------------------------------------------
type Coord = (Double, Double)
-----------------------------------------------------------------------------
data Method
  = ClearRect Coord Double Double
  | FillRect Coord Double Double
  | StrokeRect Coord Double Double
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance Monad Canvas where
  (>>=) = Bind
  return = pure
-----------------------------------------------------------------------------
instance Applicative Canvas where
  (<*>) = ap
  pure = Return
-----------------------------------------------------------------------------
instance Functor Canvas where
  fmap = liftM
-----------------------------------------------------------------------------
instance Semigroup a => Semigroup (Canvas a) where
  (<>) = liftM2 (<>)
-----------------------------------------------------------------------------
instance Monoid a => Monoid (Canvas a) where
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftM2 mappend
#endif
  mempty  = return mempty
-----------------------------------------------------------------------------
draw :: JSVal -> Canvas a -> JSM a
draw ctx (Method (ClearRect (x,y) h w)) =
  void $ ctx # ("clearRect" :: String) $ [ x, y, h, w ]
draw ctx (Method (FillRect (x,y) h w)) =
  void $ ctx # ("fillRect" :: String) $ [ x, y, h, w ]
draw ctx (Method (StrokeRect (x,y) h w)) =
  void $ ctx # ("strokeRect" :: String) $ [ x, y, h, w ]
draw ctx (Bind m f) =
  draw ctx =<< f <$> draw ctx m
draw _ (Return m) =
  pure m
-----------------------------------------------------------------------------
canvas_ :: forall action a . [ Attribute action ] -> Canvas a -> View action
canvas_ attributes canvas = node HTML "canvas" Nothing attrs []
  where
    attrs :: [ Attribute action ]
    attrs = drawEvent : attributes

    drawEvent :: Attribute action
    drawEvent = Event $ \_ obj _ _ ->
      flip (FFI.set "draw") obj =<< do
        FFI.syncCallback1 $ \domRef -> do
          ctx <- domRef # ("getContext" :: String) $ ["2d" :: MisoString]
          void (draw ctx canvas)
-----------------------------------------------------------------------------
