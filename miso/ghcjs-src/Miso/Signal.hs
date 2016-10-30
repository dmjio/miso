{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Miso.Signal
  ( mergeSignals
  , mergeManySignals
  , foldp
  , signal
  ) where

-- import           Control.Concurrent
-- import           Control.Monad
import           Control.Monad.Fix
import qualified Data.Foldable                 as F
-- import           Data.IORef
import           Data.Proxy
import           FRP.Elerea.Simple
-- import           JavaScript.Web.AnimationFrame
-- import           Miso.Html.Internal
import           Miso.Types

mergeSignals
  :: SignalGen (Signal [action])
  -> SignalGen (Signal [action])
  -> SignalGen (Signal [action])
mergeSignals x y = do
  signalX <- x
  signalY <- y
  pure $ (++) <$> signalX <*> signalY

mergeManySignals :: [ SignalGen (Signal [action]) ] -> SignalGen (Signal [action])
mergeManySignals = foldl1 mergeSignals

signal :: IO (SignalGen (Signal [a]), a -> IO ())
signal = externalMulti

foldp :: ( HasAction action model effects, Eq model )
      => Proxy effects
      -> (action -> model -> Effect action model)
      -> model
      -> SignalGen (Signal [action])
      -> (action -> IO ())
      -> SignalGen (Signal (Sample model))
foldp p update ini signalGen writer = do
  actionSignal <- signalGen
  mfix $ \sig -> do
    modelSignal <- delay (NotChanged ini) sig
    effectful2 handleUpdate actionSignal modelSignal
      where
        handleUpdate actions m = do
          goFold m update actions writer >>= \case
            NotChanged newModel ->
              pure $ NotChanged newModel
            Changed newModel -> do
              performActions p actions newModel
              pure $ Changed newModel

goFold
  :: forall model action . Eq model
  => Sample model
  -> (action -> model -> Effect action model)
  -> [action]
  -> (action -> IO ())
  -> IO (Sample model)
goFold m _ [] _ = pure $ NotChanged (fromChanged m)
goFold initialModel update as _ = go initialModel as
  where
    go = F.foldrM f
    f action model =
      case update action (fromChanged model) of
        NoEffect m -> do
          pure $ case m == fromChanged initialModel of
            True -> NotChanged m
            False -> Changed m
        Effect m eff -> do
--          void . forkIO $ writer =<< eff
          newAction <- eff
          go (NotChanged m) [newAction]

fromChanged :: Sample a -> a
fromChanged (Changed x) = x
fromChanged (NotChanged x) = x
