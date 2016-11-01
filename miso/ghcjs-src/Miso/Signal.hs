{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Miso.Signal
  ( mergeSignals
  , mergeManySignals
  , foldp
  , signal
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Data.Proxy
import FRP.Elerea.Simple
import Miso.Types
import Miso.Concurrent ( Notify (..), notifier )

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
signal = do
  (source, sink) <- externalMulti
  pure (source, \action -> sink action >> notify notifier)

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
          goFold (fromChanged m) update actions writer >>= \case
            NotChanged newModel ->
              pure $ NotChanged newModel
            Changed newModel -> do
              performActions p actions newModel
              pure $ Changed newModel

goFold
  :: forall model action . Eq model
  => model
  -> (action -> model -> Effect action model)
  -> [action]
  -> (action -> IO ())
  -> IO (Sample model)
goFold initialModel update actions writer = go initialModel actions
  where
    go model [] | model == initialModel = pure (NotChanged model)
                | otherwise = pure (Changed model)
    go model (a:as) = do
      case update a model of
        NoEffect m -> go m as
        Effect m eff -> do
          void . forkIO $ writer =<< eff
          go m as

fromChanged :: Sample a -> a
fromChanged (Changed x) = x
fromChanged (NotChanged x) = x
