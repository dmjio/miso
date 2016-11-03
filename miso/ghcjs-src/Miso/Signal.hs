{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Miso.Signal
  ( mergeSignals
  , mergeManySignals
  , foldp
  , signal
  , start
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Fix
import           Data.Proxy
import qualified FRP.Elerea.Simple  as E

import           Miso.Concurrent    ( Notify (..), notifier )
import           Miso.Types

mergeSignals
  :: Signal action
  -> Signal action
  -> Signal action
mergeSignals (SignalCore x) (SignalCore y) = SignalCore $ do
  signalX <- x
  signalY <- y
  pure $ (++) <$> signalX <*> signalY

mergeManySignals :: [ Signal action ] -> Signal action
mergeManySignals = foldl1 mergeSignals

start :: SignalCore f a -> IO (IO (f a))
start (SignalCore s) = E.start s

signal :: IO (Signal a, a -> IO ())
signal = do
  (source, sink) <- E.externalMulti
  pure (SignalCore source, \action -> sink action >> notify notifier)

foldp :: ( HasAction action model effects, Eq model )
      => Proxy effects
      -> (action -> model -> Effect action model)
      -> model
      -> Signal action
      -> (action -> IO ())
      -> SignalCore Sample model
foldp p update ini (SignalCore signalGen) writer = SignalCore $ do
  actionSignal <- signalGen
  mfix $ \sig -> do
    modelSignal <- E.delay (NotChanged ini) sig
    E.effectful2 handleUpdate actionSignal modelSignal
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
