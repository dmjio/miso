module Miso.Signal.Internal (signal, defaultSignal) where

import qualified FRP.Elerea.Simple as E
import           System.IO.Unsafe

import           Miso.Types
import           Miso.Concurrent

signal :: IO (Signal a, a -> IO ())
signal = do
  (source, sink) <- E.externalMulti
  pure (SignalCore source, \action -> sink action >> notify notifier)

defaultSignal :: (Signal a, a -> IO ())
{-# NOINLINE defaultSignal #-}
defaultSignal = unsafePerformIO signal
