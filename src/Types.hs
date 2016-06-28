module Types where

import           Control.Applicative
import qualified FRP.Elerea.Simple   as FRP
import           FRP.Elerea.Simple   hiding (Signal)

data Sample a = Changed a | NotChanged a
  deriving (Show, Eq)

instance Functor Sample where fmap = liftA

instance Applicative Sample where
  pure = NotChanged
  Changed f <*> Changed x = Changed (f x)
  Changed f <*> NotChanged x = Changed (f x)
  NotChanged f <*> Changed x = Changed (f x)
  NotChanged f <*> NotChanged x = NotChanged (f x)

fromChanged :: Sample x -> x
fromChanged (Changed x) = x
fromChanged (NotChanged x) = x

newtype Signal a = Signal {
  signalGen :: SignalGen (FRP.Signal (Sample [a]))
 }

instance Functor Signal where fmap = liftA

instance Applicative Signal where
  pure = Signal . pure . pure . pure . pure
  Signal f <*> Signal x = Signal $ liftA2 (liftA2 (liftA2 (<*>))) f x
