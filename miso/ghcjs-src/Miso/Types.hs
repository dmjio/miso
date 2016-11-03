{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Miso.Types where

import           Data.Aeson
import           Data.JSString
import qualified Data.Map          as M
import           Data.Proxy
import           Data.String
import qualified FRP.Elerea.Simple as E
import           GHC.TypeLits
import           Miso.Storage
import           Miso.TypeLevel

data Sample a
  = Changed a
  | NotChanged a
  deriving (Functor, Show)

data Effect action model
  = Effect model (IO action)
  | NoEffect model
  deriving (Functor)

data Settings (stepConfig :: [k]) (action :: *) =
   Settings { events :: M.Map JSString Bool
            , stepConfig :: Proxy stepConfig
            , extraSignals :: [ Signal action ]
            , useIsomorphic :: Bool
            }

type Signal = SignalCore []

newtype SignalCore f a = SignalCore (E.SignalGen (E.Signal (f a)))
  deriving (Functor)

instance Applicative f => Applicative (SignalCore f) where
  pure = SignalCore . pure . pure . pure
  SignalCore f <*> SignalCore y = SignalCore $ do
    g <- f
    x <- y
    pure $ (<*>) <$> g <*> x

data DebugModel
data DebugActions
data SaveToLocalStorage (key :: Symbol)
data SaveToSessionStorage (key :: Symbol)

type DefaultStepConfig = '[]

defaultStepConfig :: Proxy DefaultStepConfig
defaultStepConfig = Proxy

instance Show model => ToAction actions model DebugModel where
  toAction _ _ m = print m

instance Show actions => ToAction actions model DebugActions where
  toAction _ as _ = print as

instance (ToJSON model, KnownSymbol sym, Show model) =>
  ToAction actions model (SaveToSessionStorage sym) where
    toAction _ _ m = setSessionStorage key m
      where
        key = fromString $ symbolVal (Proxy :: Proxy sym)

instance ( ToJSON model, KnownSymbol sym, Show model ) =>
  ToAction actions model (SaveToLocalStorage sym) where
    toAction _ _ m = setLocalStorage key m
      where
        key = fromString $ symbolVal (Proxy :: Proxy sym)

instance HasAction model action '[] where
    performActions _ _ _ = pure ()

instance ( Nub (e ': es) ~ (e ': es)
          , HasAction action model es
          , ToAction action model e
          , Show model)
   => HasAction action model (e ': es) where
     performActions _ as m =
       toAction nextAction as m >>
         performActions nextActions as m
           where
             nextAction :: Proxy e; nextActions :: Proxy es
             nextActions = Proxy; nextAction = Proxy

class Nub effects ~ effects =>
  HasAction action model effects
    where
      performActions
        :: Proxy effects
        -> [action]
        -> model
        -> IO ()

class ToAction action model effect where
  toAction :: Proxy effect -> [action] -> model -> IO ()
