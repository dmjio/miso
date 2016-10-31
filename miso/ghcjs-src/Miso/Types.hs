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

import GHC.TypeLits
import Data.String
import Data.Proxy
import FRP.Elerea.Simple
import Miso.Html.String
import Miso.TypeLevel

data Sample a
  = Changed a
  | NotChanged a
  deriving (Functor, Show)

data Effect action model
  = Effect model (IO action)
  | NoEffect model
  deriving (Functor)

class ExtractEvents (events :: [ (Symbol, Bool) ]) where
  extractEvents :: Proxy events -> [(MisoString, Bool)]

instance (ExtractEvents es, KnownSymbol e) =>
  ExtractEvents ('(e, 'True) ': es) where
    extractEvents _ =
      (eventName, True) :
         extractEvents (Proxy :: Proxy es)
      where
        eventName =
          fromString $ symbolVal (Proxy :: Proxy e)

instance ( ExtractEvents events, KnownSymbol event ) =>
  ExtractEvents ('(event, 'False) ': events) where
    extractEvents _ =
      (eventName, False) :
        extractEvents (Proxy :: Proxy events)
      where
        eventName = fromString $ symbolVal (Proxy :: Proxy event)

instance ExtractEvents '[] where extractEvents = const []

data Settings (events :: [(Symbol,Bool)]) (stepConfig :: [k]) (action :: *) =
   Settings { events :: Proxy events
            , stepConfig :: Proxy stepConfig
            , extraSignals :: [ SignalGen (Signal [ action ]) ]
            , useIsomorphic :: Bool
            }

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

-- instance (ToJSON model, KnownSymbol sym, Show model) =>
--   ToAction actions model (SaveToSessionStorage sym) where
--   toAction _ _ m = do
--     let key = fromString $ symbolVal (Proxy :: Proxy sym)
--     Just w <- currentWindow
--     Just s <- getSessionStorage w
--     S.setItem s key (CS.cs (encode m) :: T.Text)

-- instance ( ToJSON model, KnownSymbol sym, Show model )
--   => ToAction actions model (SaveToLocalStorage sym) where
--   toAction _ _ m = do
--     let key = T.pack $ symbolVal (Proxy :: Proxy sym)
--     Just w <- currentWindow
--     Just s <- getLocalStorage w
--     S.setItem s (textToJSString key) (CS.cs (encode m) :: T.Text)

instance HasAction model action '[] where
    performActions _ _ _ = pure ()

-- instance ( Nub (e ': es) ~ (e ': es)
--          , HasAction action model es
--          , ToAction action model e
--          , Show model)
--   => HasAction action model (e ': es) where
--     performActions _ as m =
--       toAction nextAction as m >>
--         performActions nextActions as m
--           where
--             nextAction :: Proxy e; nextActions :: Proxy es
--             nextActions = Proxy; nextAction = Proxy

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

type DefaultEvents = '[
    '("blur", 'True)
  , '("change", 'False)
  , '("click", 'False)
  , '("dblclick", 'False)
  , '("focus", 'False)
  , '("input", 'False)
  , '("keydown", 'False)
  , '("keypress", 'False)
  , '("keyup", 'False)
  , '("mouseup", 'False)
  , '("mousedown", 'False)
  , '("mouseenter", 'False)
  , '("mouseleave", 'False)
  , '("mouseover", 'False)
  , '("mouseout", 'False)
  , '("dragstart", 'False)
  , '("dragover", 'False)
  , '("dragend", 'False)
  , '("dragenter", 'False)
  , '("dragleave", 'False)
  , '("drag", 'False)
  , '("drop", 'False)
  , '("submit", 'False)
  ]
