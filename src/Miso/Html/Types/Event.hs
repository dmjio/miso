{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Types.Event
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
--
----------------------------------------------------------------------------
module Miso.Html.Types.Event where

import Control.Monad.Free.Church
import Control.Monad.Free.TH
import Data.Aeson
import GHC.Generics
import Miso.String

-- | Grammar for purely handling events
data Action a where
  GetEvent :: (MisoVal -> a) -> Action a
  GetTarget :: (MisoVal -> a) -> Action a
  GetParent :: MisoVal -> (MisoVal -> a) -> Action a
  GetField :: MisoString -> MisoVal -> (Maybe MisoVal -> a) -> Action a
  GetChildren :: MisoVal -> (MisoVal -> a) -> Action a
  GetItem :: MisoVal -> Int -> (Maybe MisoVal -> a) -> Action a
  GetNextSibling :: MisoVal -> (MisoVal -> a) -> Action a
  Stringify :: FromJSON v => MisoVal -> (v -> a) -> Action a
  ConsoleLog :: MisoVal -> (() -> a) -> Action a
  SetField :: ToJSON v => MisoString -> v -> a -> Action a
  Apply :: MisoVal -> MisoString -> [Value] -> (MisoVal -> a) -> Action a

deriving instance Functor Action

-- | Options
data Options = Options {
  stopPropagation :: Bool
, preventDefault :: Bool
} deriving (Show, Eq, Generic)

-- | Type def. for event handler DSL
type Grammar a = F Action a

-- | Retrieve event
--
-- > function (e) { return e; }
--
$(makeFreeCon 'GetEvent)

-- | Retrieve target
--
-- > e.target;
--
$(makeFreeCon 'GetTarget)

-- | Attempts to lookup parent field on DOM node
--
-- > node.parent;
--
$(makeFreeCon 'GetParent)

-- | Attempts to lookup field on DOM node
--
-- > object.field;
--
$(makeFreeCon 'GetField)

-- | Assigns field on event object
--
-- > function (e) { e.field = value; }
--
$(makeFreeCon 'SetField)

-- | Retrieves children
--
-- > node.children;
--
$(makeFreeCon 'GetChildren)

-- | Retrieves child at index
--
-- > node.children[index];
--
$(makeFreeCon 'GetItem)

-- | Retrieves next sibling
--
-- > node.nextSibling;
--
$(makeFreeCon 'GetNextSibling)

-- | Executes function on object
--
-- > node['f'].apply(node.['f'], ["arg1", "arg2"])
--
$(makeFreeCon 'Apply)

-- | Logs to console
--
-- > console.log(o);
--
$(makeFreeCon 'ConsoleLog)

-- | Converts a `JSVal` to a `(FromJSON a => Result a)`
--
-- > JSON.stringify(o);
--
$(makeFreeCon 'Stringify)
