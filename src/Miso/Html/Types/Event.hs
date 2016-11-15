{-# LANGUAGE RankNTypes #-}
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
data Action o a where
  GetEvent :: (o -> a) -> Action o a
  GetTarget :: (o -> a) -> Action o a
  GetParent :: o -> (o -> a) -> Action o a
  GetField :: MisoString -> o -> (Maybe o -> a) -> Action o a
  GetChildren :: o -> (o -> a) -> Action o a
  GetItem :: o -> Int -> (Maybe o -> a) -> Action o a
  GetNextSibling :: o -> (o -> a) -> Action o a
  Stringify :: FromJSON v => o -> (v -> a) -> Action o a
  ConsoleLog :: o -> (() -> a) -> Action o a
  SetField :: ToJSON v => MisoString -> v -> a -> Action o a
  Apply :: o -> MisoString -> [Value] -> (o -> a) -> Action o a

deriving instance Functor (Action o)

-- | Options
data Options = Options {
  stopPropagation :: Bool
, preventDefault :: Bool
} deriving (Show, Eq, Generic)

-- | Type def. for event handler DSL
type Grammar a = forall o . F (Action o) a

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
