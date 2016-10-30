{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE StandaloneDeriving        #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Types
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.Types where

import           Data.Aeson
import qualified Data.Map         as M
import           Miso.Html.String (MisoString)
import           Miso.Event

-- | Attributes that exist on HTML
newtype Attrs = Attrs (M.Map MisoString MisoString)

-- | Fields that a DOM node contains
newtype Props = Props (M.Map MisoString Value)

-- | Individual CSS property diffing
newtype CSS = CSS (M.Map MisoString MisoString)

-- | `View` Attributes to annotate DOM, converted into `Events`, `Props`, `Attrs` and `CSS`
data Attribute action =
    E (EventHandler action)
  | A MisoString MisoString
  | C MisoString MisoString
  | P MisoString Value

data EventHandlers
  = forall a . EventHandlers [ EventHandler a ]

-- | Useful for `drop` events
newtype AllowDrop = AllowDrop Bool
  deriving (Show, Eq)
