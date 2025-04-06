{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lens
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Lens
  ( -- ** Types
    Lens (..)
   -- ** Combinators
  , (.~)
  , set
  , (%~)
  , over
  , (^.)
  , (+~)
  , (*~)
  , (-~)
  , (%=)
  , modifying
  , (+=)
  , (*=)
  , (//=)
  , (-=)
  , (.=)
  , (<~)
  , (<%=)
  , (<.=)
  , (<?=)
  , (<<.=)
  , (<<%=)
  , assign
  , use
  , (?=)
  , (<>~)
  -- ** Smart constructors
  , lens
  -- ** Utils
  , (<&>)
  -- * Re-exports
  , (&)
  ) where
----------------------------------------------------------------------------
import Control.Monad.State
import Data.Function ((&))
import Control.Arrow ((<<<))
import Prelude hiding ((.), id)
import Control.Category
----------------------------------------------------------------------------
-- | Docs
-- Expose a simple @Lens@ abstraction that is compatible with other lens libraris
-- This provides users an out-of-the box experience w/o breaking the bank of
-- dependency footprint and cognitive load. Add supprt for @MonadState@ so
-- it can be used with @Effect@. Example usage below.
--
---------------------------------------------------------------------------------
-- | Person
-- > data Person = Person
-- >   { _name :: String
-- >   , _address :: Address
-- >   , _age  :: Int
-- >   } deriving (Show, Eq, Generic)
---------------------------------------------------------------------------------
-- > -- | Address type
-- > newtype Address
-- >   = Address
-- >   { _zipCode :: Zip
-- >   } deriving (Show, Eq)
---------------------------------------------------------------------------------
-- > -- | Zip code type synonym
-- > type Zip = String
---------------------------------------------------------------------------------
-- > -- | Name
-- > name :: Lens Person String
-- > name = lens _name $ \record x -> record { _name = x }
---------------------------------------------------------------------------------
-- > -- | Address lens
-- > address :: Lens Person Address
-- > address = lens _address $ \record x -> record { _address = x }
---------------------------------------------------------------------------------
-- > -- | Zip Code lens
-- > zipCode :: Lens Address Zip
-- > zipCode = lens _zipCode $ \record x -> record { _zipCode = x }
---------------------------------------------------------------------------------
-- > -- | Composition example
-- > personZip :: Lens Person Zip
-- > personZip = zipCode . address
---------------------------------------------------------------------------------
-- > person :: Person
-- > person = Person "john" (Address "90210") 33
---------------------------------------------------------------------------------
-- | Putting it all together
--
-- >
-- > main :: IO ()
-- > main = startApp person updatePerson
-- >   print $ john & address .~ Address "10012"
-- >   print $ john & zipCode . address %~ (++"!")
-- >   print $ john ^. address
--
-- > Person
-- >  { _name = "john"
-- >  , _age = 33
-- >  , _address = Address {_zipCode = "10012"}
-- >  }
---------------------------------------------------------------------------------
-- | Example usage in miso's @Effect@ @Monad@
-- >
-- > newtype Model = Model { _value :: Int }
-- >
-- > value :: Lens Model Int
-- > value = lens _value $ \model v -> model { _value = v }
-- >
-- > data Action = AddOne | SubtractOne | SayHelloWorld
-- >
-- > updateModel :: Action -> Effect Model Action ()
-- > updateModel (AddOne event) = do
-- >   value += 1
-- >   io $ consoleLog (ms (show event))
-- > updateModel (SubtractOne event) = do
-- >   value -= 1
-- >   io $ consoleLog (ms (show event))
-- > updateModel SayHelloWorld =
-- >   io (consoleLog "Hello World!")
---------------------------------------------------------------------------------
-- | Simple Lens formulation that makes it easier to create / consume lenses
data Lens record field
  = Lens
  { _get :: record -> field
  , _set :: record -> field -> record
  }
----------------------------------------------------------------------------
-- | Lens are Categories
instance Category Lens where
  id = Lens (\x -> x) (\x _ -> x)
  Lens g1 s1 . Lens g2 s2 = Lens
    { _get = g1 <<< g2
    , _set = \r f -> s2 r (s1 (g2 r) f)
    }
----------------------------------------------------------------------------
-- | Combinators
infixr 4 .~
(.~) :: Lens record field -> field -> record -> record
(.~) _lens = flip (_set _lens)
----------------------------------------------------------------------------
set :: Lens record field -> field -> record -> record
set = (.~)
----------------------------------------------------------------------------
-- | Combinators
infixr 4 %~
(%~) :: Lens record a -> (a -> a) -> record -> record
(%~) _lens f record = _set _lens record $ f (record ^. _lens)
----------------------------------------------------------------------------
-- | clone of (%~)
over :: Lens record a -> (a -> a) -> record -> record
over = (%~)
----------------------------------------------------------------------------
-- | More
infixl 8 ^.
(^.) :: record -> Lens record field -> field
(^.) = flip _get
----------------------------------------------------------------------------
-- | More
infixr 4 +~
(+~) :: Num field => Lens record field -> field -> record -> record
(+~) _lens x record = record & _lens %~ (+x)
----------------------------------------------------------------------------
-- | More
infixl 8 *~
(*~) :: Num field => Lens record field -> field -> record -> record
(*~) _lens x record = record & _lens %~ (*x)
----------------------------------------------------------------------------
-- | More
infixr 4 -~
(-~) :: Num field => Lens record field -> field -> record -> record
(-~) _lens x record = record & _lens %~ subtract x
----------------------------------------------------------------------------
-- |
infixr 4 <>~
(<>~) :: Monoid field => Lens record field -> field -> record -> record
(<>~) _lens x record = record & _lens %~ (<> x)
----------------------------------------------------------------------------
infixr 2 <~
(<~) :: MonadState record m => Lens record field -> m field -> m ()
l <~ mb = do
  b <- mb
  l .= b
----------------------------------------------------------------------------
-- | Modify a field in a State with a lens
infixl 4 %=
(%=) :: MonadState record m => Lens record field -> (field -> field) -> m ()
(%=) _lens f = modify (\r -> r & _lens %~ f)
----------------------------------------------------------------------------
-- | Synonym for (%=)
modifying :: MonadState record m => Lens record field -> (field -> field) -> m ()
modifying = (%=)
----------------------------------------------------------------------------
-- |
infixl 8 <%=
(<%=) :: MonadState record m => Lens record b -> (b -> b) -> m b
l <%= f = do
  l %= f
  use l
----------------------------------------------------------------------------
-- |
infix 4 <.=
(<.=) :: MonadState record m => Lens record b -> b -> m b
l <.= b = do
  l .= b
  return b
----------------------------------------------------------------------------
-- |
infix 4 <?=
(<?=) :: MonadState record m => Lens record (Maybe b) -> b -> m b
l <?= b = do
  l .= Just b
  return b
----------------------------------------------------------------------------
-- |
infix 4 <<.=
(<<.=) :: MonadState record m => Lens record b -> b -> m b
l <<.= b = do
  old <- use l
  l .= b
  return old
----------------------------------------------------------------------------
-- |
infix 4 <<%=
(<<%=) :: MonadState record m => Lens record b -> (b -> b) -> m b
l <<%= f = do
  old <- use l
  l %= f
  return old
----------------------------------------------------------------------------
-- | Set a field in a State with a lens
infixl 8 .=
(.=) :: MonadState record m => Lens record field -> field -> m ()
(.=) _lens f = modify (\r -> r & _lens .~ f)
----------------------------------------------------------------------------
-- | Synonym for (.=)
assign :: MonadState record m => Lens record field -> field -> m ()
assign = (.=)
----------------------------------------------------------------------------
-- | Set a field in a State with a lens
use :: MonadState record m => Lens record field -> m field
use _lens = (^. _lens) <$> get
----------------------------------------------------------------------------
-- | Set a field in a State with a lens
infix 4 ?=
(?=) :: MonadState record m => Lens record (Maybe field) -> field -> m ()
(?=) _lens value = _lens .= Just value
----------------------------------------------------------------------------
-- | Set a field in a State with a lens
infix 4 +=
(+=) :: (MonadState record m, Num field)  => Lens record field -> field -> m ()
(+=) _lens f = modify (\r -> r & _lens +~ f)
----------------------------------------------------------------------------
-- | Set a field in a State with a lens
infix 4 *=
(*=) :: (MonadState record m, Num field)  => Lens record field -> field -> m ()
(*=) _lens f = modify (\r -> r & _lens *~ f)
----------------------------------------------------------------------------
-- | Set a field in a State with a lens
infix 4 //=
(//=) :: (MonadState record m, Fractional field)  => Lens record field -> field -> m ()
(//=) _lens f = modify (\r -> r & _lens %~ (/ f))
----------------------------------------------------------------------------
-- | Set a field in a State with a lens
infixl 8 -=
(-=) :: (MonadState record m, Num field) => Lens record field -> field -> m ()
(-=) _lens f = modify (\r -> r & _lens -~ f)
---------------------------------------------------------------------------------
-- | Smart constructor 'lens' function
lens
  :: (record -> field)
  -> (record -> field -> record)
  -> Lens record field
lens = Lens
----------------------------------------------------------------------------
-- | Utils
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
f <&> x = x <$> f
----------------------------------------------------------------------------
