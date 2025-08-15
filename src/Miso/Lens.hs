-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE RankNTypes          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lens
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This modules exposes a very simple 'Lens' formulation that is compatible with other lens libraries.
--
-- For state management of miso applications, this module should meet all of your needs. It also
-- ensures a smaller payload size during compilation.
--
-- @
-- data Lens record field
--  = Lens
--  { _get :: record -> field
--  , _set :: record -> field -> record
--  }
-- @
--
-- The goal is to provide users with an out-of-the box lens experience without the large
-- dependency footprint and cognitive load. This module also aims to preserve semantics of
-- existing lens combinators using a simple formulation (not the van Laarhoven). It must be imported
-- separately (@import Miso.Lens@) and can be used with the @Effect@ Monad inside of a miso
-- application (as described below).
--
-- This module is at fixity and interface parity with @lens@ and @microlens@ and can therefore
-- be used interchangeably with them. Simply replace the @Miso.Lens@ import with @Control.Lens@.
-- For convenience we re-export the 'Lens'' synonym to ease the transition into @lens@ or
-- @microlens@.
--
-- For the curious reader, if you'd like more information on @lens@ and the van Laarhoven
-- formulation, we recommend the @lens@ library <https://hackage.haskell.org/package/lens>.
--
-- @
-- -- Person type
-- data Person = Person
--   { _name :: String
--   , _address :: Address
--   , _age  :: Int
--   } deriving (Show, Eq, Generic)
--
-- -- Address type
-- newtype Address
--   = Address
--   { _zipCode :: Zip
--   } deriving (Show, Eq)
--
-- -- | Zip code type synonym
-- type Zip = String
--
-- -- | Name Lens
-- name :: Lens Person String
-- name = 'lens' _name $ \\record x -> record { _name = x }
--
-- -- | Address Lens
-- address :: Lens Person Address
-- address = 'lens' _address $ \\record x -> record { _address = x }
--
-- -- | Zip Code Lens
-- zipCode :: Lens Address Zip
-- zipCode = 'lens' _zipCode $ \\record x -> record { _zipCode = x }
--
-- -- | Lens Composition example
-- personZip :: Lens Person Zip
-- personZip = zipCode . address
--
-- -- | Person example
-- person :: Person
-- person = Person "john" (Address "90210") 33
--
-- main :: IO ()
-- main = print $ john '&' address '.~' Address "10012"
--
-- > Person
-- >  { _name = "john"
-- >  , _age = 33
-- >  , _address = Address {_zipCode = "10012"}
-- >  }
-- @
--
-- Example usage with miso's @Effect@ @Monad@
--
-- @
-- newtype Model = Model { _value :: Int }
--
-- value :: Lens Model Int
-- value = 'lens' _value $ \\model v -> model { _value = v }
--
-- data Action = AddOne | SubtractOne
--
-- updateModel :: Action -> 'Effect' Model Action
-- updateModel AddOne      = value '+=' 1
-- updateModel SubtractOne = value '-=' 1
-- @
----------------------------------------------------------------------------
module Miso.Lens
  ( -- ** Types
    Lens (..)
  , Prism (..)
  , Getter
  , Setter
    -- ** Smart constructor
  , lens
    -- ** Re-exports
  , (&)
  , (<&>)
    -- ** Lens Combinators
  , (.~)
  , (?~)
  , set
  , (%~)
  , over
  , (^.)
  , (+~)
  , (*~)
  , (//~)
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
  , view
  , (?=)
  , (<>~)
  , _1
  , _2
  , _id
  , this
    -- ** Prism Combinators
  , prism
  , preview
  , review
  , _Nothing
  , _Just
  , _Left
  , _Right
  , (^?)
  -- *** Re-exports
  , compose
  -- *** Conversion
  , Lens'
  , toVL
  , fromVL
  ) where
----------------------------------------------------------------------------
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, modify, gets)
import Control.Monad.Identity (Identity(..))
import Control.Category (Category (..))
import Control.Arrow ((<<<))
import Data.Functor.Const (Const(..))
import Data.Function ((&))
import Data.Functor((<&>))
import Data.Kind (Type)
import Prelude hiding ((.))
----------------------------------------------------------------------------
import Miso.Util (compose)
----------------------------------------------------------------------------
-- | A @Lens@ is a generalized getter and setter.
--
-- Lenses allow both the retrieval of values from fields in a record and the
-- assignment of values to fields in a record. The power of a @Lens@ comes
-- from its ability to be composed with other lenses.
--
-- In the context of building applications with miso, the @model@ is
-- often a deeply nested product type. This makes it highly conducive
-- to @Lens@ operations (as defined below).
--
data Lens record field
  = Lens
  { _get :: Getter record field
    -- ^ Retrieves a field from a record
  , _set :: Setter record field
    -- ^ Sets a field on a record
  }
----------------------------------------------------------------------------
-- | Type to express a getter on a @Lens@
type Getter record field = record -> field
----------------------------------------------------------------------------
-- | Type to express a setter on a @Lens@
type Setter record field = field -> record -> record
----------------------------------------------------------------------------
-- | van Laarhoven formulation, used for conversion w/ 'miso' @Lens@.
type Lens' s a = forall (f :: Type -> Type). Functor f => (a -> f a) -> s -> f s
----------------------------------------------------------------------------
-- | Convert from `miso` @Lens@ to van Laarhoven @Lens'@
toVL :: Lens record field -> Lens' record field
toVL Lens {..} = \f record -> flip _set record <$> f (_get record)
----------------------------------------------------------------------------
-- | Convert from `miso` @Lens@ to van Laarhoven @Lens'@
fromVL
  :: Lens' record field
  -> Lens record field
fromVL lens_ = Lens {..}
  where
    _get record = getConst (lens_ Const record)
    _set field = runIdentity . lens_ (\_ -> Identity field)
----------------------------------------------------------------------------
-- | Lens are Categories, and can therefore be composed.
instance Category Lens where
  id = Lens Prelude.id const
  Lens g1 s1 . Lens g2 s2 = Lens
    { _get = g1 <<< g2
    , _set = \f r -> s2 (s1 f (g2 r)) r
    }
----------------------------------------------------------------------------
-- | Set a field on a record
--
-- @
-- newtype Person = Person { _name :: String }
--
-- name :: Lens Person String
-- name = lens _name $ \\person n -> person { _name = n }
--
-- setName :: Person -> String -> Person
-- setName person newName = person & name .~ newName
-- @
infixr 4 .~
(.~) :: Lens record field -> field -> record -> record
(.~) _lens = _set _lens
----------------------------------------------------------------------------
-- | Synonym for '(.~)'
--
set :: Lens record field -> field -> record -> record
set = (.~)
----------------------------------------------------------------------------
-- | Set an options field on a record
--
-- @
-- newtype Person = Person { _name :: Maybe String }
--
-- name :: Lens Person (Maybe String)
-- name = lens _name $ \\person n -> person { _name = n }
--
-- setName :: Person -> String -> Person
-- setName person newName = person & name ?~ newName
-- @
infixr 4 ?~
(?~) :: Lens record (Maybe field) -> field -> record -> record
(?~) _lens f r = r & _lens .~ Just f
----------------------------------------------------------------------------
-- | Modify a field on a record by applying a function to it.
--
-- @
-- newtype Counter = Counter { _value :: Int }
--
-- value :: Lens Counter Int
-- value = lens _value $ \\counter v -> counter { _value = v }
--
-- increment :: Counter -> Counter
-- increment counter = counter & value %~ (+1)
-- @
infixr 4 %~
(%~) :: Lens record field -> (field -> field) -> record -> record
(%~) _lens f record = _set _lens (f (record ^. _lens)) record
----------------------------------------------------------------------------
-- | Synonym for '(%~)'
over :: Lens record field -> (field -> field) -> record -> record
over = (%~)
----------------------------------------------------------------------------
-- | Read a field from a record using a 'Lens'
--
-- @
-- newtype Person = Person { _name :: String }
--   deriving (Show, Eq)
--
-- name :: Lens Person String
-- name = lens _name $ \\person n -> person { _name = n }
--
-- getName :: Person -> String
-- getName = person ^. name
-- @
infixl 8 ^.
(^.) :: record -> Lens record field -> field
(^.) = flip _get
----------------------------------------------------------------------------
-- | Increment a @Num@eric field on a record using a @Lens@
--
-- @
-- newtype Person = Person { _age :: Int }
--
-- age :: Lens Person Int
-- age = lens _age $ \\person a -> person { _age = a }
--
-- birthday :: Person -> Person
-- birthday person = person & age +~ 1
-- @
infixr 4 +~
(+~) :: Num field => Lens record field -> field -> record -> record
(+~) _lens x record = record & _lens %~ (+x)
----------------------------------------------------------------------------
-- | Multiply a @Num@eric field on a record using a @Lens@
--
-- @
-- newtype Circle = Circle { _radius :: Int }
--
-- radius :: Lens Circle Int
-- radius = lens _radius $ \\circle r -> circle { _radius = r }
--
-- expand :: Circle -> Circle
-- expand circle = circle & radius *~ 10
-- @
infixr 4 *~
(*~) :: Num field => Lens record field -> field -> record -> record
(*~) _lens x record = record & _lens %~ (*x)
----------------------------------------------------------------------------
-- | Divide a @Fractional@ field on a record using a @Lens@
--
-- @
-- newtype Circle = Circle { _radius :: Int }
--
-- radius :: Lens Circle Int
-- radius = lens _radius $ \\circle r -> circle { _radius = r }
--
-- shrink :: Circle -> Circle
-- shrink circle = circle & radius //~ 10
-- @
infixr 4 //~
(//~) :: Fractional field => Lens record field -> field -> record -> record
(//~) _lens x record = record & _lens %~ (/x)
----------------------------------------------------------------------------
-- | Increment a @Num@eric field on a record using a @Lens@
--
-- @
-- newtype Person = Person { _age :: Int }
--
-- age :: Lens Person Int
-- age = lens _age $ \\person a -> person { _age = a }
--
-- timeTravel :: Person -> Person
-- timeTravel person = person & age -~ 1
-- @
infixr 4 -~
(-~) :: Num field => Lens record field -> field -> record -> record
(-~) _lens x record = record & _lens %~ subtract x
----------------------------------------------------------------------------
-- | Monoidally append a field in a record using a @Lens@
--
-- @
-- newtype List = List { _values :: [Int] }
--
-- values :: Lens List [Int]
-- values = lens _values $ \\l vs -> l { _values = vs }
--
-- addElement :: List -> List
-- addElement list = list & values <>~ [2]
--
-- addElement (List [])
-- -- List [2]
-- @
--
infixr 4 <>~
(<>~) :: Monoid field => Lens record field -> field -> record -> record
(<>~) _lens x record = record & _lens %~ (<> x)
----------------------------------------------------------------------------
-- | Execute a monadic action in @MonadState@ that returns a field. Sets the
-- return value equal to the field in the record.
--
-- As a reasonable mnemonic, this lets you store the result of a monadic action in a 'Lens' rather than
-- in a local variable.
--
-- @
-- do foo <- bar
--    ...
-- @
--
-- will store the result in a variable, while
--
-- @
-- do fooLens '<~' bar
--    ...
-- @
--
-- will store the result in field focused by the 'Lens'.
infixr 2 <~
(<~) :: MonadState record m => Lens record field -> m field -> m ()
l <~ mb = do
  b <- mb
  l .= b
----------------------------------------------------------------------------
-- | Modify a record in @MonadState@ monad at a field using a @Lens@
--
-- @
-- newtype Model = Model { _value :: Int }
--
-- data Action = AddOne | SubtractOne
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update AddOne = do
--   value %= (+1)
-- @
infix 4 %=
(%=) :: MonadState record m => Lens record field -> (field -> field) -> m ()
(%=) _lens f = modify (\r -> r & _lens %~ f)
----------------------------------------------------------------------------
-- | Synonym for '(%=)'
modifying :: MonadState record m => Lens record field -> (field -> field) -> m ()
modifying = (%=)
----------------------------------------------------------------------------
-- | Modify the field of a record in @MonadState@ using a @Lens@, then
-- return the newly modified field from the updated record.
--
-- @
-- import Miso.String (ms)
--
-- newtype Model = Model { _value :: Int }
--   deriving (Show)
--
-- data Action = AddOne
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update AddOne = do
--   result <- value <%= (+1)
--   io_ $ consoleLog (ms result)
-- @
infix 4 <%=
(<%=) :: MonadState record m => Lens record field -> (field -> field) -> m field
l <%= f = do
  l %= f
  use l
----------------------------------------------------------------------------
-- | Assign the field of a record in @MonadState@ to a value using a @Lens@
-- Return the value after assignment.
--
-- @
-- import Miso.String (ms)
--
-- newtype Model = Model { _value :: Int }
--
-- data Action = Assign Int
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (Assign x) = do
--   result <- value <.= x
--   io_ $ consoleLog (ms result) -- x
-- @
infix 4 <.=
(<.=) :: MonadState record m => Lens record field -> field -> m field
l <.= b = do
  l .= b
  return b
----------------------------------------------------------------------------
-- | Assign the field of a record in a @MonadState@ to a value (wrapped in a 'Just')
-- using a @Lens@. Return the value after assignment.
--
-- @
-- import Miso.String (ms)
--
-- newtype Model = Model { _value :: Maybe Int }
--
-- data Action = SetValue Int
--
-- value :: Lens Model (Maybe Int)
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (SetValue x) = do
--   result <- value <?= x
--   io_ $ consoleLog (ms result) -- Just 1
-- @
infix 4 <?=
(<?=) :: MonadState record m => Lens record (Maybe field) -> field -> m field
l <?= b = do
  l .= Just b
  return b
----------------------------------------------------------------------------
-- | Assign the field of a record in a @MonadState@ to a value using a @Lens@.
-- Returns the /previous/ value, before assignment.
--
-- @
-- import Miso.String (ms)
--
-- newtype Model = Model { _value :: Int }
--   deriving (Show, Eq)
--
-- data Action = Assign Int
--   deriving (Show, Eq)
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (Assign x) = do
--   value .= x
--   previousValue <- value <<.= 1
--   io_ $ consoleLog $ ms previousValue -- prints value at x
-- @
infix 4 <<.=
(<<.=) :: MonadState record m => Lens record field -> field -> m field
l <<.= b = do
  old <- use l
  l .= b
  return old
----------------------------------------------------------------------------
-- | Retrieves the field associated with a record in @MonadReader@ using a @Lens@.
--
-- @
-- import Miso.String (ms)
--
-- newtype Model = Model { _value :: Int }
--   deriving (Show, Eq)
--
-- data Action = PrintInt
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update PrintInt = do
--   Model x <- view value
--   io_ $ consoleLog (ms x) -- prints model value
-- @
----------------------------------------------------------------------------
view :: MonadReader record m => Lens record field -> m field
view lens_ = asks (^. lens_)
----------------------------------------------------------------------------
-- | Modifies the field of a record in @MonadState@ using a @Lens@.
-- Returns the /previous/ value, before modification.
--
-- @
-- import Miso.String (ms)
--
-- newtype Model = Model { _value :: Int }
--   deriving (Show, Eq)
--
-- data Action = Modify (Int -> Int)
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (Modify f) = do
--   value .= 2
--   result <- value <<%= f
--   io_ $ consoleLog (ms result) -- prints previous value of 2
-- @
infix 4 <<%=
(<<%=) :: MonadState record m => Lens record field -> (field -> field) -> m field
l <<%= f = do
  old <- use l
  l %= f
  return old
----------------------------------------------------------------------------
-- | Sets the value of a field in a record using @MonadState@ and a @Lens@
--
-- @
-- newtype Model = Model { _value :: Int }
--   deriving (Show, Eq)
--
-- data Action = SetValue Int
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update' :: Action -> Effect Model Action
-- update' (SetValue v) = value .= v
-- @
infix 4 .=
(.=) :: MonadState record m => Lens record field -> field -> m ()
(.=) _lens f = modify (\r -> r & _lens .~ f)
----------------------------------------------------------------------------
-- | Synonym for '(.=)'
assign :: MonadState record m => Lens record field -> field -> m ()
assign = (.=)
----------------------------------------------------------------------------
-- | Retrieves the value of a field in a record using a @Lens@ inside @MonadState@
--
-- @
-- import Miso.String (ms)
--
-- newtype Model = Model { _value :: Int }
--   deriving (Show, Eq)
--
-- data Action = SetValue Int
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (SetValue x) = do
--   value .= x
--   result <- use value
--   io_ $ consoleLog (ms result) -- prints the value of 'x'
-- @
use :: MonadState record m => Lens record field -> m field
use _lens = gets (^. _lens)
----------------------------------------------------------------------------
-- | Sets the value of a field in a record using a @Lens@ inside a @MonadState@
-- The value is wrapped in a @Just@ before being assigned.
--
-- @
-- newtype Model = Model { _value :: Maybe Int }
--   deriving (Show, Eq)
--
-- data Action = AssignValue Int
--
-- value :: Lens Model (Maybe Int)
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (AssignValue x) = value ?= x
-- @
infix 4 ?=
(?=) :: MonadState record m => Lens record (Maybe field) -> field -> m ()
(?=) _lens value = _lens .= Just value
----------------------------------------------------------------------------
-- | Increments the value of a @Num@eric field of a record using a @Lens@
-- inside a @State@ Monad.
--
-- @
-- newtype Model = Model { _value :: Int }
--   deriving (Show, Eq)
--
-- data Action = IncrementBy Int
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (IncrementBy x) = value += x
-- @
infix 4 +=
(+=) :: (MonadState record m, Num field)  => Lens record field -> field -> m ()
(+=) _lens f = modify (\r -> r & _lens +~ f)
----------------------------------------------------------------------------
-- | Multiplies the value of a @Num@eric field of a record using a @Lens@
-- inside a @State@ Monad.
--
-- @
-- newtype Model = Model { _value :: Int }
--   deriving (Show, Eq)
--
-- data Action = MultiplyBy Int
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (MultiplyBy x) = value *= x
-- @
infix 4 *=
(*=) :: (MonadState record m, Num field)  => Lens record field -> field -> m ()
(*=) _lens f = modify (\r -> r & _lens *~ f)
----------------------------------------------------------------------------
-- | Divides the value of a @Fractional@ field of a record using a @Lens@
-- inside a @State@ Monad.
--
-- @
-- newtype Model = Model { _value :: Double }
--   deriving (Show, Eq)
--
-- data Action = DivideBy Double
--
-- value :: Lens Model Double
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (DivideBy x) = value //= x
-- @
infix 4 //=
(//=) :: (MonadState record m, Fractional field)  => Lens record field -> field -> m ()
(//=) _lens f = modify (\r -> r & _lens %~ (/ f))
----------------------------------------------------------------------------
-- | Subtracts the value of a @Num@eric field of a record using a @Lens@
-- inside of a @State@ Monad.
--
-- @
-- newtype Model = Model { _value :: Double }
--   deriving (Show, Eq)
--
-- data Action = SubtractBy Double
--
-- value :: Lens Model Double
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect Model Action
-- update (SubtractBy x) = value -= x
-- @
infix 4 -=
(-=) :: (MonadState record m, Num field) => Lens record field -> field -> m ()
(-=) _lens f = modify (\r -> r & _lens -~ f)
---------------------------------------------------------------------------------
-- | @Lens@ that operates on the first element of a tuple
--
-- @
-- update AddOne = do
--   _1 += 1
-- @
_1 :: Lens (a,b) a
_1 = lens fst $ \(_,b) x -> (x,b)
---------------------------------------------------------------------------------
-- | @Lens@ that operates on the second element of a tuple
--
-- @
-- update AddOne = do
--   _2 += 1
-- @
_2 :: Lens (a,b) b
_2 = lens snd $ \(a,_) x -> (a,x)
---------------------------------------------------------------------------------
-- | @Lens@ that operates on itself
--
-- @
-- update AddOne = do
--   _id += 1
-- @
_id :: Lens a a
_id = Control.Category.id
---------------------------------------------------------------------------------
-- | @Lens@ that operates on itself
--
-- @
-- update AddOne = do
--   this += 1
-- @
this :: Lens a a
this = _id
---------------------------------------------------------------------------------
-- | Smart constructor @lens@ function. Used to easily construct a @Lens@
--
-- > name :: Lens Person String
-- > name = lens _name $ \p n -> p { _name = n }
--
lens
  :: (record -> field)
  -> (record -> field -> record)
  -> Lens record field
lens getter setter = Lens getter (flip setter)
----------------------------------------------------------------------------
data Prism s a
  = Prism
  { _up :: a -> s
  , _down :: s -> Maybe a
  }
----------------------------------------------------------------------------
review :: Prism s a -> a -> s
review = _up
----------------------------------------------------------------------------
preview :: Prism s a -> s -> Maybe a
preview = _down
----------------------------------------------------------------------------
_Left :: Prism (Either a b) a
_Left = prism Left $ either Just (const Nothing)
----------------------------------------------------------------------------
_Right :: Prism (Either a b) b
_Right = prism Right (either (const Nothing) Just)
----------------------------------------------------------------------------
_Just :: Prism (Maybe a) a
_Just = prism Just Prelude.id
----------------------------------------------------------------------------
_Nothing :: Prism (Maybe a) a
_Nothing = prism (const Nothing) Prelude.id
----------------------------------------------------------------------------
infixl 8 ^?
(^?) = flip preview
----------------------------------------------------------------------------
prism :: (a -> s) -> (s -> Maybe a) -> Prism s a
prism = Prism
----------------------------------------------------------------------------
