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
  , Lens'
    -- ** Smart constructor
  , lens
    -- ** Utils
  , (<&>)
    -- ** Re-exports
  , (&)
    -- ** Combinators
  , (.~)
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
  , (?=)
  , (<>~)
  ) where
----------------------------------------------------------------------------
import Control.Monad.State (MonadState, modify, get)
import Control.Category (Category (..))
import Control.Arrow ((<<<))
import Data.Function ((&))
----------------------------------------------------------------------------
-- |
-- Exposes a simple @Lens@ abstraction that is compatible with other lens libraries.
-- This provides users an out-of-the box lens experience w/o the large dependency
-- footprint and cognitive load. Includes support for @MonadState@ so combinators
-- can be used with @Effect@. Example usage below.
--
-- This library is at fixity and interface parity with lens and microlens
-- and can therefore be used interchangeably with them.
--
-- > -- Person type
-- > data Person = Person
-- >   { _name :: String
-- >   , _address :: Address
-- >   , _age  :: Int
-- >   } deriving (Show, Eq, Generic)
--
-- > -- Address type
-- > newtype Address
-- >   = Address
-- >   { _zipCode :: Zip
-- >   } deriving (Show, Eq)
--
-- > -- | Zip code type synonym
-- > type Zip = String
--
-- > -- | Name Lens
-- > name :: Lens Person String
-- > name = lens _name $ \record x -> record { _name = x }
--
-- > -- | Address Lens
-- > address :: Lens Person Address
-- > address = lens _address $ \record x -> record { _address = x }
--
-- > -- | Zip Code Lens
-- > zipCode :: Lens Address Zip
-- > zipCode = lens _zipCode $ \record x -> record { _zipCode = x }
--
-- > -- | Lens Composition example
-- > personZip :: Lens Person Zip
-- > personZip = zipCode . address
--
-- > -- | Person example
-- > person :: Person
-- > person = Person "john" (Address "90210") 33
--
-- > main :: IO ()
-- > main = do
-- >   print $ john & address .~ Address "10012"
-- >   print $ john & zipCode . address %~ (++"!")
-- >   print $ john ^. address
--
-- > -- Person
-- > --  { _name = "john"
-- > --  , _age = 33
-- > --  , _address = Address {_zipCode = "10012"}
-- > --  }
--
-- > -- Example usage in miso's @Effect@ @Monad@
--
-- > newtype Model = Model { _value :: Int }
--
-- > value :: Lens Model Int
-- > value = lens _value $ \model v -> model { _value = v }
--
-- > data Action = AddOne | SubtractOne | SayHelloWorld
--
-- > updateModel :: Action -> Effect Model Action ()
-- > updateModel (AddOne event) = do
-- >   value += 1
-- >   io $ consoleLog (ms (show event))
-- > updateModel (SubtractOne event) = do
-- >   value -= 1
-- >   io $ consoleLog (ms (show event))
-- > updateModel SayHelloWorld =
-- >   io (consoleLog "Hello World!")
--
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
  { _get :: record -> field
    -- ^ Retrieves a field from a record
  , _set :: record -> field -> record
    -- ^ Sets a field on a record
  }
----------------------------------------------------------------------------
-- | Type synonym re-export for @lens@ / @microlens@ compatability.
-- Note: use this if you plan on migrating to lens or microlens eventually.
-- Just use @Lens@ otherwise (as examples show).
type Lens' record field = Lens record field
----------------------------------------------------------------------------
-- | Lens are Categories, and can therefore be composed.
instance Category Lens where
  id = Lens (\x -> x) (\x _ -> x)
  Lens g1 s1 . Lens g2 s2 = Lens
    { _get = g1 <<< g2
    , _set = \r f -> s2 r (s1 (g2 r) f)
    }
----------------------------------------------------------------------------
-- | Set a field on a record
--
-- > newtype Person = Person { _name :: String }
--
-- > name :: Lens Person String
-- > name = lens _name $ \person n -> person { _name = n }
--
-- > setName :: Person -> String -> Person
-- > setName person newName = person & name .~ newName
--
infixr 4 .~
(.~) :: Lens record field -> field -> record -> record
(.~) _lens = flip (_set _lens)
----------------------------------------------------------------------------
-- | Synonym for @(.~)@
--
set :: Lens record field -> field -> record -> record
set = (.~)
----------------------------------------------------------------------------
-- | Modify a field on a record
--
-- > \x -> record & field %~ f x
--
infixr 4 %~
(%~) :: Lens record a -> (a -> a) -> record -> record
(%~) _lens f record = _set _lens record $ f (record ^. _lens)
----------------------------------------------------------------------------
-- | Synonym for (%~)
over :: Lens record a -> (a -> a) -> record -> record
over = (%~)
----------------------------------------------------------------------------
-- | Read a field from a record using a @Lens@
--
-- > newtype Person = Person { _name :: String }
-- >   deriving (Show, Eq)
--
-- > name :: Lens Person String
-- > name = lens _name $ \person n -> person { _name = n }
--
-- > getName :: Person -> String
-- > getName = person ^. name
--
infixl 8 ^.
(^.) :: record -> Lens record field -> field
(^.) = flip _get
----------------------------------------------------------------------------
-- | Increment a @Num@eric field on a record using a @Lens@
--
-- > newtype Person = Person { _age :: Int }
--
-- > age :: Lens Person Int
-- > age = lens _age $ \person a -> person { _age = a }
--
-- > birthday :: Person -> Person
-- > birthday person = person & age +~ 1
--
infixr 4 +~
(+~) :: Num field => Lens record field -> field -> record -> record
(+~) _lens x record = record & _lens %~ (+x)
----------------------------------------------------------------------------
-- | Multiply a @Num@eric field on a record using a @Lens@
--
-- > newtype Circle = Circle { _radius :: Int }
--
-- > radius :: Lens Circle Int
-- > radius = lens _radius $ \circle r -> circle { _radius = r }
--
-- > expand :: Circle -> Circle
-- > expand circle = circle & radius *~ 10
--
infixr 4 *~
(*~) :: Num field => Lens record field -> field -> record -> record
(*~) _lens x record = record & _lens %~ (*x)
----------------------------------------------------------------------------
-- | Divide a @Fractional@ field on a record using a @Lens@
--
-- > newtype Circle = Circle { _radius :: Int }
--
-- > radius :: Lens Circle Int
-- > radius = lens _radius $ \circle r -> circle { _radius = r }
--
-- > expand :: Circle -> Circle
-- > expand circle = circle & radius *~ 10
--
infixr 4 //~
(//~) :: Fractional field => Lens record field -> field -> record -> record
(//~) _lens x record = record & _lens %~ (/x)
----------------------------------------------------------------------------
-- | Increment a @Num@eric field on a record using a @Lens@
--
-- > newtype Person = Person { _age :: Int }
--
-- > age :: Lens Person Int
-- > age = lens _age $ \person a -> person { _age = a }
--
-- > timeTravel :: Person -> Person
-- > timeTravel person = person & age -~ 1
--
infixr 4 -~
(-~) :: Num field => Lens record field -> field -> record -> record
(-~) _lens x record = record & _lens %~ subtract x
----------------------------------------------------------------------------
-- | Monoidally append a field in a record using a @Lens@
--
-- > newtype List = List { _values :: [Int] }
--
-- > values :: Lens List [Int]
-- > values = lens _values $ \l vs -> l { _values = vs }
--
-- > addElement :: List -> List
-- > addElement list = list & values <>~ [2]
--
-- > addElement (List [])
-- > -- List [2]
--
infixr 4 <>~
(<>~) :: Monoid field => Lens record field -> field -> record -> record
(<>~) _lens x record = record & _lens %~ (<> x)
----------------------------------------------------------------------------
-- | Execute a monadic action in @MonadState@ that returns a field. Sets the
-- return value equal to the field in the record.
--
-- > newtype List = List { _values :: [Int] }
--
-- > values :: Lens List [Int]
-- > values = lens _values $ \l vs -> l { _values = vs }
-- >
-- > addElement :: List -> List
-- > addElement list = list & values <>~ [2]
--
infixr 2 <~
(<~) :: MonadState record m => Lens record field -> m field -> m ()
l <~ mb = do
  b <- mb
  l .= b
----------------------------------------------------------------------------
-- | Modify a record in @MonadState@ monad at a field using a @Lens@
--
-- > newtype Model = Model { _value :: Int }
--
-- > data Action = AddOne | SubtractOne
--
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
--
-- > update :: Action -> Effect Model Action ()
-- > update AddOne = do
-- >   value %= (+1)
--
infix 4 %=
(%=) :: MonadState record m => Lens record field -> (field -> field) -> m ()
(%=) _lens f = modify (\r -> r & _lens %~ f)
----------------------------------------------------------------------------
-- | Synonym for (%=)
modifying :: MonadState record m => Lens record field -> (field -> field) -> m ()
modifying = (%=)
----------------------------------------------------------------------------
-- | Modify the field of a record in @MonadState@ using a @Lens@, then
-- return the newly modified field from the updated record.
--
-- > import Miso.String (ms)
--
-- > newtype Model = Model { _value :: Int }
-- >   deriving (Show)
--
-- > data Action = AddOne
--
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
--
-- > update :: Action -> Effect Model Action ()
-- > update AddOne = do
-- >   result <- value <%= (+1)
-- >   io $ consoleLog (ms result)
--
infix 4 <%=
(<%=) :: MonadState record m => Lens record b -> (b -> b) -> m b
l <%= f = do
  l %= f
  use l
----------------------------------------------------------------------------
-- | Assign the field of a record in @MonadState@ to a value using a @Lens@
-- Return the value after assignment.
--
-- > import Miso.String (ms)
--
-- > newtype Model = Model { _value :: Int }
--
-- > data Action = Assign Int
--
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
--
-- > update :: Action -> Effect Model Action ()
-- > update (Assign x) = do
-- >   result <- value <.= x
-- >   io $ consoleLog (ms result) -- x
--
infix 4 <.=
(<.=) :: MonadState record m => Lens record b -> b -> m b
l <.= b = do
  l .= b
  return b
----------------------------------------------------------------------------
-- | Assign the field of a record in a @MonadState@ to a value (wrapped in a 'Just')
-- using a @Lens@. Return the value after assignment.
--
-- > import Miso.String (ms)
--
-- > newtype Model = Model { _value :: Maybe Int }
--
-- > data Action = SetValue Int
--
-- > value :: Lens Model (Maybe Int)
-- > value = lens _value $ \p x -> p { _value = x }
--
-- > update :: Action -> Effect Model Action ()
-- > update (SetValue x) = do
-- >   result <- value <?= x
-- >   io $ consoleLog (ms result) -- Just 1
--
infix 4 <?=
(<?=) :: MonadState record m => Lens record (Maybe b) -> b -> m b
l <?= b = do
  l .= Just b
  return b
----------------------------------------------------------------------------
-- | Assign the field of a record in a @MonadState@ to a value using a @Lens@.
-- Returns the /previous/ value, before assignment.
--
-- > import Miso.String (ms)
--
-- > newtype Model = Model { _value :: Int }
-- >   deriving (Show, Eq)
--
-- > data Action = Assign Int
-- >   deriving (Show, Eq)
--
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
--
-- > update :: Action -> Effect Model Action ()
-- > update (Assign x) = do
-- >   value .= x
-- >   previousValue <- value <<.= 1
-- >   io $ consoleLog $ ms (show previousValue) -- prints value at x
--
infix 4 <<.=
(<<.=) :: MonadState record m => Lens record b -> b -> m b
l <<.= b = do
  old <- use l
  l .= b
  return old
----------------------------------------------------------------------------
-- | Modifies the field of a record in @MonadState@ using a @Lens@.
-- Returns the /previous/ value, before modification.
--
-- > import Miso.String (ms)
--
-- > newtype Model = Model { _value :: Int }
-- >   deriving (Show, Eq)
-- 
-- > data Action = Modify (Int -> Int)
-- 
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
-- 
-- > update :: Action -> Effect Model Action ()
-- > update (Modify f) = do
-- >   value .= 2
-- >   result <- value <<%= f
-- >   io $ consoleLog (ms (show result)) -- prints previous value of 2
--
infix 4 <<%=
(<<%=) :: MonadState record m => Lens record b -> (b -> b) -> m b
l <<%= f = do
  old <- use l
  l %= f
  return old
----------------------------------------------------------------------------
-- | Sets the value of a field in a record using @MonadState@ and a @Lens@
--
-- > import Miso.String (ms)
--
-- > newtype Model = Model { _value :: Int }
-- >   deriving (Show, Eq)
-- 
-- > data Action = SetValue Int
-- 
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
-- 
-- > update' :: Action -> Effect Model Action ()
-- > update' (SetValue v) = value .= v
--
infix 4 .=
(.=) :: MonadState record m => Lens record field -> field -> m ()
(.=) _lens f = modify (\r -> r & _lens .~ f)
----------------------------------------------------------------------------
-- | Synonym for (.=)
assign :: MonadState record m => Lens record field -> field -> m ()
assign = (.=)
----------------------------------------------------------------------------
-- | Retrieves the value of a field in a record using a @Lens@ inside @MonadState@
--
-- > import Miso.String (ms)
--
-- > newtype Model = Model { _value :: Int }
-- >   deriving (Show, Eq)
-- 
-- > data Action = SetValue Int
-- 
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
-- 
-- > update :: Action -> Effect Model Action ()
-- > update (SetValue x) = do
-- >   value .= x
-- >   result <- use value
-- >   io $ consoleLog (ms (show result)) -- prints the value of 'x'
--
use :: MonadState record m => Lens record field -> m field
use _lens = (^. _lens) <$> get
----------------------------------------------------------------------------
-- | Sets the value of a field in a record using a @Lens@ inside a @MonadState@
-- The value is wrapped in a @Just@ before being assigned.
--
-- > newtype Model = Model { _value :: Maybe Int }
-- >   deriving (Show, Eq)
-- 
-- > data Action = AssignValue Int
-- 
-- > value :: Lens Model (Maybe Int)
-- > value = lens _value $ \p x -> p { _value = x }
-- 
-- > update :: Action -> Effect Model Action ()
-- > update (AssignValue x) = value ?= x
--
infix 4 ?=
(?=) :: MonadState record m => Lens record (Maybe field) -> field -> m ()
(?=) _lens value = _lens .= Just value
----------------------------------------------------------------------------
-- | Increments the value of a @Num@eric field of a record using a @Lens@
-- inside a @State@ Monad.
--
-- > newtype Model = Model { _value :: Int }
-- >   deriving (Show, Eq)
-- 
-- > data Action = IncrementBy Int
-- 
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
-- 
-- > update :: Action -> Effect Model Action ()
-- > update (IncrementBy x) = value += x
--
infix 4 +=
(+=) :: (MonadState record m, Num field)  => Lens record field -> field -> m ()
(+=) _lens f = modify (\r -> r & _lens +~ f)
----------------------------------------------------------------------------
-- | Multiplies the value of a @Num@eric field of a record using a @Lens@
-- inside a @State@ Monad.
--
-- > newtype Model = Model { _value :: Int }
-- >   deriving (Show, Eq)
-- 
-- > data Action = MultiplyBy Int
-- 
-- > value :: Lens Model Int
-- > value = lens _value $ \p x -> p { _value = x }
-- 
-- > update :: Action -> Effect Model Action ()
-- > update (MultiplyBy x) = value *= x
--
infix 4 *=
(*=) :: (MonadState record m, Num field)  => Lens record field -> field -> m ()
(*=) _lens f = modify (\r -> r & _lens *~ f)
----------------------------------------------------------------------------
-- | Divides the value of a @Fractional@ field of a record using a @Lens@
-- inside a @State@ Monad.
--
-- > newtype Model = Model { _value :: Double }
-- >   deriving (Show, Eq)
-- 
-- > data Action = DivideBy Double
-- 
-- > value :: Lens Model Double
-- > value = lens _value $ \p x -> p { _value = x }
-- 
-- > update :: Action -> Effect Model Action ()
-- > update (DivideBy x) = value //= x
--
infix 4 //=
(//=) :: (MonadState record m, Fractional field)  => Lens record field -> field -> m ()
(//=) _lens f = modify (\r -> r & _lens %~ (/ f))
----------------------------------------------------------------------------
-- | Subtracts the value of a @Num@eric field of a record using a @Lens@
-- inside of a @State@ Monad.
--
-- > newtype Model = Model { _value :: Double }
-- >   deriving (Show, Eq)
-- 
-- > data Action = SubtractBy Double
--
-- > value :: Lens Model Double
-- > value = lens _value $ \p x -> p { _value = x }
-- 
-- > update :: Action -> Effect Model Action ()
-- > update (SubtractBy x) = value -= x
--
infix 4 -=
(-=) :: (MonadState record m, Num field) => Lens record field -> field -> m ()
(-=) _lens f = modify (\r -> r & _lens -~ f)
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
lens = Lens
----------------------------------------------------------------------------
-- | Functor utility, a flipped infix @fmap@.
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
f <&> x = x <$> f
----------------------------------------------------------------------------
