-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lens
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A simple t'Lens' formulation compatible with @lens@ and @microlens@.
--
-- The t'Lens' type is defined as:
--
-- @
-- data 'Lens' record field
--  = 'Lens'
--  { '_get' :: record -> field
--  , '_set' :: record -> field -> record
--  }
-- @
--
-- Key features:
--
-- * Provides an out-of-the-box lens experience with a minimal dependency footprint.
-- * Uses a simple formulation (not van Laarhoven) for smaller compilation payload.
-- * Import separately: @import Miso.Lens@.
-- * Works with the @Effect@ monad inside miso applications.
-- * Fixity and interface parity with @lens@ and @microlens@; replace
--   @import Miso.Lens@ with @import Control.Lens@ to switch seamlessly.
-- * Re-exports t'Lens'' for easy migration to
--   [lens](https://hackage.haskell.org/package/lens) or
--   [microlens](https://hackage.haskell.org/package/microlens).
--
-- For more on the van Laarhoven formulation, see the
-- [lens](https://hackage.haskell.org/package/lens) library.
--
-- === Example: Lenses for a nested record
--
-- @
-- data Person = Person
--   { _name :: String
--   , _address :: Address
--   , _age  :: Int
--   } deriving (Show, Eq, Generic)
--
-- newtype Address
--   = Address
--   { _zipCode :: Zip
--   } deriving (Show, Eq)
--
-- type Zip = String
--
-- name :: Lens Person String
-- name = 'lens' _name $ \\record x -> record { _name = x }
--
-- address :: Lens Person Address
-- address = 'lens' _address $ \\record x -> record { _address = x }
--
-- zipCode :: Lens Address Zip
-- zipCode = 'lens' _zipCode $ \\record x -> record { _zipCode = x }
--
-- -- Lenses compose via '.'
-- personZip :: Lens Person Zip
-- personZip = zipCode . address
--
-- main :: IO ()
-- main = print $ person '&' address '.~' Address "10012"
--   -- Person { _name = "john", _age = 33, _address = Address {_zipCode = "10012"} }
-- @
--
-- === Example: Usage with the @Effect@ monad
--
-- @
-- newtype Model = Model { _value :: Int }
--
-- value :: Lens Model Int
-- value = 'lens' _value $ \\model v -> model { _value = v }
--
-- data Action = AddOne | SubtractOne
--
-- updateModel :: Action -> Effect context props Model Action
-- updateModel = \\case
--   AddOne    -> value '+=' 1
--   SubtractOne -> value '-=' 1
-- @
----------------------------------------------------------------------------
module Miso.Lens
  ( -- ** Types
    Lens
  , LensCore (..)
  , Prism (..)
    -- ** Smart constructor
  , lens
  , prism
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
  , (%?=)
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
  , preview
  , preuse
  , review
  , _Nothing
  , _Just
  , _Left
  , _Right
  , (^?)
  -- *** Containers
  , At (..)
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
import Control.Arrow ((>>>))
import Data.Functor.Const (Const(..))
import Data.Function ((&))
import Data.Functor((<&>))
import Data.Kind (Type)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Prelude hiding ((.))
----------------------------------------------------------------------------
import Miso.Util (compose)
----------------------------------------------------------------------------
-- | A t'Lens' is a generalized getter and setter.
--
-- Lenses allow both the retrieval of values from fields in a record and the
-- assignment of values to fields in a record. The power of a t'Lens' comes
-- from its ability to be composed with other lenses.
--
-- In the context of building applications with miso, the @model@ is
-- often a deeply nested product type. This makes it highly conducive
-- to t'Lens' operations (as defined below).
--
type Lens s a = LensCore a s
----------------------------------------------------------------------------
-- | t'LensCore' is an internal type used to reverse composition like
-- VL libraries do.
data LensCore field record
  = Lens
  { _get :: record -> field
    -- ^ Retrieves a field from a record
  , _set :: field -> record -> record
    -- ^ Sets a field on a record
  }
----------------------------------------------------------------------------
-- | van Laarhoven formulation, used for conversion w/ 'Miso.miso' t'Lens'.
type Lens' s a = forall (f :: Type -> Type). Functor f => (a -> f a) -> s -> f s
----------------------------------------------------------------------------
-- | Convert a t'Lens' to a van Laarhoven t'Lens''
toVL :: Lens record field -> Lens' record field
toVL Lens {..} = \f record -> flip _set record <$> f (_get record)
----------------------------------------------------------------------------
-- | Convert a van Laarhoven t'Lens'' to a t'Lens'
fromVL
  :: Lens' record field
  -- ^ Van Laarhoven lens to convert
  -> Lens record field
fromVL lens_ = Lens {..}
  where
    _get record = getConst (lens_ Const record)
    _set field = runIdentity . lens_ (\_ -> Identity field)
----------------------------------------------------------------------------
-- | t'Lens' form a 'Category', and can therefore be composed.
instance Category LensCore where
  id = Lens Prelude.id const
  Lens g1 s1 . Lens g2 s2 = Lens
    { _get = g1 >>> g2
    , _set = \f r -> s1 (s2 f (g1 r)) r
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
-- | Read a field from a record using a t'Lens'
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
-- | Increment a @Num field => field@ on a record using a t'Lens'
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
-- | Multiply a @Num@eric field on a record using a t'Lens'
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
-- | Divide a @Fractional@ field on a record using a t'Lens'
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
-- | Increment a @Num@eric field on a record using a t'Lens'
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
-- | Monoidally append a field in a record using a t'Lens'
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
-- As a reasonable mnemonic, this lets you store the result of a monadic action in a t'Lens' rather than
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
-- will store the result in field focused by the t'Lens'.
infixr 2 <~
(<~) :: MonadState record m => Lens record field -> m field -> m ()
l <~ mb = do
  b <- mb
  l .= b
----------------------------------------------------------------------------
-- | Modify a record in @MonadState@ monad at a field using a t'Lens'
--
-- @
-- newtype Model = Model { _value :: Int }
--
-- data Action = AddOne | SubtractOne
--
-- value :: Lens Model Int
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect context props Model Action
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
-- | Modify the field of a record in @MonadState@ using a t'Lens', then
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
-- update :: Action -> Effect context props Model Action
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
-- | Assign the field of a record in @MonadState@ to a value using a t'Lens'
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
-- update :: Action -> Effect context props Model Action
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
-- using a t'Lens'. Return the value after assignment.
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
-- update :: Action -> Effect context props Model Action
-- update (SetValue x) = do
--   result <- value <?= x
--   io_ $ consoleLog (ms result) -- Just 1
-- @
infix 4 <?=
(<?=) :: MonadState record m => Lens record (Maybe field) -> field -> m field
l <?= b = do
  l ?= b
  return b
----------------------------------------------------------------------------
-- | Assign the field of a record in a @MonadState@ to a value using a t'Lens'.
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
-- update :: Action -> Effect context props Model Action
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
-- | Retrieves the field associated with a record in @MonadReader@ using a t'Lens'.
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
-- update :: Action -> Effect context props Model Action
-- update PrintInt = do
--   Model x <- view value
--   io_ $ consoleLog (ms x) -- prints model value
-- @
----------------------------------------------------------------------------
view :: MonadReader record m => Lens record field -> m field
view lens_ = asks (^. lens_)
----------------------------------------------------------------------------
-- | Modifies the field of a record in @MonadState@ using a t'Lens'.
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
-- update :: Action -> Effect context props Model Action
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
-- | Sets the value of a field in a record using @MonadState@ and a t'Lens'
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
-- update' :: Action -> Effect context props Model Action
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
-- | Retrieves the value of a field in a record using a t'Lens' inside @MonadState@
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
-- update :: Action -> Effect context props Model Action
-- update (SetValue x) = do
--   value .= x
--   result <- use value
--   io_ $ consoleLog (ms result) -- prints the value of x
-- @
use :: MonadState record m => Lens record field -> m field
use _lens = gets (^. _lens)
----------------------------------------------------------------------------
-- | Sets the value of a field in a record using a t'Lens' inside a @MonadState@
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
-- update :: Action -> Effect context props Model Action
-- update (AssignValue x) = value ?= x
-- @
infix 4 ?=
(?=) :: MonadState record m => Lens record (Maybe field) -> field -> m ()
(?=) _lens value = _lens .= Just value
----------------------------------------------------------------------------
-- | Alters the @Just@ value of a field in a record using a t'Lens' inside a @MonadState@
--
-- @
-- newtype Model = Model { _value :: Maybe Int }
--   deriving (Show, Eq)
--
-- data Action = IncrementIfJust
--
-- value :: Lens Model (Maybe Int)
-- value = lens _value $ \\p x -> p { _value = x }
--
-- update :: Action -> Effect context props Model Action
-- update IncrementIfJust = value %?= (+1)
--
-- @
infix 4 %?=
(%?=) :: MonadState record m => Lens record (Maybe field) -> (field -> field) -> m ()
(%?=) _lens f = _lens %= \case
  Nothing -> Nothing
  Just x -> Just (f x)
----------------------------------------------------------------------------
-- | Increments the value of a @Num@eric field of a record using a t'Lens'
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
-- update :: Action -> Effect context props Model Action
-- update (IncrementBy x) = value '+=' x
-- @
infix 4 +=
(+=) :: (MonadState record m, Num field)  => Lens record field -> field -> m ()
(+=) _lens f = modify (\r -> r & _lens +~ f)
----------------------------------------------------------------------------
-- | Multiplies the value of a @Num@eric field of a record using a t'Lens'
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
-- update :: Action -> Effect context props Model Action
-- update (MultiplyBy x) = value *= x
-- @
infix 4 *=
(*=) :: (MonadState record m, Num field)  => Lens record field -> field -> m ()
(*=) _lens f = modify (\r -> r & _lens *~ f)
----------------------------------------------------------------------------
-- | Divides the value of a @Fractional@ field of a record using a t'Lens'
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
-- update :: Action -> Effect context props Model Action
-- update (DivideBy x) = value //= x
-- @
infix 4 //=
(//=) :: (MonadState record m, Fractional field)  => Lens record field -> field -> m ()
(//=) _lens f = modify (\r -> r & _lens %~ (/ f))
----------------------------------------------------------------------------
-- | Subtracts the value of a @Num@eric field of a record using a t'Lens'
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
-- update :: Action -> Effect context props Model Action
-- update (SubtractBy x) = value -= x
-- @
infix 4 -=
(-=) :: (MonadState record m, Num field) => Lens record field -> field -> m ()
(-=) _lens f = modify (\r -> r & _lens -~ f)
---------------------------------------------------------------------------------
-- | t'Lens' that operates on the first element of a tuple
--
-- @
-- update AddOne = do
--   _1 'Miso.Lens.+=' 1
-- @
_1 :: Lens (a,b) a
_1 = lens fst $ \(_,b) x -> (x,b)
---------------------------------------------------------------------------------
-- | t'Lens' that operates on the second element of a tuple
--
-- @
-- update AddOne = do
--   _2 'Miso.Lens.+=' 1
-- @
_2 :: Lens (a,b) b
_2 = lens snd $ \(a,_) x -> (a,x)
---------------------------------------------------------------------------------
-- | t'Lens' that operates on itself
--
-- @
-- update AddOne = do
--   _id 'Miso.Lens.+=' 1
-- @
_id :: Lens a a
_id = Control.Category.id
---------------------------------------------------------------------------------
-- | t'Lens' that operates on itself
--
-- @
-- update AddOne = do
--   this 'Miso.Lens.+=' 1
-- @
this :: Lens a a
this = _id
---------------------------------------------------------------------------------
-- | Smart constructor 'lens' function. Used to easily construct a t'Lens'
--
-- > name :: 'Lens' Person String
-- > name = 'lens' _name $ \p n -> p { _name = n }
--
lens
  :: (record -> field)
  -- ^ Getter: read the field from a record
  -> (record -> field -> record)
  -- ^ Setter: write a new field value into a record
  -> Lens record field
lens getter setter = Lens getter (flip setter)
----------------------------------------------------------------------------
-- | A t'Prism' is a first-class reference into a sum type constructor.
--
-- 'Prism' values can be used with 'preview' to try to extract a value,
-- and with 'review' to embed a value back into the sum type.
data Prism s a
  = Prism
  { _up :: a -> s
  -- ^ Embed a value @a@ back into the sum type @s@ (the @review@ direction).
  , _down :: s -> Maybe a
  -- ^ Try to extract a value @a@ from @s@; 'Nothing' if the wrong constructor.
  }
----------------------------------------------------------------------------
-- | Embed a value into a sum type using a t'Prism'.
review :: Prism s a -> a -> s
review = _up
----------------------------------------------------------------------------
-- | Try to extract a value from a sum type using a t'Prism' inside 'MonadReader'.
preview :: MonadReader r m => Prism r a -> m (Maybe a)
preview = asks . preview
----------------------------------------------------------------------------
-- | Try to extract a value from a sum type using a t'Prism' inside 'MonadState'.
preuse :: MonadState s m => Prism s a -> m (Maybe a)
preuse = gets . preview
----------------------------------------------------------------------------
-- | t'Prism' for the 'Left' constructor of 'Either'.
_Left :: Prism (Either a b) a
_Left = prism Left $ either Just (const Nothing)
----------------------------------------------------------------------------
-- | t'Prism' for the 'Right' constructor of 'Either'.
_Right :: Prism (Either a b) b
_Right = prism Right (either (const Nothing) Just)
----------------------------------------------------------------------------
-- | t'Prism' for the 'Just' constructor of 'Maybe'.
_Just :: Prism (Maybe a) a
_Just = prism Just Prelude.id
----------------------------------------------------------------------------
-- | t'Prism' that matches a 'Nothing' value.
_Nothing :: Prism (Maybe a) a
_Nothing = prism (const Nothing) Prelude.id
----------------------------------------------------------------------------
-- | Infix alias for 'preview'. Try to extract a value using a t'Prism'.
--
-- @
-- Right 42 '^?' '_Right' == Just 42
-- Left  "x" '^?' '_Right' == Nothing
-- @
infixl 8 ^?
(^?) :: s -> Prism s a -> Maybe a
(^?) = flip preview
----------------------------------------------------------------------------
-- | Smart constructor for t'Prism'.
prism
  :: (a -> s)
  -- ^ Embed direction: construct @s@ from @a@.
  -> (s -> Maybe a)
  -- ^ Match direction: try to extract @a@ from @s@.
  -> Prism s a
prism = Prism
----------------------------------------------------------------------------
-- | Class for getting and setting values across various container types.
--
-- > M.singleton 'a' "foo" & at 'a' .~ Just "bar"
-- > -- fromList [('a',"bar")]
--
-- > update (SetValue value)
-- >   at 10 ?= value
--
-- @since 1.9.0.0
class At at where
  type family Index at :: Type
  -- ^ Index of the container
  type family IxValue at :: Type
  -- ^ Indexed value of the container
  at :: Index at -> Lens at (Maybe (IxValue at))
----------------------------------------------------------------------------
instance Ord k => At (Map k v) where
  type Index (Map k v) = k
  type IxValue (Map k v) = v
  at key = lens (M.lookup key) $ \m value ->
    case value of
      Nothing -> M.delete key m
      Just v -> M.insert key v m
----------------------------------------------------------------------------
instance At (IntMap v) where
  type Index (IntMap v) = Int
  type IxValue (IntMap v) = v
  at key = lens (IM.lookup key) $ \m value ->
    case value of
      Nothing -> IM.delete key m
      Just v -> IM.insert key v m
----------------------------------------------------------------------------
instance Ord k => At (Set k) where
  type Index (Set k) = k
  type IxValue (Set k) = ()
  at key = Lens {..}
    where
      _set = \v m ->
        case v of
          Nothing -> S.delete key m
          Just () -> S.insert key m
      _get m
        | S.member key m = Just ()
        | otherwise = Nothing
----------------------------------------------------------------------------
instance At IntSet where
  type Index IntSet = Int
  type IxValue IntSet = ()
  at key = Lens {..}
    where
      _set = \v m ->
        case v of
          Nothing -> IS.delete key m
          Just () -> IS.insert key m
      _get m
        | IS.member key m = Just ()
        | otherwise = Nothing
----------------------------------------------------------------------------
instance At [a] where
  type Index [a] = Int
  type IxValue [a] = a
  at key = Lens {..}
    where
      _set Nothing m
        | key < 0 = m
        | otherwise = splitAt key m & \(lhs, rhs) -> lhs <> drop 1 rhs
      _set (Just v) m
        | key < 0 = m
        | otherwise = splitAt key m & \(lhs, rhs) ->
            case rhs of
              [] -> lhs
              _ : xs -> lhs <> (v : xs)
      _get = lookup key . zip [0..]
----------------------------------------------------------------------------
