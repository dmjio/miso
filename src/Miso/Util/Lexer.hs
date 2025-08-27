-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Util.Lexer
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Util.Lexer
  ( -- ** Types
    Lexer (..)
  , Stream (..)
  , Located (..)
  , Location (..)
  , LexerError (..)
    -- ** Combinators
  , getStartColumn
  , zeroLocation
  , initialLocation
  , mkStream
  , oops
  , streamError
  , string
  , string'
  , char
  , satisfy
  , peek
  , getInput
  , putInput
  , getLocation
  , setLocation
  , modifyInput
  , withLocation
  ) where
----------------------------------------------------------------------------
import           Control.Monad
import           Control.Applicative
----------------------------------------------------------------------------
import           Miso.String (MisoString, ToMisoString)
import qualified Miso.String as MS
----------------------------------------------------------------------------
data LexerError
  = LexerError MisoString Location
  | UnexpectedEOF Location
  deriving (Eq)
----------------------------------------------------------------------------
instance Show LexerError where
  show (UnexpectedEOF loc) =
    "Unexpected EOF at: " <> show loc
  show (LexerError xs loc) =
    "Unexpected \"" <> take 5 (MS.unpack xs) <> "\"... at " <> show loc
----------------------------------------------------------------------------
data Location
  = Location
  { line :: Int
  , column :: (Int,Int)
  } deriving Eq
----------------------------------------------------------------------------
instance Show Location where
  show (Location l col) = show l <> " " <> show col
----------------------------------------------------------------------------
getStartColumn :: Location -> Int
getStartColumn = fst . column
----------------------------------------------------------------------------
initialLocation :: Location
initialLocation = Location 1 (1,1)
----------------------------------------------------------------------------
zeroLocation :: Location
zeroLocation = Location 0 (0,0)
----------------------------------------------------------------------------
newtype Lexer token
  = Lexer
  { runLexer :: Stream -> Either LexerError (token, Stream)
  }
----------------------------------------------------------------------------
oops :: Lexer token
oops = Lexer $ \s -> Left (streamError s)
----------------------------------------------------------------------------
streamError :: Stream -> LexerError
streamError (Stream xs l) = unexpected xs l
----------------------------------------------------------------------------
mkStream :: MisoString -> Stream
mkStream xs = Stream xs initialLocation
----------------------------------------------------------------------------
data Stream
  = Stream
  { stream :: MisoString
  , currentLocation :: Location
  } deriving Eq
----------------------------------------------------------------------------
data Located token
  = Located
  { token :: token
  , location :: Location
  } deriving Eq
----------------------------------------------------------------------------
instance Show token => Show (Located token) where
  show (Located t l) = show l <> " " <> show t
----------------------------------------------------------------------------
instance Functor Lexer where
  fmap f (Lexer l) = Lexer $ \input -> do
    (t, x) <- l input
    pure (f t, x)
----------------------------------------------------------------------------
instance Applicative Lexer where
  pure x = Lexer $ \input -> pure (x, input)
  Lexer l1 <*> Lexer l2 = Lexer $ \input -> do
    (f, x) <- l1 input
    (a, y) <- l2 x
    pure (f a, y)
----------------------------------------------------------------------------
instance Monad Lexer where
  m >>= f = Lexer $ \input -> do
    (x, s) <- runLexer m input
    runLexer (f x) s
----------------------------------------------------------------------------
instance Alternative Lexer where
  empty = Lexer $ \(Stream s l)  -> Left (unexpected s l)
  Lexer l1 <|> Lexer l2 = Lexer $ \input ->
    case (l1 input, l2 input) of
      (res, Left _) -> res
      (Left _, res) -> res
      (Right (x, Stream s sl), Right (y,Stream t tl)) ->
        if MS.length s <= MS.length t
        then Right (x, Stream s sl)
        else Right (y, Stream t tl)
----------------------------------------------------------------------------
instance MonadPlus Lexer where
  mplus = (<|>)
----------------------------------------------------------------------------
peek :: Lexer (Maybe Char)
peek = Lexer $ \ys ->
  pure $ case ys of
    Stream xs l ->
      case MS.uncons xs of
        Nothing -> (Nothing, Stream mempty l)
        Just (z,zs) -> (Just z, Stream (MS.singleton z <> zs) l)
----------------------------------------------------------------------------
satisfy :: (Char -> Bool) -> Lexer Char
satisfy predicate = Lexer $ \ys ->
  case ys of
    Stream s l ->
      case MS.uncons s of
        Nothing -> Left (unexpected s l)
        Just (z,zs)
          | predicate z -> Right (z, Stream zs l)
          | otherwise -> Left (unexpected zs l)
----------------------------------------------------------------------------
unexpected :: MisoString -> Location -> LexerError
unexpected xs loc | MS.null xs = UnexpectedEOF loc
unexpected cs loc = LexerError cs loc
----------------------------------------------------------------------------
getInput :: Lexer Stream
getInput = Lexer $ \s -> Right (s, s)
----------------------------------------------------------------------------
putInput :: Stream -> Lexer ()
putInput s = Lexer $ \_ -> Right ((), s)
----------------------------------------------------------------------------
getLocation :: Lexer Location
getLocation = Lexer $ \(Stream s l) -> pure (l, Stream s l)
----------------------------------------------------------------------------
setLocation :: Location -> Lexer ()
setLocation l = Lexer $ \(Stream s _) -> pure ((), Stream s l)
----------------------------------------------------------------------------
modifyInput :: (Stream -> Stream) -> Lexer ()
modifyInput f = do
  s <- getInput
  putInput (f s)
----------------------------------------------------------------------------
char :: Char -> Lexer Char
char c = satisfy (== c)
----------------------------------------------------------------------------
string' :: String -> Lexer String
string' = traverse char
----------------------------------------------------------------------------
string :: MisoString -> Lexer MisoString
string prefix = Lexer $ \s ->
  case s of
    Stream ys l
      | prefix `MS.isPrefixOf` ys ->
          Right (prefix, Stream (MS.drop (MS.length prefix) ys) l)
      | otherwise ->
          Left (unexpected ys l)
----------------------------------------------------------------------------
withLocation :: ToMisoString token => Lexer token -> Lexer (Located token)
withLocation lexer = do
  result <- lexer
  let
    adjustLoc :: Location -> MisoString -> Location
    adjustLoc l = MS.foldl' adjust (next l)

  setLocation =<< adjustLoc <$> getLocation <*> pure (MS.ms result)
  Located result <$> getLocation
    where
      next :: Location -> Location
      next (Location l (_, end)) = Location l (end, end)

      adjust :: Location -> Char -> Location
      adjust (Location l (_, _)) '\n'       = Location (l + 1) (1,1)
      adjust (Location l (start, end)) '\t' = Location l (start, end + 8)
      adjust (Location l (start, end))   _  = Location l (start, end + 1)
----------------------------------------------------------------------------
