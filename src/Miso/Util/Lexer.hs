{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Util.Lexer
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Util.Lexer" is an internal lexer combinator library used by
-- miso's JSON pipeline ("Miso.JSON.Lexer") and URI\/router tokeniser
-- ("Miso.Router"). It is __not__ designed for general application use,
-- but is exposed for downstream code that needs to build custom lexers.
--
-- The central type is 'Lexer':
--
-- @
-- newtype 'Lexer' token = Lexer
--   { 'runLexer' :: 'Stream' -> Either 'LexerError' (token, 'Stream') }
-- @
--
-- 'Lexer' is a 'Monad', 'Alternative', and 'MonadFail'. Its
-- 'Control.Applicative.Alternative' instance implements the
-- /maximal munch/ rule: when both branches succeed, the one that
-- consumes the most input wins.
--
-- = Key types
--
-- * 'Stream' — the remaining input text together with a 'Location'
--   (line and column) cursor.
-- * 'Location' — @{line :: Int, column :: (Int, Int)}@, used in error
--   messages and 'Located' tokens.
-- * 'Located' token — a lexed value paired with the 'Location' at which
--   it was recognised.
-- * 'LexerError' — either @LexerError MisoString Location@ (unexpected
--   input) or @UnexpectedEOF Location@ (ran out of input).
--
-- = Primitive combinators
--
-- @
-- 'satisfy'  :: (Char -> Bool) -> 'Lexer' Char   -- consume one matching char
-- 'char'     :: Char -> 'Lexer' Char              -- consume a specific char
-- 'string'   :: 'Miso.String.MisoString' -> 'Lexer' 'Miso.String.MisoString'   -- consume a literal prefix
-- 'string''  :: String -> 'Lexer' String          -- same for 'String'
-- 'peek'     :: 'Lexer' (Maybe Char)              -- look ahead without consuming
-- 'oops'     :: 'Lexer' token                     -- always fails
-- @
--
-- = Stream and location access
--
-- @
-- 'getInput'     :: 'Lexer' 'Stream'
-- 'putInput'     :: 'Stream' -> 'Lexer' ()
-- 'modifyInput'  :: ('Stream' -> 'Stream') -> 'Lexer' ()
-- 'getLocation'  :: 'Lexer' 'Location'
-- 'setLocation'  :: 'Location' -> 'Lexer' ()
-- @
--
-- = Running a lexer
--
-- @
-- 'runLexer' :: 'Lexer' token -> 'Stream' -> Either 'LexerError' (token, 'Stream')
-- 'mkStream' :: 'Miso.String.MisoString' -> 'Stream'   -- create initial stream
-- @
--
-- = See also
--
-- * "Miso.Util.Parser" — parser combinator library that consumes 'Lexer' output
-- * "Miso.JSON.Lexer" — JSON tokeniser built on this module
-- * "Miso.Router" — URI tokeniser built on this module
-- * "Miso.Util" — higher-level 'Miso.Util.sepBy', 'Miso.Util.oneOf', …
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
#if __GLASGOW_HASKELL__ <= 865
import           Control.Monad.Fail
#endif
import           Control.Applicative
----------------------------------------------------------------------------
import           Miso.String (MisoString, ToMisoString)
import qualified Miso.String as MS
----------------------------------------------------------------------------
-- | Potential errors during lexing
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
-- | Type to hold the location (line and column) of a Token
data Location
  = Location
  { line :: Int
  -- ^ Current line number (1-based)
  , column :: (Int,Int)
  -- ^ @(start, end)@ column offsets for the current token (1-based)
  } deriving Eq
----------------------------------------------------------------------------
instance Show Location where
  show (Location l col) = show l <> " " <> show col
----------------------------------------------------------------------------
-- | Helper for extracting column from t'Location'
getStartColumn :: Location -> Int
getStartColumn = fst . column
----------------------------------------------------------------------------
-- | Initial t'Location'
initialLocation :: Location
initialLocation = Location 1 (1,1)
----------------------------------------------------------------------------
-- | Empty t'Location'
zeroLocation :: Location
zeroLocation = Location 0 (0,0)
----------------------------------------------------------------------------
-- | A Lexer is a state monad with optional failure the abides by the
-- maximal munch rule in its 'Alternative' instance.
newtype Lexer token
  = Lexer
  { runLexer :: Stream -> Either LexerError (token, Stream)
  -- ^ Run the lexer against a 'Stream'; returns the token and remaining input, or a 'LexerError'
  }
----------------------------------------------------------------------------
-- | Combinator that always fails to lex
oops :: Lexer token
oops = Lexer $ \s -> Left (streamError s)
----------------------------------------------------------------------------
-- | Smart constructor for t'LexerError'
streamError
  :: Stream
  -- ^ The stream at the point of failure; used to populate the error location
  -> LexerError
streamError (Stream xs l) = unexpected xs l
----------------------------------------------------------------------------
-- | Smart constructor for t'Stream'
mkStream
  :: MisoString
  -- ^ Input text to lex
  -> Stream
mkStream xs = Stream xs initialLocation
----------------------------------------------------------------------------
-- | A t'Stream' of text used as input to lexing
data Stream
  = Stream
  { stream :: MisoString
    -- ^ Current t'Stream' of text
  , currentLocation :: Location
    -- ^ current t'Location' in the t'Stream'
  } deriving Eq
----------------------------------------------------------------------------
-- | A t'Located' token holds the lexed output the t'Location' at which
-- the successful lex occurred.
data Located token
  = Located
  { token :: token
  -- ^ The lexed token value
  , location :: Location
  -- ^ 'Location' in the source at which this token was recognised
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
instance MonadFail Lexer where
  fail _ = oops
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
-- | Fetches the first character in the t'Stream', does not consume input
peek :: Lexer (Maybe Char)
peek = Lexer $ \ys ->
  pure $ case ys of
    Stream xs l ->
      case MS.uncons xs of
        Nothing -> (Nothing, Stream mempty l)
        Just (z,zs) -> (Just z, Stream (MS.singleton z <> zs) l)
----------------------------------------------------------------------------
-- | Predicate combinator that consumes matching input
satisfy
  :: (Char -> Bool)
  -- ^ Predicate; the next character is consumed only if this returns 'True'
  -> Lexer Char
satisfy predicate = Lexer $ \ys ->
  case ys of
    Stream s l ->
      case MS.uncons s of
        Nothing -> Left (unexpected s l)
        Just (z,zs)
          | predicate z -> Right (z, Stream zs l)
          | otherwise -> Left (unexpected zs l)
----------------------------------------------------------------------------
-- | Smart constructor for t'LexerError'
-- If the input is empty, an 'UnexpectedEOF' is issued.
unexpected :: MisoString -> Location -> LexerError
unexpected xs loc | MS.null xs = UnexpectedEOF loc
unexpected cs loc = LexerError cs loc
----------------------------------------------------------------------------
-- | Retrieves current input from t'Lexer'
getInput :: Lexer Stream
getInput = Lexer $ \s -> Right (s, s)
----------------------------------------------------------------------------
-- | Overrides current t'Stream' in t'Lexer' to user-specified t'Stream'.
putInput
  :: Stream
  -- ^ Replacement stream; replaces the current lexer input
  -> Lexer ()
putInput s = Lexer $ \_ -> Right ((), s)
----------------------------------------------------------------------------
-- | Retrieves the current t'Stream' t'Location'
getLocation :: Lexer Location
getLocation = Lexer $ \(Stream s l) -> pure (l, Stream s l)
----------------------------------------------------------------------------
-- | Sets the current t'Stream' t'Location'
setLocation
  :: Location
  -- ^ New location to record in the stream cursor
  -> Lexer ()
setLocation l = Lexer $ \(Stream s _) -> pure ((), Stream s l)
----------------------------------------------------------------------------
-- | Modifies a t'Stream'
modifyInput
  :: (Stream -> Stream)
  -- ^ Transform to apply to the current lexer input
  -> Lexer ()
modifyInput f = do
  s <- getInput
  putInput (f s)
----------------------------------------------------------------------------
-- | Lexer combinator for matching a 'Char'
char
  :: Char
  -- ^ The exact character to consume
  -> Lexer Char
char c = satisfy (== c)
----------------------------------------------------------------------------
-- | Lexer combinator for matching a 'String'
string'
  :: String
  -- ^ Literal string prefix to consume character by character
  -> Lexer String
string' = traverse char
----------------------------------------------------------------------------
-- | Lexer combinator for matching a 'MisoString'
string
  :: MisoString
  -- ^ Literal string prefix to consume from the input
  -> Lexer MisoString
string prefix = Lexer $ \s ->
  case s of
    Stream ys l
      | prefix `MS.isPrefixOf` ys ->
          Right (prefix, Stream (MS.drop (MS.length prefix) ys) l)
      | otherwise ->
          Left (unexpected ys l)
----------------------------------------------------------------------------
-- | Lexer combinator for executing a t'Lexer' with annotated t'Location' information
withLocation
  :: ToMisoString token
  => Lexer token
  -- ^ Inner lexer whose result is wrapped with its source 'Location'
  -> Lexer (Located token)
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
