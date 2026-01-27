{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.QQ
-- Copyright   :  (C) 2016-2025 David M. Johnson (@dmjio)
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A QuasiQuoter for `inline-js` functionality.
--
-- @
--
-- {-# LANGUAGE QuasiQuotes #-}
--
-- import Miso.FFI.QQ (js)
--
-- fac :: Int -> IO Int
-- fac n = [js|
--   let x = 0;
--   for (i = 0; i < ${n}; i++) {
--     x *= i;
--   }
--   return x;
-- |]
--
-- @
--
----------------------------------------------------------------------------
module Miso.FFI.QQ
  ( js
  ) where
----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Control.Applicative
import           Data.Typeable
import           Control.Monad
import qualified Data.Set as S
import           Data.Set (Set)
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
----------------------------------------------------------------------------
import           Miso.String
import           Miso.Util.Lexer
import qualified Miso.String as MS
import qualified Miso.FFI as FFI
import qualified Miso.DSL as DSL
----------------------------------------------------------------------------
-- | QuasiQuoter for specifying multiline 'Miso.String.MisoString'
--
js :: QuasiQuoter
js = QuasiQuoter
  { quoteExp  = \s -> dataToExpQ (withString `extQ` inlineJS) (pack s)
  , quotePat  = \_ -> fail "quotePat: not implemented"
  , quoteType = \_ -> fail "quoteType: not implemented"
  , quoteDec  = \_ -> fail "quoteDec: not implemented"
  }
----------------------------------------------------------------------------
inlineJS :: MisoString -> Maybe (Q Exp)
inlineJS jsString = pure $ do
  kvs <- forM (S.toList vars) $ \s -> do
    k <- [| MS.pack $(stringE (MS.unpack s)) |]
    let v = mkName (MS.unpack s)
    pure $ tupE [ pure k, pure (VarE v) ]
  [| FFI.inline (formatVars jsString vars) =<< createWith $(listE kvs) |]
    where
      vars = getVariables jsString
----------------------------------------------------------------------------
extQ :: (Typeable a, Typeable b) => (a -> c) -> (b -> c) -> a -> c
extQ f g a = maybe (f a) g (cast a)
----------------------------------------------------------------------------
withString :: (Quote m, Typeable a) => a -> Maybe (m Exp)
withString a = liftString <$> cast a
----------------------------------------------------------------------------
formatVars :: MisoString -> Set MisoString -> MisoString
formatVars = Prelude.foldl' go
  where
    go :: MisoString -> MisoString -> MisoString
    go haystack var = replace needle var haystack
      where
        needle = "${" <> var <> "}"
----------------------------------------------------------------------------
typeCheck :: Set MisoString -> Q (Map MisoString Exp)
typeCheck xs = M.unions <$> do
  forM (S.toList xs) $ \x ->
    lookupValueName (unpack x) >>= \case
      Nothing -> fail (MS.unpack x <> " is not in scope")
      Just v -> pure (M.singleton x (VarE v))
---------------------------------------------------------------------------
getVariables :: MisoString -> Set MisoString
getVariables s =
  case runLexer lexer (mkStream s) of
    Left _ -> mempty
    Right (xs,_) -> xs
  where
    varLexer :: Lexer MisoString
    varLexer = do
      void (string "${")
      xs <- some $ satisfy (/= '}')
      void (char '}')
      pure (MS.pack xs)

    anything :: Lexer MisoString
    anything = mempty <$ satisfy (const True)

    lexer :: Lexer (Set MisoString)
    lexer = S.filter (/="") . S.fromList <$>
      many (varLexer <|> anything)
----------------------------------------------------------------------------
