-----------------------------------------------------------------------------
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.FFI.QQ
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.FFI.QQ" provides the @'js'@ quasi-quoter, which lets you embed
-- JavaScript snippets directly in Haskell source. In-scope Haskell
-- variables are spliced into the JS body with @${varName}@ interpolation
-- syntax, and their types are checked at compile time via
-- 'Miso.DSL.ToJSVal'.
--
-- Enable the extension and import the quoter:
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import "Miso.FFI.QQ" ('js')
-- @
--
-- = Quick start
--
-- @
-- -- Compute a factorial entirely in JavaScript
-- fac :: Int -> IO Int
-- fac n = ['js'|
--   let x = 1;
--   for (let i = 1; i \<= ${n}; i++) {
--     x *= i;
--   }
--   return x;
-- |]
--
-- -- Call a third-party JS library with a DOM reference and a string
-- highlight :: 'Miso.DSL.JSVal' -> 'Miso.String.MisoString' -> IO ()
-- highlight domRef lang = ['js'|
--   hljs.highlightElement(${domRef}, { language: ${lang} });
-- |]
-- @
--
-- = How it works
--
-- At compile time the quasi-quoter:
--
-- 1. Lexes the JS body to find all @${varName}@ interpolations.
-- 2. Looks up each @varName@ in the Haskell scope (compile error if not found).
-- 3. Builds a 'Miso.DSL.Object' mapping short generated keys to the
--    marshalled values (via 'Miso.FFI.Internal.inline' \/ 'Miso.DSL.createWith').
-- 4. Rewrites the JS body, replacing each @${varName}@ with its generated
--    key, and wraps the whole thing in a JS function so the keys are visible
--    as named parameters.
--
-- The result is semantically equivalent to:
--
-- @
-- do o <- 'Miso.DSL.createWith' [(\"a0\", toJSVal n)]
--    'Miso.FFI.Internal.inline' \"… body with a0 instead of n …\" o
-- @
--
-- = Differences from eval
--
-- Unlike 'Miso.DSL.eval', the generated code runs in a fresh function scope —
-- it cannot read or write surrounding local variables other than those
-- explicitly interpolated. This makes it both safer and faster (JS engines
-- can optimise closed-over functions that don't reference @eval@).
--
-- = See also
--
-- * 'Miso.FFI.Internal.inline' — the runtime primitive this expands to
-- * "Miso.DSL" — 'Miso.DSL.ToJSVal', 'Miso.DSL.createWith'
-- * "Miso.FFI" — higher-level browser API wrappers
-----------------------------------------------------------------------------
module Miso.FFI.QQ
  ( js
  ) where
----------------------------------------------------------------------------
import           Control.Applicative
import           Data.Data
import           Control.Monad
import           System.IO.Unsafe (unsafePerformIO)
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Util.Lexer
import           Miso.DSL
import qualified Miso.String as MS
import qualified Miso.FFI as FFI
----------------------------------------------------------------------------
-- | QuasiQuoter for specifying inline JavaScript.
--
js :: QuasiQuoter
js = QuasiQuoter
  { quoteExp  = \s -> dataToExpQ (withString `extQ` inlineJS) s
  , quotePat  = \_ -> fail "quotePat: not implemented"
  , quoteType = \_ -> fail "quoteType: not implemented"
  , quoteDec  = \_ -> fail "quoteDec: not implemented"
  }
----------------------------------------------------------------------------
inlineJS :: String -> Maybe (Q Exp)
inlineJS jsString = pure $ do
  found <- typeCheck vars
  kvs <- forM found $ \(var, key) -> do
    k <- [| MS.pack $(stringE (MS.unpack key)) |]
    let v = mkName (MS.unpack var)
    val <- [| unsafePerformIO (toJSVal $(varE v)) :: JSVal |]
    pure $ tupE [ pure k, pure val ]
  [| do o <- createWith ($(listE kvs) :: [(MisoString, JSVal)])
        FFI.inline $(stringE (MS.unpack (formatVars (MS.pack jsString) found)))
          o
   |] where
        vars = getVariables (MS.pack jsString)
----------------------------------------------------------------------------
extQ :: (Typeable a, Typeable b) => (a -> c) -> (b -> c) -> a -> c
extQ f g a = maybe (f a) g (cast a)
----------------------------------------------------------------------------
withString :: (Quote m, Typeable a) => a -> Maybe (m Exp)
withString a = liftString <$> cast a
----------------------------------------------------------------------------
-- | Use `isPrefixOf` as you traverse the string in lex order and do a replace
formatVars :: MisoString -> [(MisoString, MisoString)] -> MisoString
formatVars s [] = s
formatVars s table@((var,key):xs) =
  case MS.uncons s of
    Nothing ->
      mempty
    Just ('$', cs) -> do
      let needle = "{" <> var <> "}"
      if needle `MS.isPrefixOf` cs
        then
          formatVars (key <> MS.drop (MS.length needle) cs) xs
        else
          formatVars cs table
    Just (c,cs) ->
      MS.cons c (formatVars cs table)
----------------------------------------------------------------------------
keys :: [MisoString]
keys = do
  (x,y) <- (,) <$> ['a'..'z'] <*> ['0'..'9']
  pure (MS.pack [x,y])
----------------------------------------------------------------------------
typeCheck :: [MisoString] -> Q [(MisoString, MisoString)]
typeCheck vars = do
  forM (Prelude.zip vars keys) $ \(var, key) ->
    lookupValueName (MS.unpack var) >>= \case
      Nothing -> fail (MS.unpack var <> " is not in scope")
      Just _ -> pure (var, key)
---------------------------------------------------------------------------
getVariables :: MisoString -> [MisoString]
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

    lexer :: Lexer [MisoString]
    lexer = Prelude.filter (/="") <$>
      many (varLexer <|> anything)
----------------------------------------------------------------------------
