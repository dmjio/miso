{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}
module Miso.Boot where

#if __GLASGOW_HASKELL__ >= 865

#ifdef WASM
import Language.Haskell.TH.Syntax
    ( mkName,
      Q,
      Type(TupleT, AppT, ConT),
      Dec(ForeignD),
      Callconv(JavaScript),
      Foreign(ExportF) )
import Data.Maybe ( fromMaybe )
#else
import Language.Haskell.TH ( Q, Dec )
#endif

-- | Export entry point for WASM backend
-- otherwise expand to empty list of Decs
--
-- @
-- foreign export javascript "hs_start" main :: IO ()
-- @
entryPoint :: Maybe String -> Q [Dec]
#ifdef WASM
entryPoint epn =
  pure [ ForeignD (ExportF JavaScript n (mkName "main") (AppT (ConT ''IO) (TupleT 0))) ]
  where
    n = fromMaybe "hs_start" epn
#else
entryPoint _ = pure []
#endif

#endif
