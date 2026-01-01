-----------------------------------------------------------------------------
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE LambdaCase          #-}
-----------------------------------------------------------------------------
module Miso.DSL.TH (evalTH) where
-----------------------------------------------------------------------------
import Control.Exception (evaluate)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
-----------------------------------------------------------------------------
-- | This is from @amesgen. It's a workaround until we can have proper
-- support for js-sources. It constructs a 
--
evalTH :: String -> [TH.Q TH.Type] -> TH.Q TH.Exp
evalTH jsChunk argTys = do
  ffiImportName <- TH.newName . show =<< TH.newName "wasm_ffi_import_eval"
  sig <- mkSig argTys
  let ffiImport =
        TH.ForeignD $
          TH.ImportF
            TH.JavaScript
            TH.Safe
            jsChunk
            ffiImportName
            sig
  TH.addTopDecls [ffiImport]

  argNames <- traverse (\_ -> TH.newName "x") argTys
  let argPats = TH.varP <$> argNames
      argExps = TH.varE <$> argNames
  -- Safe FFI imports return a thunk that needs to be evaluated to make sure
  -- that the FFI call actually completed ('unsafeInterleaveIO'-like). To avoid
  -- surprises, use this unconditionally.
  TH.lamE argPats [|evaluate =<< $(TH.appsE $ TH.varE ffiImportName : argExps)|]
  where
    mkSig = \case
      [] -> [t|IO ()|]
      t : ts -> [t|$t -> $(mkSig ts)|]
-----------------------------------------------------------------------------
