-----------------------------------------------------------------------------
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell     #-}
-----------------------------------------------------------------------------
module Miso.DSL.TH.File (evalFile) where
-----------------------------------------------------------------------------
import Language.Haskell.TH qualified as TH
-----------------------------------------------------------------------------
import Miso.DSL.TH (evalTH)
-----------------------------------------------------------------------------
-- | Like 'eval', but read the JS code to evaluate from a file.
evalFile
  :: FilePath
  -- ^ Path to JS file that will be converted into an FFI declaration.
  -> TH.Q TH.Exp
evalFile path = eval_ =<< TH.runIO (readFile path)
  where
    eval_ :: String -> TH.Q TH.Exp
    eval_ chunk = [| $(Miso.DSL.TH.evalTH chunk []) :: IO () |]
-----------------------------------------------------------------------------
