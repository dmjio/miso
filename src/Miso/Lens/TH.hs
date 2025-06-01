-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lens.TH
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.Lens.TH (makeLenses) where
-----------------------------------------------------------------------------
import Data.Maybe
import Language.Haskell.TH
-----------------------------------------------------------------------------
makeLenses :: Name -> Q [Dec]
makeLenses name = do
  reify name >>= \case
    TyConI (NewtypeD _ _ _ _ con _) -> do
      case con of
        RecC _ fieldNames ->
          pure (processFieldNames fieldNames)
        _ -> pure []
    TyConI (DataD _ _ _ _ cons _) ->
      flip concatMapM cons $ \case
        RecC _ fieldNames -> do
          pure (processFieldNames fieldNames)
        _ -> pure []
    _ -> pure []
  where
    processFieldNames fieldNames = concat
      [ mkFields fName (ConT name) fieldType
      | (fieldName, _, fieldType) <- fieldNames
      , let fName = show (nameBase fieldName)
      , listToMaybe fName == Just '_'
      ]
    mkFields fieldName conType fieldType =
     let -- dmj: drops '_' prefix
       lensName = mkName (drop 1 fieldName)
     in
       [ FunD lensName
         [ Clause [] (NormalB (mkLens fieldName)) []
         ]
       , SigD lensName (mkLensType conType fieldType)
       ]
    concatMapM f xs =
      concat <$> mapM f xs
    mkLensType conType =
      AppT (AppT (ConT (mkName "Lens")) conType)
    mkLens n =
      AppE (AppE (VarE (mkName "lens")) (VarE (mkName n))) $
        LamE
        [ VarP recName, VarP fieldName ] $
          RecUpdE (VarE recName)
            [ (mkName n, VarE fieldName) ]
      where
        recName = mkName "record"
        fieldName = mkName "field"
-----------------------------------------------------------------------------
