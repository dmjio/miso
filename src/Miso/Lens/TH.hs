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
module Miso.Lens.TH (makeLenses, makeClassy, compose, this) where
-----------------------------------------------------------------------------
import Data.Char
import Data.Maybe
import Language.Haskell.TH
-----------------------------------------------------------------------------
import Miso.Util (compose)
import Miso.Lens (this)
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
      , let fName = nameBase fieldName
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
makeClassy :: Name -> Q [Dec]
makeClassy name = do
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
    instanceName =
      AppT (ConT (mkName ("Has" <> baseName))) (ConT name)
    baseName = nameBase name
    baseNameLower
      | x : xs <- baseName = toLower x : xs
      | otherwise = []
    processFieldNames fieldNames =
        [ InstanceD Nothing [] instanceName
          [ ValD (VarP (mkName baseNameLower)) (NormalB (VarE (mkName "this"))) []
            -- instance HasFoo Foo where foo = this
          ]
        , ClassD [] (mkName $ "Has" <> nameBase name)
            [ PlainTV (mkName baseNameLower) BndrReq
            ] [] $ reverse $ concat
            [ mkFields fName (VarT (mkName baseNameLower)) fieldType
            | (fieldName, _, fieldType) <- fieldNames
            , let fName = nameBase fieldName
            , listToMaybe fName == Just '_'
            ] ++
            [ SigD
                (mkName baseNameLower) 
                (AppT
                   (AppT
                      (ConT (mkName "Lens"))
                      (VarT (mkName baseNameLower)))
                      (ConT name))
            ]
        ]
    mkFields fieldName varType fieldType =
      let -- dmj: drops '_' prefix
        lensName = mkName (drop 1 fieldName)
      in
        [ FunD lensName
          [ Clause [] (NormalB (wrapMkLens fieldName)) []
          ]
          -- fooX = lens _fooX (\r x -> r { _fooX = x }) . foo
        , SigD lensName (mkLensType varType fieldType)
          -- fooY :: Lens foo Int
        ]
    concatMapM f xs =
      concat <$> mapM f xs
    mkLensType varType x =
      AppT (AppT (ConT (mkName "Lens")) varType) x
    wrapMkLens n =
      AppE (AppE (VarE (mkName "compose")) (mkLens n)) (VarE (mkName baseNameLower))
    mkLens n
      = AppE (AppE (VarE (mkName "lens")) (VarE (mkName n)))
      $ LamE [ VarP recName, VarP fieldName ]
      $ RecUpdE (VarE recName) [ (mkName n, VarE fieldName) ]
      where
        recName = mkName "record"
        fieldName = mkName "field"
-------------------------------------------------------------------------------
