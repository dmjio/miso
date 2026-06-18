-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lens.TH
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Miso.Lens.TH
  ( -- ** TH
    makeLenses
  , makeClassy
    -- ** Re-exports
  , lens
  , compose
  , this
  , Lens
  ) where
-----------------------------------------------------------------------------
import Data.Char
import Data.Maybe
import Language.Haskell.TH
-----------------------------------------------------------------------------
import Miso.Util (compose)
import Miso.Lens (this, lens, Lens)
-----------------------------------------------------------------------------
-- | Automatically generates Haskell lenses via template-haskell.
--
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
      [ mkFields fieldName (ConT name) fieldType
      | (fieldName, _, fieldType) <- fieldNames
      , listToMaybe (nameBase fieldName) == Just '_'
      ]
    mkFields fieldName conType fieldType =
     let -- dmj: drops '_' prefix
       lensName = mkName (drop 1 (nameBase fieldName))
     in
       [ FunD lensName
         [ Clause [] (NormalB (mkLens fieldName)) []
         ]
       , SigD lensName (mkLensType conType fieldType)
       ]
    concatMapM f xs =
      concat <$> mapM f xs
    mkLensType conType =
      AppT (AppT (ConT ''Lens) conType)
    mkLens fieldName =
      AppE (AppE (VarE 'lens) (VarE fieldName))
        $ LamE [ VarP recName, VarP fieldVar ]
        $ RecUpdE (VarE recName) [ (fieldName, VarE fieldVar) ]
      where
        recName = mkName "record"
        fieldVar = mkName "field"
-----------------------------------------------------------------------------
-- | Automatically generates classy lenses via template-haskell.
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
          [ ValD (VarP (mkName baseNameLower)) (NormalB (VarE 'this)) []
            -- instance HasFoo Foo where foo = this
          ]
        , ClassD [] (mkName $ "Has" <> nameBase name)
            [ PlainTV (mkName baseNameLower) BndrReq
            ] [] $ reverse $ concat
            [ mkFields fieldName (VarT (mkName baseNameLower)) fieldType
            | (fieldName, _, fieldType) <- fieldNames
            , listToMaybe (nameBase fieldName) == Just '_'
            ] ++
            [ SigD
                (mkName baseNameLower)
                (AppT
                   (AppT
                      (ConT ''Lens)
                      (VarT (mkName baseNameLower)))
                      (ConT name))
            ]
        ]
    mkFields fieldName varType fieldType =
      let -- dmj: drops '_' prefix
        lensName = mkName (drop 1 (nameBase fieldName))
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
      AppT (AppT (ConT ''Lens) varType) x
    wrapMkLens fieldName =
      AppE (AppE (VarE 'compose) (mkLens fieldName)) (VarE (mkName baseNameLower))
    mkLens fieldName
      = AppE (AppE (VarE 'lens) (VarE fieldName))
      $ LamE [ VarP recName, VarP fieldVar ]
      $ RecUpdE (VarE recName) [ (fieldName, VarE fieldVar) ]
      where
        recName = mkName "record"
        fieldVar = mkName "field"
-------------------------------------------------------------------------------
