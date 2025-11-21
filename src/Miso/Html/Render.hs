-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE CPP                   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Render
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 'Miso.Types.View' serialization
--
----------------------------------------------------------------------------
module Miso.Html.Render
  ( -- *** Classes
    ToHtml (..)
  ) where
----------------------------------------------------------------------------
import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import           Unsafe.Coerce (unsafeCoerce)
#ifdef SSR
import Control.Exception (SomeException, catch)
import System.IO.Unsafe (unsafePerformIO)
#endif
----------------------------------------------------------------------------
import           Miso.String hiding (intercalate)
import           Miso.Types
----------------------------------------------------------------------------
-- | Class for rendering HTML
class ToHtml a where
  toHtml :: a -> L.ByteString
----------------------------------------------------------------------------
-- | Render a @Miso.Types.View@ to a @L.ByteString@
instance ToHtml (Miso.Types.View m a) where
  toHtml = renderView
----------------------------------------------------------------------------
-- | Render a @[Miso.Types.View]@ to a @L.ByteString@
instance ToHtml [Miso.Types.View m a] where
  toHtml = foldMap renderView
----------------------------------------------------------------------------
renderView :: View m a -> L.ByteString
renderView = toLazyByteString . renderBuilder
----------------------------------------------------------------------------
intercalate :: Builder -> [Builder] -> Builder
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) =
  mconcat
  [ x
  , sep
  , intercalate sep xs
  ]
----------------------------------------------------------------------------
renderBuilder :: Miso.Types.View m a -> Builder
renderBuilder = \case
  VText "" ->
    fromMisoString " "
  VText s  ->
    fromMisoString s
  VNode _ "doctype" [] [] ->
    "<!bdoctype html>"
  VNode _ tag attrs children ->
    mconcat
    [ "<"
    , fromMisoString tag
    , mconcat [ " " <> intercalate " " (renderAttrs <$> attrs)
              | not (Prelude.null attrs)
              ]
    , ">"
    , mconcat
      [ mconcat
        [ foldMap renderBuilder (collapseSiblingTextNodes children)
        , "</" <> fromMisoString tag <> ">"
        ]
      | tag `notElem` ["img", "input", "br", "hr", "meta", "link"]
      ]
    ]
  VComp _ (SomeComponent vcomp) ->
    mconcat $ renderBuilder <$> vkids
      where
#ifdef SSR
      vkids = [ unsafeCoerce $ (view vcomp) $ getInitialComponentModel vcomp ]
#else
      vkids = [ unsafeCoerce $ (view vcomp) (model vcomp) ]
#endif
  VFrag _ kids ->
    mconcat $ renderBuilder <$> kids
----------------------------------------------------------------------------
renderAttrs :: Attribute action -> Builder
renderAttrs (Property key value) =
  mconcat
  [ fromMisoString key
  , stringUtf8 "=\""
  , toHtmlFromJSON value
  , stringUtf8 "\""
  ]
renderAttrs (On _) = mempty
renderAttrs (Styles styles) =
  mconcat
  [ "style"
  , stringUtf8 "=\""
  , mconcat
    [ mconcat
      [ fromMisoString k
      , charUtf8 ':'
      , fromMisoString v
      , charUtf8 ';'
      ]
    | (k,v) <- M.toList styles
    ]
  , stringUtf8 "\""
  ]
----------------------------------------------------------------------------
-- | The browser can't distinguish between multiple text nodes
-- and a single text node. So it will always parse a single text node
-- this means we must collapse adjacent text nodes during hydration.
collapseSiblingTextNodes :: [View m a] -> [View m a]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (VText x : VText y : xs) =
  collapseSiblingTextNodes (VText (x <> y) : xs)
collapseSiblingTextNodes (x:xs) =
  x : collapseSiblingTextNodes xs
----------------------------------------------------------------------------
-- | Helper for turning JSON into Text
-- Object, Array and Null are kind of non-sensical here
toHtmlFromJSON :: Value -> Builder
toHtmlFromJSON (String t)   = fromMisoString (ms t)
toHtmlFromJSON (Number t)   = fromMisoString $ ms (show t)
toHtmlFromJSON (Bool True)  = "true"
toHtmlFromJSON (Bool False) = "false"
toHtmlFromJSON Null         = "null"
toHtmlFromJSON (Object o)   = fromMisoString $ ms (show o)
toHtmlFromJSON (Array a)    = fromMisoString $ ms (show a)
-----------------------------------------------------------------------------
#ifdef SSR
-- | Used for server-side model hydration, internally only in 'renderView'.
--
-- We use 'unsafePerformIO' here because @servant@'s 'MimeRender' is a pure function
-- yet we need to allow the users to hydrate in 'IO'.
--
getInitialComponentModel :: Component parent model action -> model
getInitialComponentModel Component {..} =
  case hydrateModel of
    Nothing -> model
    Just action -> unsafePerformIO $
      action `catch` (\(e :: SomeException) -> do
        putStrLn "Encountered exception during model hydration, falling back to default model"
        print e
        pure model)
----------------------------------------------------------------------------
#endif
