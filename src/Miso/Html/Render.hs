-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Render
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 'View' serialization
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
----------------------------------------------------------------------------
import           Miso.String hiding (intercalate)
import qualified Miso.String as MS
import           Miso.Types
----------------------------------------------------------------------------
-- | Class for rendering HTML
class ToHtml a where
  toHtml :: a -> L.ByteString
----------------------------------------------------------------------------
-- | Render a @View@ to a @L.ByteString@
instance ToHtml (View m a) where
  toHtml = renderView
----------------------------------------------------------------------------
-- | Render a @[View]@ to a @L.ByteString@
instance ToHtml [View m a] where
  toHtml = foldMap renderView
----------------------------------------------------------------------------
-- | Render a @Component parent model action@ to a @L.ByteString@
instance ToHtml (Component parent model action) where
  toHtml Component {..} = renderView (view model)
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
-- |
-- HTML-encodes the given text.
--
-- >>> Data.Text.IO.putStrLn $ text "<a href=\"\">"
-- &lt;a href=&quot;&quot;&gt;
htmlEncode :: MisoString -> MisoString
htmlEncode = MS.concatMap $ \case
  '<' -> "&lt;"
  '>' -> "&gt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  x -> MS.singleton x
----------------------------------------------------------------------------
renderBuilder :: View m a -> Builder
renderBuilder (VText "")    = fromMisoString " "
renderBuilder (VText s)     = fromMisoString (htmlEncode s)
renderBuilder (VNode _ "doctype" [] []) = "<!doctype html>"
renderBuilder (VNode _ tag attrs children) =
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
renderBuilder (VComp ns tag attrs (SomeComponent Component {..})) =
  renderBuilder (VNode ns tag attrs [ unsafeCoerce (view model) ])
  -- dmj: Just trust me bro moment.
  -- This is fine to do because we don't need the polymorphism here
  -- when monomorphizing to Builder. Release the skolems.
----------------------------------------------------------------------------
renderAttrs :: Attribute action -> Builder
renderAttrs (Property key value) =
  mconcat
  [ fromMisoString key
  , stringUtf8 "=\""
  , toHtmlFromJSON value
  , stringUtf8 "\""
  ]
renderAttrs (Event _) = mempty
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
----------------------------------------------------------------------------
