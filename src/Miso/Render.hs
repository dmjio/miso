-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Render
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Render
  ( -- *** Classes
    ToHtml (..)
    -- *** Combinator
  , HTML
  ) where
----------------------------------------------------------------------------
import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Media as M
import           Servant.API (Accept (..), MimeRender (..))
----------------------------------------------------------------------------
import           Miso.String hiding (intercalate)
import           Miso.Types
----------------------------------------------------------------------------
-- | HTML MimeType used for servant APIs
--
-- > type Home = "home" :> Get '[HTML] (Component model action)
--
data HTML
----------------------------------------------------------------------------
-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentTypes _ =
    "text" M.// "html" M./: ("charset", "utf-8") NE.:|
      ["text" M.// "html"]
----------------------------------------------------------------------------
-- | Class for rendering HTML
class ToHtml a where
  toHtml :: a -> L.ByteString
----------------------------------------------------------------------------
-- | Render a @View@ to a @L.ByteString@
instance ToHtml (View a) where
  toHtml = renderView
----------------------------------------------------------------------------
-- | Render a @[View]@ to a @L.ByteString@
instance ToHtml [View a] where
  toHtml = foldMap renderView
----------------------------------------------------------------------------
-- | Render HTML from a servant API
instance ToHtml a => MimeRender HTML a where
  mimeRender _ = toHtml
----------------------------------------------------------------------------
renderView :: View a -> L.ByteString
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
renderBuilder :: View a -> Builder
renderBuilder (Text "")    = fromMisoString " "
renderBuilder (Text s)     = fromMisoString s
renderBuilder (TextRaw "") = fromMisoString " "
renderBuilder (TextRaw s)  = fromMisoString s
renderBuilder (Node _ "doctype" _ [] []) = "<!doctype html>"
renderBuilder (Node _ tag _ attrs children) =
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
renderBuilder (VComp mount attributes _ (SomeComponent Component {..})) =
  mconcat
  [ stringUtf8 "<div data-component-id=\""
  , fromMisoString mount
  , "\" "
  , intercalate " " (renderAttrs <$> attributes)
  , ">"
  , renderBuilder (view model)
  , "</div>"
  ]
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
collapseSiblingTextNodes :: [View a] -> [View a]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (Text x : Text y : xs) =
  collapseSiblingTextNodes (Text (x <> y) : xs)
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
