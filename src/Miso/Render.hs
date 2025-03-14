{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Miso.Render
  ( ToHtml (..)
  , HTML
  ) where

import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Media as M
import           Servant.API (Accept (..), MimeRender (..))

import           Miso.String hiding (intercalate)
import           Miso.Types

data HTML

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentTypes _ =
    "text" M.// "html" M./: ("charset", "utf-8") NE.:|
      ["text" M.// "html"]

class ToHtml a where
  toHtml :: a -> L.ByteString

instance ToHtml (Component name model action) where
  toHtml = renderComponent

instance ToHtml (View a) where
  toHtml = renderView

instance ToHtml [View a] where
  toHtml = foldMap renderView

instance ToHtml a => MimeRender HTML a where
  mimeRender _ = toHtml

renderView :: View a -> L.ByteString
renderView = toLazyByteString . renderBuilder

renderComponent :: Component name model action -> L.ByteString
renderComponent (Component _ App{..}) = renderView (view model)

intercalate :: Builder -> [Builder] -> Builder
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) =
  mconcat
  [ x
  , sep
  , intercalate sep xs
  ]

renderBuilder :: View a -> Builder
renderBuilder (Text "")    = fromMisoString " "
renderBuilder (Text s)     = fromMisoString s
renderBuilder (TextRaw "") = fromMisoString " "
renderBuilder (TextRaw s)  = fromMisoString s
renderBuilder (Node _ "doctype" _ [] []) = "<!doctype html>"
renderBuilder (Node _ tag _ attrs children) =
  mconcat $
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
renderBuilder (Embed (SomeComponent (Component mount App {..})) options) =
  mconcat
  [ stringUtf8 "<div data-component-id=\""
  , fromMisoString mount
  , "\" "
  , intercalate " " (renderAttrs <$> attributes options)
  , ">"
  , renderBuilder (view model)
  , "</div>"
  ]

renderAttrs :: Attribute action -> Builder
renderAttrs (P key value) =
  mconcat
  [ fromMisoString key
  , stringUtf8 "=\""
  , toHtmlFromJSON value
  , stringUtf8 "\""
  ]
renderAttrs (E _) = mempty
renderAttrs (S styles) =
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
  
collapseSiblingTextNodes :: [View a] -> [View a]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (Text x : Text y : xs) =
  collapseSiblingTextNodes (Text (x <> y) : xs)
collapseSiblingTextNodes (x:xs) =
  x : collapseSiblingTextNodes xs

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
