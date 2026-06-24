-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE CPP                   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Render
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Html.Render" provides the 'ToHtml' typeclass for serialising a
-- 'Miso.Types.View' tree to a lazy 'Data.ByteString.Lazy.ByteString' of
-- UTF-8 HTML. This is the foundation of miso's
-- <https://en.wikipedia.org/wiki/Server-side_scripting server-side rendering (SSR)>
-- support.
--
-- Instances are provided for both @'Miso.Types.View' m a@ (a single node)
-- and @['Miso.Types.View' m a]@ (a sequence of nodes).
--
-- = Quick start
--
-- @
-- import           "Miso.Html.Render" ('ToHtml', 'toHtml')
-- import qualified Data.ByteString.Lazy as L
--
-- renderPage :: Model -> L.ByteString
-- renderPage m = 'toHtml' (view m)
-- @
--
-- With @servant@, use @'toHtml'@ inside a @'Data.ByteString.Lazy.ByteString'@
-- or @OctetStream@ response, or wire it into a 'Miso.Html.Render.ToHtml' servant
-- MIME type.
--
-- = Rendering rules
--
-- * __'Miso.Types.VNode'__ — rendered as @\<tag attrs\>children\<\/tag\>@.
--   Self-closing elements (@\<br\/\>@, @\<img\/\>@, @\<input\/\>@, …) are
--   rendered without a closing tag.
-- * __'Miso.Types.VText'__ — rendered as a raw text string (no escaping
--   beyond what is already in the 'Miso.String.MisoString').
-- * __'Miso.Types.VComp'__ — recursively renders the sub-component's view
--   using its initial (or hydrated) model.
-- * __'Miso.Types.VFrag'__ — renders all children inline, no wrapper tag.
-- * __Event handlers__ (@'Miso.Types.On'@) — silently dropped; they have
--   no meaning in a static HTML string.
-- * __Boolean properties__ (@disabled@, @checked@, @required@, …) — rendered
--   as bare attribute names when @True@, omitted entirely when @False@.
-- * __Adjacent text nodes__ — collapsed into a single text node to match
--   browser parsing behaviour during hydration.
--
-- = SSR flag
--
-- When compiled with @-fssr@ the renderer calls the component's optional
-- @hydrateModel@ action to derive the initial model (e.g. by fetching from
-- a database), falling back to the static @model@ if the action throws.
--
-- = See also
--
-- * "Miso.Hydrate" — client-side hydration from server-rendered HTML
-- * "Miso.Html.Element" — element smart constructors
-- * "Miso.Html" — top-level HTML DSL re-export hub
-----------------------------------------------------------------------------
module Miso.Html.Render
  ( -- *** Classes
    ToHtml (..)
  ) where
----------------------------------------------------------------------------
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import           Unsafe.Coerce (unsafeCoerce)
#ifdef SSR
import           Control.Exception (SomeException, catch)
import           System.IO.Unsafe (unsafePerformIO)
#endif
----------------------------------------------------------------------------
import           Miso.JSON
import           Miso.String hiding (intercalate)
import qualified Miso.String as MS
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
booleanProperties :: Set MisoString
booleanProperties = S.fromList
  [ "allowfullscreen"
  , "allowpaymentrequest"
  , "allowusermedia"
  , "async"
  , "autofocus"
  , "autoplay"
  , "checked"
  , "controls"
  , "default"
  , "defer"
  , "disabled"
  , "download"
  , "formnovalidate"
  , "hidden"
  , "inert"
  , "ismap"
  , "itemscope"
  , "loop"
  , "multiple"
  , "muted"
  , "nomodule"
  , "novalidate"
  , "open"
  , "playsinline"
  , "readonly"
  , "required"
  , "reversed"
  , "selected"
  , "truespeed"
  ]
----------------------------------------------------------------------------
renderBuilder :: forall m a . Miso.Types.View m a -> Builder
renderBuilder (VText _ "")    = fromMisoString " "
renderBuilder (VText _ s)     = fromMisoString s
renderBuilder (VNode _ "doctype" [] []) = "<!doctype html>"
renderBuilder (VNode ns tag attrs children) = mconcat
  [ "<"
  , fromMisoString tag
  , mconcat [ " " <> intercalate " " (renderAttrs <$> attrs)
            | not (Prelude.null attrs)
            ]
  , if tag `elem` selfClosing then "/>" else ">"
  , mconcat
    [ mconcat
      [ foldMap renderBuilder (collapseSiblingTextNodes children)
      , "</" <> fromMisoString tag <> ">"
      ]
    | tag `notElem` selfClosing
    ]
  ] where
      selfClosing = htmls <> svgs <> mathmls
      htmls = [ x
              | ns == HTML
              , x <- [ "area", "base", "col", "embed", "img", "input", "br", "hr", "meta", "link", "param", "source", "track", "wbr" ]
              ]
      svgs  = [ x
              | ns == SVG
              , x <- [ "circle", "line", "rect", "path", "ellipse", "polygon", "polyline", "use", "image"]
              ]
      mathmls =
              [ x
              | ns == MATHML
              , x <- ["mglyph", "mprescripts", "none", "maligngroup", "malignmark" ]
              ]

renderBuilder (VComp _ (SomeComponent props vcomp_)) =
  foldMap renderBuilder vkids
    where
#ifdef SSR
      vkids = [ unsafeCoerce $ view vcomp_ props (getInitialComponentModel vcomp_) ]
#else
      vkids = [ unsafeCoerce $ view vcomp_ props (model vcomp_) ]
#endif
renderBuilder (VFrag _ kids) = foldMap renderBuilder kids
----------------------------------------------------------------------------
renderAttrs :: Attribute action -> Builder
renderAttrs (ClassList classes) =
  mconcat
  [ "class"
  , stringUtf8 "=\""
  , fromMisoString (MS.unwords classes)
  , stringUtf8 "\""
  ]
renderAttrs (Property key (Bool enabled)) -- dmj: account for boolean properties
  | S.member key booleanProperties, enabled = fromMisoString key
  | S.member key booleanProperties, not enabled = mempty
  | otherwise = mconcat
      [ fromMisoString key
      , stringUtf8 "=\""
      , toHtmlFromJSON (Bool enabled)
      , stringUtf8 "\""
      ]
renderAttrs (Property "key" _) = mempty
renderAttrs (Property key value) =
  mconcat
  [ fromMisoString key
  , stringUtf8 "=\""
  , toHtmlFromJSON value
  , stringUtf8 "\""
  ]
renderAttrs (On _) = mempty
renderAttrs (Styles styles_) =
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
    | (k,v) <- M.toList styles_
    ]
  , stringUtf8 "\""
  ]
----------------------------------------------------------------------------
-- | The browser can't distinguish between multiple text nodes
-- and a single text node. So it will always parse a single text node
-- this means we must collapse adjacent text nodes during hydration.
collapseSiblingTextNodes :: [View m a] -> [View m a]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (VText _ x : VText k y : xs) =
  collapseSiblingTextNodes (VText k (x <> y) : xs)
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
