-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE CPP                   #-}
#ifdef SSR
{-# LANGUAGE RecordWildCards       #-}
#endif
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
-- * __'Miso.Types.VText'__ — rendered with HTML escaping: @&@, @\<@ and @>@
--   become @&amp;@, @&lt;@ and @&gt;@, so the browser's parser reproduces
--   the original string exactly. This keeps hydration faithful (the parsed
--   DOM text matches the client's virtual DOM byte-for-byte) and prevents
--   markup \/ script injection through text content. Text inside raw-text
--   elements (@script@, @style@) is emitted unescaped, since the parser
--   does not decode entities there.
-- * __Attribute values__ — rendered with HTML escaping: @&@, @\"@, @\<@ and
--   @>@ become entities, for the same round-tripping and injection-safety
--   reasons as text nodes.
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
import           System.IO.Unsafe (unsafePerformIO)
#ifdef SSR
import           Control.Exception (SomeException, catch)
#endif
----------------------------------------------------------------------------
import           Data.IORef (readIORef)
import           GHC.StaticPtr
----------------------------------------------------------------------------
import           Miso.JSON
import           Miso.Runtime (globalContext)
import           Miso.String hiding (intercalate)
import qualified Miso.String as MS
import           Miso.Types
----------------------------------------------------------------------------
-- | Class for rendering HTML
class ToHtml a where
  toHtml :: a -> L.ByteString
----------------------------------------------------------------------------
-- | Render a @Miso.Types.View@ to a @L.ByteString@
instance ToHtml (View context model action) where
  toHtml = renderView
----------------------------------------------------------------------------
-- | Render a @[Miso.Types.View]@ to a @L.ByteString@
instance ToHtml [View context model action] where
  toHtml = foldMap renderView
----------------------------------------------------------------------------
renderView :: View context model action -> L.ByteString
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
renderBuilder :: View context model action -> Builder
renderBuilder = renderBuilderWith False
----------------------------------------------------------------------------
-- | Serializes a 'View'. The 'Bool' tracks whether we are inside a raw-text
-- element (@script@ \/ @style@), where the HTML parser does not decode
-- character references and text must therefore be emitted unescaped.
renderBuilderWith :: Bool -> View context model action -> Builder
renderBuilderWith _ (VText _ "")    = fromMisoString " "
renderBuilderWith rawText (VText _ s)
  | rawText = fromMisoString s
  | otherwise = escapeText s
renderBuilderWith _ (VNode _ "doctype" [] [] _) = "<!doctype html>"
renderBuilderWith _ (VNode ns tag attrs children _) = mconcat
  [ "<"
  , fromMisoString tag
  , mconcat [ " " <> intercalate " " (renderAttrs <$> attrs)
            | not (Prelude.null attrs)
            ]
  , if tag `elem` selfClosing then "/>" else ">"
  , mconcat
    [ mconcat
      [ foldMap
          (renderBuilderWith (tag `elem` rawTextElements))
          (collapseSiblingTextNodes children)
      , "</" <> fromMisoString tag <> ">"
      ]
    | tag `notElem` selfClosing
    ]
  ] where
      rawTextElements = [ "script", "style" ]
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
renderBuilderWith rawText (VComp someComp) =
  case someComp of
    SomeComponent _key props comp_ ->
      -- The app-global @context@ is read from 'globalContext'. For the common
      -- @context ~ ()@ case the 'view' ignores it, so the initial @undefined@ is
      -- never forced. But if a 'view' here inspects a non-trivial @context@,
      -- SSR must seed the cell with 'Miso.setContext' before serializing, or
      -- forcing @ctx@ raises an exception. See 'Miso.setContext' for details.
      let ctx = unsafePerformIO (readIORef globalContext) in
#ifdef SSR
      renderBuilderWith rawText (view comp_ ctx props (getInitialComponentModel comp_))
#else
      renderBuilderWith rawText (view comp_ ctx props (model comp_))
#endif
renderBuilderWith rawText (VCompStatic ptr props0) =
  case deRefStaticPtr ptr of
   SomeStaticComponent mk -> case mk props0 of
    SomeComponent _key props comp_ ->
      -- The app-global @context@ is read from 'globalContext'. For the common
      -- @context ~ ()@ case the 'view' ignores it, so the initial @undefined@ is
      -- never forced. But if a 'view' here inspects a non-trivial @context@,
      -- SSR must seed the cell with 'Miso.setContext' before serializing, or
      -- forcing @ctx@ raises an exception. See 'Miso.setContext' for details.
      let ctx = unsafePerformIO (readIORef globalContext) in
#ifdef SSR
      renderBuilderWith rawText (view comp_ ctx props (getInitialComponentModel comp_))
#else
      renderBuilderWith rawText (view comp_ ctx props (model comp_))
#endif
renderBuilderWith rawText (VFrag _ kids) =
  foldMap (renderBuilderWith rawText) kids
----------------------------------------------------------------------------
renderAttrs :: Attribute model action -> Builder
renderAttrs (ClassList classes) =
  mconcat
  [ "class"
  , stringUtf8 "=\""
  , escapeAttr (MS.unwords classes)
  , stringUtf8 "\""
  ]
renderAttrs (Property key (Bool enabled)) -- dmj: account for boolean properties
  | S.member key booleanProperties, enabled = fromMisoString key
  | S.member key booleanProperties, not enabled = mempty
  | otherwise = mconcat
      [ fromMisoString key
      , stringUtf8 "=\""
      , escapeAttr (textFromJSON (Bool enabled))
      , stringUtf8 "\""
      ]
renderAttrs (Property "key" _) = mempty
renderAttrs (Property key value) =
  mconcat
  [ fromMisoString key
  , stringUtf8 "=\""
  , escapeAttr (textFromJSON value)
  , stringUtf8 "\""
  ]
renderAttrs (On _) = mempty
renderAttrs (OnStatic _) = mempty
renderAttrs (Styles styles_) =
  mconcat
  [ "style"
  , stringUtf8 "=\""
  , mconcat
    [ mconcat
      [ escapeAttr k
      , charUtf8 ':'
      , escapeAttr v
      , charUtf8 ';'
      ]
    | (k,v) <- M.toList styles_
    ]
  , stringUtf8 "\""
  ]
----------------------------------------------------------------------------
-- | Escapes a text node so the browser's parser reproduces the original
-- string exactly. Without this, text containing @\<@ followed by a letter
-- (e.g. code listings, user-generated content) parses as markup: the DOM
-- gains phantom elements, hydration reports a mismatch and falls back to a
-- full diff, and untrusted text becomes an injection vector.
escapeText :: MisoString -> Builder
escapeText = mconcat . fmap escapeChar . MS.unpack
  where
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar c   = charUtf8 c
----------------------------------------------------------------------------
-- | Escapes an attribute value (values are always rendered double-quoted,
-- so @\"@ must become an entity as well).
escapeAttr :: MisoString -> Builder
escapeAttr = mconcat . fmap escapeChar . MS.unpack
  where
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar c    = charUtf8 c
----------------------------------------------------------------------------
-- | The browser can't distinguish between multiple text nodes
-- and a single text node. So it will always parse a single text node
-- this means we must collapse adjacent text nodes during hydration.
collapseSiblingTextNodes :: [View context model action] -> [View context model action]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (VText _ x : VText k y : xs) =
  collapseSiblingTextNodes (VText k (x <> y) : xs)
collapseSiblingTextNodes (x:xs) =
  x : collapseSiblingTextNodes xs
----------------------------------------------------------------------------
-- | Helper for turning JSON into Text
-- Object, Array and Null are kind of non-sensical here
textFromJSON :: Value -> MisoString
textFromJSON (String t)   = ms t
textFromJSON (Number t)   = ms (show t)
textFromJSON (Bool True)  = "true"
textFromJSON (Bool False) = "false"
textFromJSON Null         = "null"
textFromJSON (Object o)   = ms (show o)
textFromJSON (Array a)    = ms (show a)
-----------------------------------------------------------------------------
#ifdef SSR
-- | Used for server-side model hydration, internally only in 'renderView'.
--
-- We use 'unsafePerformIO' here because @servant@'s 'MimeRender' is a pure function
-- yet we need to allow the users to hydrate in 'IO'.
--
getInitialComponentModel :: Component context props model action -> model
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
