-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
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
-- * __'Miso.Types.VText'__ — rendered as a raw text string (no escaping
--   beyond what is already in the 'Miso.String.MisoString').
-- * __'Miso.Types.VComp'__ — recursively renders the sub-component's view
--   using its initial (or hydrated) model.
-- * __'Miso.Types.VFrag'__ — renders all children inline, no wrapper tag.
-- * __'Miso.Types.VContext'__ — resolved against the app-global @context@
--   (read from 'globalContext') and the result rendered in its place.
-- * __'Miso.Types.VProps'__ — resolved against the @props@ of the enclosing
--   component (@()@ for a bare 'Miso.Types.View' under 'toHtml'; the value
--   given to 'toHtmlWith' otherwise) and the result rendered in its place.
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
    -- *** Functions
  , toHtmlWith
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
--
-- A bare 'View' has no enclosing t'Component' to supply @props@, so its
-- @props@ are fixed to @()@ (the @props ~ ()@ constraint lets a 'View' that is
-- polymorphic in @props@ still resolve this instance). A 'Miso.Types.VProps'
-- node at this level therefore sees @()@; one nested inside a mounted
-- component sees that component's real @props@.
instance (props ~ ()) => ToHtml (View context props model action) where
  toHtml = renderView
----------------------------------------------------------------------------
-- | Render a @[Miso.Types.View]@ to a @L.ByteString@
instance (props ~ ()) => ToHtml [View context props model action] where
  toHtml = foldMap renderView
----------------------------------------------------------------------------
renderView :: View context () model action -> L.ByteString
renderView = toHtmlWith ()
----------------------------------------------------------------------------
-- | Render a 'View' to a @L.ByteString@, supplying the @props@ that any
-- 'Miso.Types.VProps' node in it resolves against.
--
-- This is the general form of 'toHtml', for a 'View' whose @props@ type is
-- not @()@ — e.g. a component's 'Miso.Types.view' applied directly:
--
-- @
-- toHtmlWith props (view comp ctx props model)
-- @
--
-- @since 1.14.0.0
toHtmlWith :: props -> View context props model action -> L.ByteString
toHtmlWith props = toLazyByteString . renderBuilder props
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
-- | Serialise a 'View' given the @props@ of the t'Component' it belongs to.
-- Entering a @VComp@ \/ @VCompStatic@ switches to that child's @props@.
renderBuilder :: props -> View context props model action -> Builder
renderBuilder _ (VText _ "")    = fromMisoString " "
renderBuilder _ (VText _ s)     = fromMisoString s
renderBuilder _ (VNode _ "doctype" [] [] _) = "<!doctype html>"
renderBuilder props_ (VNode ns tag attrs children _) = mconcat
  [ "<"
  , fromMisoString tag
  , mconcat [ " " <> intercalate " " (renderAttrs <$> attrs)
            | not (Prelude.null attrs)
            ]
  , if tag `elem` selfClosing then "/>" else ">"
  , mconcat
    [ mconcat
      [ foldMap (renderBuilder props_) (collapseSiblingTextNodes props_ children)
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
renderBuilder _ (VComp someComp) = renderComp someComp
renderBuilder _ (VCompStatic (StaticMount ptr props0)) =
  case deRefStaticPtr ptr of
    SomeStaticComponent mk -> renderComp (mk props0)
renderBuilder props_ (VFrag _ kids) = foldMap (renderBuilder props_) kids
renderBuilder props_ (VContext f) =
  let ctx = unsafePerformIO (readIORef globalContext) in
  renderBuilder props_ (f ctx)
renderBuilder props_ (VProps f) = renderBuilder props_ (f props_)
----------------------------------------------------------------------------
-- | Render a mounted child component: its 'view' applied to the app-global
-- @context@, the @props@ it was mounted with, and its initial (or hydrated)
-- @model@. The enclosing component's @props@ play no part, which is why the
-- @VComp@ \/ @VCompStatic@ arms of 'renderBuilder' ignore theirs.
renderComp :: SomeComponent context -> Builder
renderComp (SomeComponent _key props comp_) =
  -- The app-global @context@ is read from 'globalContext'. For the common
  -- @context ~ ()@ case the 'Miso.Lens.view' ignores it, so the initial @undefined@ is
  -- never forced. But if a 'Miso.Lens.view' here inspects a non-trivial @context@,
  -- SSR must seed the cell with 'Miso.setContext' before serializing, or
  -- forcing @ctx@ raises an exception. See 'Miso.setContext' for details.
  let ctx = unsafePerformIO (readIORef globalContext) in
#ifdef SSR
  renderBuilder props (view comp_ ctx props (getInitialComponentModel comp_))
#else
  renderBuilder props (view comp_ ctx props (model comp_))
#endif
----------------------------------------------------------------------------
renderAttrs :: Attribute model action -> Builder
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
renderAttrs (OnStatic _) = mempty
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
collapseSiblingTextNodes
  :: props
  -> [View context props model action]
  -> [View context props model action]
collapseSiblingTextNodes props_ = go
  where
    -- Look through the wrapper constructors first, so a 'VProps' \/ 'VContext'
    -- that resolves to text is collapsed with its neighbours exactly as the
    -- client does after 'buildVTree' has resolved it. Otherwise an empty
    -- 'VText' behind a wrapper renders as a lone space that hydration
    -- cannot reconcile.
    go (VProps f : xs) = go (f props_ : xs)
    go (VContext f : xs) = go (f (unsafePerformIO (readIORef globalContext)) : xs)
    go (VText _ x : VText k y : xs) = go (VText k (x <> y) : xs)
    go (x : xs) = x : go xs
    go [] = []
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
