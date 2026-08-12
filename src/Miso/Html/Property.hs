-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Property
-- Copyright   :  (C) 2016-2026 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- = Overview
--
-- "Miso.Html.Property" provides smart constructors for
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element#properties DOM properties>
-- and
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes HTML attributes>.
-- Each produces an 'Miso.Types.Attribute' that the virtual DOM applies to
-- the corresponding DOM node on every render, diffing only changed values.
--
-- All names are suffixed with @_@ to avoid clashing with Haskell
-- 'Prelude' names. This module is re-exported in its entirety by
-- "Miso.Html" and "Miso".
--
-- = Quick start
--
-- @
-- import "Miso"
--
-- view :: Model -> 'Miso.Types.View' Model Action
-- view m =
--   'Miso.Html.Element.div_' [ 'id_' \"app\", 'class_' \"container\" ]
--     [ 'Miso.Html.Element.input_'
--         [ 'type_' \"text\"
--         , 'value_' m.text
--         , 'placeholder_' \"Type here…\"
--         , 'disabled_'
--         ]
--         []
--     , 'Miso.Html.Element.img_'
--         [ 'src_' \"logo.png\", 'alt_' \"Logo\", 'width_' \"64\", 'height_' \"64\" ]
--         []
--     ]
-- @
--
-- = Class management
--
-- Four combinators handle CSS classes:
--
-- @
-- 'class_'    \"foo bar\"              -- single string, set className
-- 'className' \"foo bar\"              -- alias for class_
-- 'classes_'  [\"foo\", \"bar\"]         -- list of class names
-- 'classList_' [(\"active\", isActive)  -- conditional classes
--             ,(\"error\",  hasError)]
-- @
--
-- = Property groups
--
-- * __Global__: 'id_', 'class_', 'className', 'classes_', 'classList_',
--   'title_', 'lang_', 'hidden_', 'inert_', 'draggable_', 'tabindex_',
--   'role_', 'data_', 'aria_', 'xmlns_'
-- * __Form__: 'type_', 'value_', 'defaultValue_', 'checked_', 'placeholder_',
--   'selected_', 'disabled_', 'readonly_', 'required_', 'multiple_',
--   'autofocus_', 'autocomplete_', 'autocorrect_', 'spellcheck_',
--   'name_', 'for_', 'form_', 'action_', 'method_', 'enctype_',
--   'noValidate_', 'accept_', 'acceptCharset_', 'pattern_',
--   'min_', 'max_', 'step_', 'size_', 'maxlength_', 'minlength_',
--   'list_', 'cols_', 'rows_', 'wrap_'
-- * __Link \/ anchor__: 'href_', 'target_', 'rel_', 'hreflang_',
--   'download_', 'downloadAs_', 'ping_', 'media_'
-- * __Image \/ map__: 'src_', 'alt_', 'width_', 'height_', 'loading_',
--   'ismap_', 'usemap_', 'shape_', 'coords_'
-- * __Media__: 'autoplay_', 'controls_', 'loop_', 'muted_', 'preload_',
--   'poster_', 'volume_', 'currentTime_', 'defaultMuted_',
--   'defaultPlaybackRate_', 'playbackRate_', 'seeking_', 'mediaGroup_'
-- * __Table__: 'colspan_', 'rowspan_', 'headers_', 'scope_', 'align_'
-- * __\<script\> \/ \<meta\>__: 'async_', 'defer_', 'charset_', 'content_',
--   'httpEquiv_', 'language_', 'scoped_'
-- * __\<iframe\>__: 'sandbox_', 'seamless_', 'srcdoc_', 'frameborder_',
--   'scrolling_'
-- * __Misc__: 'open_', 'reversed_', 'default_', 'kind_', 'srclang_',
--   'label_', 'autosave_', 'formation_', 'ref_'
--
-- = See also
--
-- * "Miso.Property" — lower-level 'Miso.Property.textProp', 'Miso.Property.boolProp',
--   'Miso.Property.intProp', 'Miso.Property.doubleProp' combinators
-- * "Miso.Html.Element" — element constructors that accept these attributes
-- * "Miso.Html.Event" — event-handler attributes
-- * "Miso.CSS" — style property DSL ('Miso.CSS.style_', 'Miso.CSS.styleInline_')
-----------------------------------------------------------------------------
module Miso.Html.Property
  ( -- *** Combinators
     class_
   , className
   , classes_
   , classList_
   , id_
   , title_
   , hidden_
   , inert_
   , lang_
   , type_
   , value_
   , defaultValue_
   , checked_
   , placeholder_
   , selected_
   , accept_
   , acceptCharset_
   , action_
   , autocomplete_
   , autofocus_
   , autosave_
   , disabled_
   , enctype_
   , formation_
   , list_
   , maxlength_
   , minlength_
   , method_
   , multiple_
   , name_
   , noValidate_
   , pattern_
   , readonly_
   , required_
   , size_
   , for_
   , ref_
   , form_
   , max_
   , min_
   , step_
   , cols_
   , rows_
   , wrap_
   , href_
   , target_
   , download_
   , downloadAs_
   , hreflang_
   , media_
   , ping_
   , rel_
   , ismap_
   , usemap_
   , shape_
   , coords_
   , src_
   , height_
   , width_
   , alt_
   , loading_
   , autoplay_
   , currentTime_
   , defaultMuted_
   , volume_
   , controls_
   , loop_
   , defaultPlaybackRate_
   , mediaGroup_
   , muted_
   , playbackRate_
   , seeking_
   , preload_
   , poster_
   , default_
   , kind_
   , srclang_
   , sandbox_
   , seamless_
   , srcdoc_
   , reversed_
   , align_
   , colspan_
   , rowspan_
   , headers_
   , scope_
   , async_
   , charset_
   , content_
   , defer_
   , httpEquiv_
   , language_
   , scoped_
   , data_
   , autocorrect_
   , spellcheck_
   , role_
   , xmlns_
   , aria_
   , label_
   , draggable_
   , frameborder_
   , scrolling_
   , tabindex_
   , open_
   ) where
-----------------------------------------------------------------------------
import           Miso.Types
import           Miso.Property
-----------------------------------------------------------------------------
-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null items) ] [ ]
--
classList_ :: [(MisoString, Bool)] -> Attribute model action
classList_ xs = classList [ t | (t, True) <- xs ]
-----------------------------------------------------------------------------
-- | Define multiple classes
--
-- > div_ [ classes_ [ "red", "warning" ] ] []
--
classes_ :: [MisoString] -> Attribute model action
classes_ = classList
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/title>
title_ :: MisoString -> Attribute model action
title_ = textProp "title"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option#selected>
selected_ :: Bool -> Attribute model action
selected_ = boolProp "selected"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hidden>
hidden_ :: Bool -> Attribute model action
hidden_ = boolProp "hidden"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/inert>
inert_ :: Bool -> Attribute model action
inert_ = boolProp "inert"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang>
lang_ :: MisoString -> Attribute model action
lang_ = textProp "lang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/value>
value_ :: MisoString -> Attribute model action
value_ = textProp "value"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/defaultValue>
defaultValue_ :: MisoString -> Attribute model action
defaultValue_    = textProp "defaultValue"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/accept>
accept_ :: MisoString -> Attribute model action
accept_  = textProp "accept"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/acceptCharset>
acceptCharset_ :: MisoString -> Attribute model action
acceptCharset_   = textProp "acceptCharset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/action>
action_ :: MisoString -> Attribute model action
action_  = textProp "action"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Attributes/autocomplete>
autocomplete_ :: MisoString -> Attribute model action
autocomplete_ = textProp "autocomplete"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autosave>
autosave_ :: MisoString -> Attribute model action
autosave_ = textProp "autosave"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autocorrect>
autocorrect_ :: Bool -> Attribute model action
autocorrect_ b = textProp "autocorrect" (if b then "on" else "off")
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/spellcheck>
spellcheck_ :: Bool -> Attribute model action
spellcheck_ b = textProp "spellcheck" (if b then "true" else "false")
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/role>
role_ :: MisoString -> Attribute model action
role_ = textProp "role"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/disabled>
disabled_ :: Attribute model action
disabled_ = boolProp "disabled" True
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/enctype>
enctype_ :: MisoString -> Attribute model action
enctype_ = textProp "enctype"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/formation>
formation_ :: MisoString -> Attribute model action
formation_ = textProp "formation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/list>
list_ :: MisoString -> Attribute model action
list_  = textProp "list"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/maxlength>
maxlength_ :: MisoString -> Attribute model action
maxlength_ = textProp "maxlength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/minlength>
minlength_ :: MisoString -> Attribute model action
minlength_ = textProp "minlength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/method>
method_ :: MisoString -> Attribute model action
method_  = textProp "method"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Attributes/multiple>
multiple_ :: Bool -> Attribute model action
multiple_ = boolProp "multiple"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/noValidate>
noValidate_ :: Bool -> Attribute model action
noValidate_      = boolProp "noValidate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/pattern>
pattern_ :: MisoString -> Attribute model action
pattern_ = textProp "pattern"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/details#open>
open_ :: Bool -> Attribute model action
open_ = boolProp "open"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/readonly>
readonly_ :: Bool -> Attribute model action
readonly_ = boolProp "readOnly"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Attributes/required>
required_ :: Bool -> Attribute model action
required_ = boolProp "required"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/size>
size_ :: MisoString -> Attribute model action
size_  = textProp "size"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/for>
for_ :: MisoString -> Attribute model action
for_ = textProp "for"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/ref>
ref_ :: MisoString -> Attribute model action
ref_ = textProp "ref"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/form>
form_ :: MisoString -> Attribute model action
form_ = textProp "form"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/max>
max_ :: MisoString -> Attribute model action
max_ = textProp "max"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/min>
min_ :: MisoString -> Attribute model action
min_ = textProp "min"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/step>
step_ :: MisoString -> Attribute model action
step_  = textProp "step"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/cols>
cols_ :: MisoString -> Attribute model action
cols_  = textProp "cols"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rows>
rows_ :: MisoString -> Attribute model action
rows_  = textProp "rows"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/wrap>
wrap_ :: MisoString -> Attribute model action
wrap_  = textProp "wrap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/target>
target_ :: MisoString -> Attribute model action
target_  = textProp "target"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/download>
download_ :: MisoString -> Attribute model action
download_ = textProp "download"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/downloadAs>
downloadAs_ :: MisoString -> Attribute model action
downloadAs_      = textProp "downloadAs"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hreflang>
hreflang_ :: MisoString -> Attribute model action
hreflang_ = textProp "hreflang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/media>
media_ :: MisoString -> Attribute model action
media_ = textProp "media"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/ping>
ping_ :: MisoString -> Attribute model action
ping_  = textProp "ping"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rel>
rel_ :: MisoString -> Attribute model action
rel_ = textProp "rel"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement/isMap>
ismap_ :: Bool -> Attribute model action
ismap_ = boolProp "ismap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/usemap>
usemap_ :: MisoString -> Attribute model action
usemap_  = textProp "usemap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/shape>
shape_ :: MisoString -> Attribute model action
shape_ = textProp "shape"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/coords>
coords_ :: MisoString -> Attribute model action
coords_  = textProp "coords"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/src>
src_ :: MisoString -> Attribute model action
src_ = textProp "src"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/height>
height_ :: MisoString -> Attribute model action
height_  = textProp "height"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/width>
width_ :: MisoString -> Attribute model action
width_ = textProp "width"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/alt>
alt_ :: MisoString -> Attribute model action
alt_ = textProp "alt"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/loading>
loading_ :: MisoString -> Attribute model action
loading_ = textProp "loading"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/autoplay>
autoplay_ :: Bool -> Attribute model action
autoplay_ = boolProp "autoplay"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime>
currentTime_ :: Double -> Attribute model action
currentTime_ = doubleProp "currentTime"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/defaultMuted>
defaultMuted_ :: Bool -> Attribute model action
defaultMuted_ = boolProp "defaultMuted"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/defaultPlaybackRate>
defaultPlaybackRate_ :: Double -> Attribute model action
defaultPlaybackRate_ = doubleProp "defaultPlaybackRate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/mediaGroup>
mediaGroup_ :: MisoString -> Attribute model action
mediaGroup_ = textProp "mediaGroup"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/muted>
muted_ :: Bool -> Attribute model action
muted_ = boolProp "muted"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/playbackRate>
playbackRate_ :: Double -> Attribute model action
playbackRate_ = doubleProp "playbackRate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preload>
preload_ :: MisoString -> Attribute model action
preload_ = textProp "preload"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/seeking>
seeking_ :: Bool -> Attribute model action
seeking_ = boolProp "seeking"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/volume>
volume_ :: Double -> Attribute model action
volume_ = doubleProp "volume"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls>
controls_ :: Bool -> Attribute model action
controls_ = boolProp "controls"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/loop>
loop_ :: Bool -> Attribute model action
loop_  = boolProp "loop"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/poster>
poster_ :: MisoString -> Attribute model action
poster_  = textProp "poster"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/default>
default_ :: Bool -> Attribute model action
default_ = boolProp "default"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/kind>
kind_ :: MisoString -> Attribute model action
kind_  = textProp "kind"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/srclang>
srclang_ :: MisoString -> Attribute model action
srclang_ = textProp "srclang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/sandbox>
sandbox_ :: MisoString -> Attribute model action
sandbox_ = textProp "sandbox"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/seamless>
seamless_ :: MisoString -> Attribute model action
seamless_ = textProp "seamless"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/srcdoc>
srcdoc_ :: MisoString -> Attribute model action
srcdoc_  = textProp "srcdoc"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ol#reversed>
reversed_ :: Bool -> Attribute model action
reversed_ = boolProp "reversed"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/align>
align_ :: MisoString -> Attribute model action
align_ = textProp "align"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/colspan>
colspan_ :: MisoString -> Attribute model action
colspan_ = textProp "colspan"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rowspan>
rowspan_ :: MisoString -> Attribute model action
rowspan_ = textProp "rowspan"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/headers>
headers_ :: MisoString -> Attribute model action
headers_ = textProp "headers"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/scope>
scope_ :: MisoString -> Attribute model action
scope_ = textProp "scope"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/async>
async_ :: Bool -> Attribute model action
async_ = boolProp "async"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/charset>
charset_ :: MisoString -> Attribute model action
charset_ = textProp "charset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/content>
content_ :: MisoString -> Attribute model action
content_ = textProp "content"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLScriptElement/defer>
defer_ :: Bool -> Attribute model action
defer_ = boolProp "defer"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/httpEquiv>
httpEquiv_ :: MisoString -> Attribute model action
httpEquiv_ = textProp "httpEquiv"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/language>
language_ :: MisoString -> Attribute model action
language_ = textProp "language"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/scoped>
scoped_ :: MisoString -> Attribute model action
scoped_  = textProp "scoped"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/type>
type_ :: MisoString -> Attribute model action
type_ = textProp "type"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement/name>
name_ :: MisoString -> Attribute model action
name_ = textProp "name"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement/href>
href_ :: MisoString -> Attribute model action
href_ = textProp "href"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id>
id_ :: MisoString -> Attribute model action
id_ = textProp "id"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/placeholder>
placeholder_ :: MisoString -> Attribute model action
placeholder_ = textProp "placeholder"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/checked>
checked_ :: Bool -> Attribute model action
checked_ = boolProp "checked"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autofocus>
autofocus_ :: Bool -> Attribute model action
autofocus_ = boolProp "autofocus"
-----------------------------------------------------------------------------
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ :: MisoString -> Attribute model action
class_ = className
-----------------------------------------------------------------------------
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
className :: MisoString -> Attribute model action
className name = classList [name]
-----------------------------------------------------------------------------
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ :: MisoString -> MisoString -> Attribute model action
data_ k v = textProp ("data-" <> k) v
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
xmlns_ :: MisoString -> Attribute model action
xmlns_ = textProp "xmlns"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
aria_ :: MisoString -> MisoString -> Attribute model action
aria_ k = textProp ("aria-" <> k)
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
label_ :: MisoString -> Attribute model action
label_ = textProp "label"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
draggable_ :: Bool -> Attribute model action
draggable_ = boolProp "draggable"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
scrolling_ :: MisoString -> Attribute model action
scrolling_ = textProp "scrolling"
-----------------------------------------------------------------------------
-- | @since 1.9.0.0
frameborder_ :: MisoString -> Attribute model action
frameborder_ = textProp "frameborder"
-----------------------------------------------------------------------------
-- | [tabindex](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Attribute/tabindex) attribute
--
-- @since 1.9.0.0
tabindex_ ::  MisoString -> Attribute model action
tabindex_ = textProp "tabindex"
-----------------------------------------------------------------------------
