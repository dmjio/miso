-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Construct custom properties on DOM elements
--
-- > div_ [ prop "id" "foo" ] [ ]
--
----------------------------------------------------------------------------
module Miso.Html.Property
  ( -- *** Combinators
     class_
   , classList_
   , id_
   , title_
   , hidden_
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
   , novalidate_
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
   ) where
-----------------------------------------------------------------------------
import           Miso.Html.Types
import           Miso.Property
import           Miso.String (MisoString, intercalate)
-----------------------------------------------------------------------------
-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null items) ] [ ]
--
classList_ ::  [(MisoString, Bool)] -> Attribute action
classList_ xs =
  textProp "class" $ intercalate (" " :: MisoString) [ t | (t, True) <- xs ]
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/title>
title_ ::  MisoString -> Attribute action
title_ = textProp "title"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option#selected>
selected_ ::  Bool -> Attribute action
selected_ = boolProp "selected"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hidden>
hidden_ ::  Bool -> Attribute action
hidden_             = boolProp "hidden"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang>
lang_ ::  MisoString -> Attribute action
lang_             = textProp "lang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/value>
value_ ::  MisoString -> Attribute action
value_             = textProp "value"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/defaultValue>
defaultValue_ ::  MisoString -> Attribute action
defaultValue_      = textProp "defaultValue"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/accept>
accept_ ::  MisoString -> Attribute action
accept_            = textProp "accept"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/acceptCharset>
acceptCharset_ ::  MisoString -> Attribute action
acceptCharset_     = textProp "acceptCharset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/action>
action_ ::  MisoString -> Attribute action
action_            = textProp "action"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autocomplete>
autocomplete_ ::  Bool -> Attribute action
autocomplete_ b = textProp "autocomplete" (if b then "on" else "off")
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autosave>
autosave_ ::  MisoString -> Attribute action
autosave_          = textProp "autosave"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/disabled>
disabled_ ::  Bool -> Attribute action
disabled_          = boolProp "disabled"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement/enctype>
enctype_ ::  MisoString -> Attribute action
enctype_           = textProp "enctype"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/formation>
formation_ ::  MisoString -> Attribute action
formation_         = textProp "formation"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/list>
list_ ::  MisoString -> Attribute action
list_              = textProp "list"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/maxlength>
maxlength_ ::  MisoString -> Attribute action
maxlength_         = textProp "maxlength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/minlength>
minlength_ ::  MisoString -> Attribute action
minlength_         = textProp "minlength"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/method>
method_ ::  MisoString -> Attribute action
method_            = textProp "method"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/multiple>
multiple_ ::  Bool -> Attribute action
multiple_          = boolProp "multiple"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/novalidate>
novalidate_ ::  Bool -> Attribute action
novalidate_        = boolProp "noValidate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/pattern>
pattern_ ::  MisoString -> Attribute action
pattern_           = textProp "pattern"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/readonly>
readonly_ ::  Bool -> Attribute action
readonly_          = boolProp "readOnly"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/required>
required_ ::  Bool -> Attribute action
required_          = boolProp "required"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/size>
size_ ::  MisoString -> Attribute action
size_              = textProp "size"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/for>
for_ ::  MisoString -> Attribute action
for_               = textProp "for"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/ref>
ref_ ::  MisoString -> Attribute action
ref_               = textProp "ref"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/form>
form_ ::  MisoString -> Attribute action
form_               = textProp "form"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/max>
max_ ::  MisoString -> Attribute action
max_               = textProp "max"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/min>
min_ ::  MisoString -> Attribute action
min_               = textProp "min"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/step>
step_ ::  MisoString -> Attribute action
step_              = textProp "step"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/cols>
cols_ ::  MisoString -> Attribute action
cols_              = textProp "cols"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rows>
rows_ ::  MisoString -> Attribute action
rows_              = textProp "rows"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/wrap>
wrap_ ::  MisoString -> Attribute action
wrap_              = textProp "wrap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/target>
target_ ::  MisoString -> Attribute action
target_            = textProp "target"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/download>
download_ ::  MisoString -> Attribute action
download_          = textProp "download"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/downloadAs>
downloadAs_ ::  MisoString -> Attribute action
downloadAs_        = textProp "downloadAs"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/hreflang>
hreflang_ ::  MisoString -> Attribute action
hreflang_          = textProp "hreflang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/media>
media_ ::  MisoString -> Attribute action
media_             = textProp "media"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/ping>
ping_ ::  MisoString -> Attribute action
ping_              = textProp "ping"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rel>
rel_ ::  MisoString -> Attribute action
rel_               = textProp "rel"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/ismap>
ismap_ ::  MisoString -> Attribute action
ismap_             = textProp "ismap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/usemap>
usemap_ ::  MisoString -> Attribute action
usemap_            = textProp "usemap"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/shape>
shape_ ::  MisoString -> Attribute action
shape_             = textProp "shape"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/coords>
coords_ ::  MisoString -> Attribute action
coords_            = textProp "coords"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/src>
src_ ::  MisoString -> Attribute action
src_               = textProp "src"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/height>
height_ ::  MisoString -> Attribute action
height_            = textProp "height"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/width>
width_ ::  MisoString -> Attribute action
width_             = textProp "width"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/alt>
alt_ ::  MisoString -> Attribute action
alt_               = textProp "alt"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/loading>
loading_ ::  MisoString -> Attribute action
loading_           = textProp "loading"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/autoplay>
autoplay_ ::  Bool -> Attribute action
autoplay_          = boolProp "autoplay"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime>
currentTime_ ::  Double -> Attribute action
currentTime_          = doubleProp "currentTime"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/defaultMuted>
defaultMuted_ ::  Bool -> Attribute action
defaultMuted_          = boolProp "defaultMuted"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/defaultPlaybackRate>
defaultPlaybackRate_ ::  Double -> Attribute action
defaultPlaybackRate_          = doubleProp "defaultPlaybackRate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/mediaGroup>
mediaGroup_ :: MisoString -> Attribute action
mediaGroup_ = textProp "mediaGroup"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/muted>
muted_ :: Bool -> Attribute action
muted_ = boolProp "muted"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/playbackRate>
playbackRate_ :: Double -> Attribute action
playbackRate_ = doubleProp "playbackRate"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/preload>
preload_ :: MisoString -> Attribute action
preload_ = textProp "preload"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/seeking>
seeking_ :: Bool -> Attribute action
seeking_ = boolProp "seeking"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/volume>
volume_ :: Double -> Attribute action
volume_ = doubleProp "volume"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/controls>
controls_ ::  Bool -> Attribute action
controls_          = boolProp "controls"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/loop>
loop_ ::  Bool -> Attribute action
loop_              = boolProp "loop"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement/poster>
poster_ ::  MisoString -> Attribute action
poster_            = textProp "poster"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/default>
default_ ::  Bool -> Attribute action
default_           = boolProp "default"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/kind>
kind_ ::  MisoString -> Attribute action
kind_              = textProp "kind"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/srclang>
srclang_ ::  MisoString -> Attribute action
srclang_           = textProp "srclang"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/sandbox>
sandbox_ ::  MisoString -> Attribute action
sandbox_           = textProp "sandbox"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/seamless>
seamless_ ::  MisoString -> Attribute action
seamless_          = textProp "seamless"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/srcdoc>
srcdoc_ ::  MisoString -> Attribute action
srcdoc_            = textProp "srcdoc"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/reversed>
reversed_ ::  MisoString -> Attribute action
reversed_          = textProp "reversed"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/align>
align_ ::  MisoString -> Attribute action
align_             = textProp "align"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/colspan>
colspan_ ::  MisoString -> Attribute action
colspan_           = textProp "colspan"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/rowspan>
rowspan_ ::  MisoString -> Attribute action
rowspan_           = textProp "rowspan"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/headers>
headers_ ::  MisoString -> Attribute action
headers_           = textProp "headers"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/scope>
scope_ ::  MisoString -> Attribute action
scope_             = textProp "scope"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/async>
async_ ::  MisoString -> Attribute action
async_             = textProp "async"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/charset>
charset_ ::  MisoString -> Attribute action
charset_           = textProp "charset"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/content>
content_ ::  MisoString -> Attribute action
content_           = textProp "content"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/defer>
defer_ ::  MisoString -> Attribute action
defer_             = textProp "defer"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/httpEquiv>
httpEquiv_ ::  MisoString -> Attribute action
httpEquiv_         = textProp "httpEquiv"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/language>
language_ ::  MisoString -> Attribute action
language_          = textProp "language"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/scoped>
scoped_ ::  MisoString -> Attribute action
scoped_            = textProp "scoped"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/type>
type_ ::  MisoString -> Attribute action
type_ = textProp "type"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement/name>
name_ ::  MisoString -> Attribute action
name_ = textProp "name"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement/href>
href_ ::  MisoString -> Attribute action
href_ = textProp "href"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/id>
id_ ::  MisoString -> Attribute action
id_ = textProp "id"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/placeholder>
placeholder_ ::  MisoString -> Attribute action
placeholder_ = textProp "placeholder"
-----------------------------------------------------------------------------
-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement/checked>
checked_ ::  Bool -> Attribute action
checked_ = boolProp "checked"
-----------------------------------------------------------------------------
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/autofocus>
autofocus_ ::  Bool -> Attribute action
autofocus_ = boolProp "autofocus"
-----------------------------------------------------------------------------
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  MisoString -> Attribute action
class_ = textProp "class"
-----------------------------------------------------------------------------
-- | Set "data-*" property
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/data-*
data_ ::  MisoString -> MisoString -> Attribute action
data_ k v = textProp ("data-" <> k) v
-----------------------------------------------------------------------------
