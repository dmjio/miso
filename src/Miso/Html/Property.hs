{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Property
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Construct custom properties on DOM elements
--
-- > div_ [ prop "id" "foo" ] [ ]
--
----------------------------------------------------------------------------
module Miso.Html.Property
 (   -- * Construction
     textProp
   , stringProp
   , boolProp
   , intProp
   , integerProp
   , doubleProp
    -- * Common attributes
   , class_
   , classList_
   , id_
   , title_
   , hidden_
   -- * Inputs
   , type_
   , value_
   , defaultValue_
   , checked_
   , placeholder_
   , selected_
   -- * Input Helpers
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
   , form_
   -- * Input Ranges
   , max_
   , min_
   , step_
   -- * Input Text areas
   , cols_
   , rows_
   , wrap_
   -- * Links and areas
   , href_
   , target_
   , download_
   , downloadAs_
   , hreflang_
   , media_
   , ping_
   , rel_
   -- * Maps
   , ismap_
   , usemap_
   , shape_
   , coords_
   -- * Embedded Content
   , src_
   , height_
   , width_
   , alt_
   -- * Audio and Video
   , autoplay_
   , controls_
   , loop_
   , preload_
   , poster_
   , default_
   , kind_
   , srclang_
   -- * iframes
   , sandbox_
   , seamless_
   , srcdoc_
   -- * Ordered lists
   , reversed_
   , start_
   -- * Tables
   , align_
   , colspan_
   , rowspan_
   , headers_
   , scope_
   -- * Headers
   , async_
   , charset_
   , content_
   , defer_
   , httpEquiv_
   , language_
   , scoped_
   ) where

import           Miso.Html.Internal
import           Miso.String (MisoString, intercalate)

-- | Set field to `Bool` value
boolProp :: MisoString -> Bool -> Attribute action
boolProp = prop
-- | Set field to `String` value
stringProp ::  MisoString -> String -> Attribute action
stringProp = prop
-- | Set field to `Text` value
textProp ::  MisoString -> MisoString -> Attribute action
textProp = prop
-- | Set field to `Int` value
intProp ::  MisoString -> Int -> Attribute action
intProp = prop
-- | Set field to `Integer` value
integerProp ::  MisoString -> Int -> Attribute action
integerProp = prop
-- | Set field to `Double` value
doubleProp ::  MisoString -> Double -> Attribute action
doubleProp = prop
-- | Define multiple classes conditionally
--
-- > div_ [ classList_ [ ("empty", null items) ] [ ]
--
classList_ ::  [(MisoString, Bool)] -> Attribute action
classList_ xs =
  textProp "class" $ intercalate (" " :: MisoString) [ t | (t, True) <- xs ]
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/title>
title_ ::  MisoString -> Attribute action
title_ = textProp "title"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/selected>
selected_ ::  Bool -> Attribute action
selected_ = boolProp "selected"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hidden>
hidden_ ::  Bool -> Attribute action
hidden_             = boolProp "hidden"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/value>
value_ ::  MisoString -> Attribute action
value_             = textProp "value"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defaultValue>
defaultValue_ ::  MisoString -> Attribute action
defaultValue_      = textProp "defaultValue"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/accept>
accept_ ::  MisoString -> Attribute action
accept_            = textProp "accept"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/acceptCharset>
acceptCharset_ ::  MisoString -> Attribute action
acceptCharset_     = textProp "acceptCharset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/action>
action_ ::  MisoString -> Attribute action
action_            = textProp "action"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autocomplete>
autocomplete_ ::  Bool -> Attribute action
autocomplete_ b = textProp "autocomplete" (if b then "on" else "off")
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autosave>
autosave_ ::  MisoString -> Attribute action
autosave_          = textProp "autosave"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/disabled>
disabled_ ::  Bool -> Attribute action
disabled_          = boolProp "disabled"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/enctype>
enctype_ ::  MisoString -> Attribute action
enctype_           = textProp "enctype"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/formation>
formation_ ::  MisoString -> Attribute action
formation_         = textProp "formation"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/list>
list_ ::  MisoString -> Attribute action
list_              = textProp "list"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/maxlength>
maxlength_ ::  MisoString -> Attribute action
maxlength_         = textProp "maxlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/minlength>
minlength_ ::  MisoString -> Attribute action
minlength_         = textProp "minlength"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/method>
method_ ::  MisoString -> Attribute action
method_            = textProp "method"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/multiple>
multiple_ ::  Bool -> Attribute action
multiple_          = boolProp "multiple"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/novalidate>
novalidate_ ::  Bool -> Attribute action
novalidate_        = boolProp "noValidate"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/pattern>
pattern_ ::  MisoString -> Attribute action
pattern_           = textProp "pattern"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/readonly>
readonly_ ::  Bool -> Attribute action
readonly_          = boolProp "readOnly"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/required>
required_ ::  Bool -> Attribute action
required_          = boolProp "required"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/size>
size_ ::  MisoString -> Attribute action
size_              = textProp "size"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/for>
for_ ::  MisoString -> Attribute action
for_               = textProp "for"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/form>
form_ ::  MisoString -> Attribute action
form_               = textProp "form"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/max>
max_ ::  MisoString -> Attribute action
max_               = textProp "max"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/min>
min_ ::  MisoString -> Attribute action
min_               = textProp "min"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/step>
step_ ::  MisoString -> Attribute action
step_              = textProp "step"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/cols>
cols_ ::  MisoString -> Attribute action
cols_              = textProp "cols"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rows>
rows_ ::  MisoString -> Attribute action
rows_              = textProp "rows"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/wrap>
wrap_ ::  MisoString -> Attribute action
wrap_              = textProp "wrap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/target>
target_ ::  MisoString -> Attribute action
target_            = textProp "target"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/download>
download_ ::  MisoString -> Attribute action
download_          = textProp "download"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/downloadAs>
downloadAs_ ::  MisoString -> Attribute action
downloadAs_        = textProp "downloadAs"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/hreflang>
hreflang_ ::  MisoString -> Attribute action
hreflang_          = textProp "hreflang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/media>
media_ ::  MisoString -> Attribute action
media_             = textProp "media"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ping>
ping_ ::  MisoString -> Attribute action
ping_              = textProp "ping"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rel>
rel_ ::  MisoString -> Attribute action
rel_               = textProp "rel"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/ismap>
ismap_ ::  MisoString -> Attribute action
ismap_             = textProp "ismap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/usemap>
usemap_ ::  MisoString -> Attribute action
usemap_            = textProp "usemap"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/shape>
shape_ ::  MisoString -> Attribute action
shape_             = textProp "shape"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/coords>
coords_ ::  MisoString -> Attribute action
coords_            = textProp "coords"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/src>
src_ ::  MisoString -> Attribute action
src_               = textProp "src"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/height>
height_ ::  MisoString -> Attribute action
height_            = textProp "height"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/width>
width_ ::  MisoString -> Attribute action
width_             = textProp "width"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/alt>
alt_ ::  MisoString -> Attribute action
alt_               = textProp "alt"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autoplay>
autoplay_ ::  Bool -> Attribute action
autoplay_          = boolProp "autoplay"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/controls>
controls_ ::  Bool -> Attribute action
controls_          = boolProp "controls"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/loop>
loop_ ::  Bool -> Attribute action
loop_              = boolProp "loop"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/preload>
preload_ ::  MisoString -> Attribute action
preload_           = textProp "preload"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/poster>
poster_ ::  MisoString -> Attribute action
poster_            = textProp "poster"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/default>
default_ ::  Bool -> Attribute action
default_           = boolProp "default"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/kind>
kind_ ::  MisoString -> Attribute action
kind_              = textProp "kind"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srclang>
srclang_ ::  MisoString -> Attribute action
srclang_           = textProp "srclang"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/sandbox>
sandbox_ ::  MisoString -> Attribute action
sandbox_           = textProp "sandbox"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/seamless>
seamless_ ::  MisoString -> Attribute action
seamless_          = textProp "seamless"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/srcdoc>
srcdoc_ ::  MisoString -> Attribute action
srcdoc_            = textProp "srcdoc"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/reversed>
reversed_ ::  MisoString -> Attribute action
reversed_          = textProp "reversed"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/start>
start_ ::  MisoString -> Attribute action
start_             = textProp "start"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/align>
align_ ::  MisoString -> Attribute action
align_             = textProp "align"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/colspan>
colspan_ ::  MisoString -> Attribute action
colspan_           = textProp "colspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/rowspan>
rowspan_ ::  MisoString -> Attribute action
rowspan_           = textProp "rowspan"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/headers>
headers_ ::  MisoString -> Attribute action
headers_           = textProp "headers"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scope>
scope_ ::  MisoString -> Attribute action
scope_             = textProp "scope"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/async>
async_ ::  MisoString -> Attribute action
async_             = textProp "async"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/charset>
charset_ ::  MisoString -> Attribute action
charset_           = textProp "charset"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/content>
content_ ::  MisoString -> Attribute action
content_           = textProp "content"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/defer>
defer_ ::  MisoString -> Attribute action
defer_             = textProp "defer"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/httpEquiv>
httpEquiv_ ::  MisoString -> Attribute action
httpEquiv_         = textProp "httpEquiv"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/language>
language_ ::  MisoString -> Attribute action
language_          = textProp "language"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/scoped>
scoped_ ::  MisoString -> Attribute action
scoped_            = textProp "scoped"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/type>
type_ ::  MisoString -> Attribute action
type_ = textProp "type"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/name>
name_ ::  MisoString -> Attribute action
name_ = textProp "name"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/href>
href_ ::  MisoString -> Attribute action
href_ = textProp "href"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/id>
id_ ::  MisoString -> Attribute action
id_ = textProp "id"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/placeholder>
placeholder_ ::  MisoString -> Attribute action
placeholder_ = textProp "placeholder"
-- | <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/checked>
checked_ ::  Bool -> Attribute action
checked_ = boolProp "checked"
-- | Set "autofocus" property
-- <https://developer.mozilla.org/en-US/docs/Mozilla/Tech/XUL/Attribute/autofocus>
autofocus_ ::  Bool -> Attribute action
autofocus_ = boolProp "autofocus"
-- | Set "className" property
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element/className>
class_ ::  MisoString -> Attribute action
class_ = textProp "class"
