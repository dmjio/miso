{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Element
-- Copyright   :  (C) 2016-2018 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.Element
  ( -- * Construct an Element
      nodeHtml
    , nodeHtmlKeyed
    -- * Headers
    , h1_
    , h2_
    , h3_
    , h4_
    , h5_
    , h6_
    -- * Grouping Content
    , div_
    , p_
    , hr_
    , pre_
    , blockquote_
    -- * Text
    , code_
    , em_
    , span_
    , a_
    , strong_
    , i_
    , b_
    , u_
    , sub_
    , sup_
    , br_
    -- * Lists
    , ol_
    , ul_
    , li_
    , liKeyed_
    , dl_
    , dt_
    , dd_
    -- * Embedded Content
    , img_
    , iframe_
    , canvas_
    , math_
    , script_
    , link_
    -- * Inputs
    , select_
    , option_
    , textarea_
    , form_
    , input_
    , button_
    -- * Sections
    , section_
    , header_
    , footer_
    , nav_
    , article_
    , aside_
    , address_
    , main_
    , body_
    -- * Figures
    , figure_
    , figcaption_
    -- * Tables
    , table_
    , caption_
    , colgroup_
    , col_
    , tbody_
    , thead_
    , tfoot_
    , tr_
    , td_
    , th_
    -- * Less common elements
    , label_
    , fieldset_
    , legend_
    , datalist_
    , optgroup_
    , keygen_
    , output_
    , progress_
    , meter_
    , center_
    -- * Audio and Video
    , audio_
    , video_
    , source_
    , track_
    -- * Embedded objects
    , embed_
    , object_
    , param_
    -- * Text edits
    , ins_
    , del_
    -- * Semantic text
    , small_
    , cite_
    , dfn_
    , abbr_
    , time_
    , var_
    , samp_
    , kbd_
    , q_
    , s_
    -- * Less common tags
    , mark_
    , ruby_
    , rt_
    , rp_
    , bdi_
    , bdo_
    , wbr_
    -- * Interactive elemnts
    , details_
    , summary_
    , menuitem_
    , menu_
    ) where

import           Miso.Html.Internal
import           Miso.String (MisoString)

-- | Used to construct `VNode`'s in `View`
nodeHtml :: MisoString -> [Attribute action] -> [View action] -> View action
nodeHtml = flip (node HTML) Nothing

-- | Construct a node with a `Key`
nodeHtmlKeyed :: MisoString -> Key -> [Attribute action] -> [View action] -> View action
nodeHtmlKeyed name = node HTML name . pure

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div_ :: [Attribute action] -> [View action] -> View action
div_  = nodeHtml "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table_ :: [Attribute action] -> [View action] -> View action
table_  = nodeHtml "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead_ :: [Attribute action] -> [View action] -> View action
thead_  = nodeHtml "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody_ :: [Attribute action] -> [View action] -> View action
tbody_  = nodeHtml "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr_ :: [Attribute action] -> [View action] -> View action
tr_  = nodeHtml "tr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th_ :: [Attribute action] -> [View action] -> View action
th_  = nodeHtml "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td_ :: [Attribute action] -> [View action] -> View action
td_  = nodeHtml "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot_ :: [Attribute action] -> [View action] -> View action
tfoot_  = nodeHtml "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section_ :: [Attribute action] -> [View action] -> View action
section_  = nodeHtml "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header_ :: [Attribute action] -> [View action] -> View action
header_  = nodeHtml "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer_ :: [Attribute action] -> [View action] -> View action
footer_  = nodeHtml "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button_ :: [Attribute action] -> [View action] -> View action
button_ = nodeHtml "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form_ :: [Attribute action] -> [View action] -> View action
form_ = nodeHtml "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p_ :: [Attribute action] -> [View action] -> View action
p_ = nodeHtml "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s_ :: [Attribute action] -> [View action] -> View action
s_ = nodeHtml "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul_ :: [Attribute action] -> [View action] -> View action
ul_ = nodeHtml "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span_ :: [Attribute action] -> [View action] -> View action
span_ = nodeHtml "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong_ :: [Attribute action] -> [View action] -> View action
strong_ = nodeHtml "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li_ :: [Attribute action] -> [View action] -> View action
li_ = nodeHtml "li"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
--
liKeyed_ :: Key -> [Attribute action] -> [View action] -> View action
liKeyed_ = node HTML "li" . pure

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1_ :: [Attribute action] -> [View action] -> View action
h1_ = nodeHtml "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2_ :: [Attribute action] -> [View action] -> View action
h2_ = nodeHtml "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3_ :: [Attribute action] -> [View action] -> View action
h3_ = nodeHtml "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4_ :: [Attribute action] -> [View action] -> View action
h4_ = nodeHtml "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5_ :: [Attribute action] -> [View action] -> View action
h5_ = nodeHtml "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6_ :: [Attribute action] -> [View action] -> View action
h6_ = nodeHtml "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr_ :: [Attribute action] -> View action
hr_ = flip (nodeHtml "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre_ :: [Attribute action] -> [View action] -> View action
pre_ = nodeHtml "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: [Attribute action] -> View action
input_ = flip (nodeHtml "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: [Attribute action] -> [View action] -> View action
label_ = nodeHtml "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: [Attribute action] -> [View action] -> View action
a_ = nodeHtml "a"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark_ :: [Attribute action] -> [View action] -> View action
mark_ = nodeHtml "mark"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby_ :: [Attribute action] -> [View action] -> View action
ruby_ = nodeHtml "ruby"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt_ :: [Attribute action] -> [View action] -> View action
rt_ = nodeHtml "rt"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp_ :: [Attribute action] -> [View action] -> View action
rp_ = nodeHtml "rp"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi_ :: [Attribute action] -> [View action] -> View action
bdi_ = nodeHtml "bdi"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo_ :: [Attribute action] -> [View action] -> View action
bdo_ = nodeHtml "bdo"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr_ :: [Attribute action] -> View action
wbr_ = flip (nodeHtml "wbr") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details_ :: [Attribute action] -> [View action] -> View action
details_ = nodeHtml "details"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary_ :: [Attribute action] -> [View action] -> View action
summary_ = nodeHtml "summary"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem_ :: [Attribute action] -> [View action] -> View action
menuitem_ = nodeHtml "menuitem"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu_ :: [Attribute action] -> [View action] -> View action
menu_ = nodeHtml "menu"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset_ :: [Attribute action] -> [View action] -> View action
fieldset_ = nodeHtml "fieldset"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend_ :: [Attribute action] -> [View action] -> View action
legend_ = nodeHtml "legend"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist_ :: [Attribute action] -> [View action] -> View action
datalist_ = nodeHtml "datalist"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup_ :: [Attribute action] -> [View action] -> View action
optgroup_ = nodeHtml "optgroup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen_ :: [Attribute action] -> [View action] -> View action
keygen_ = nodeHtml "keygen"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output_ :: [Attribute action] -> [View action] -> View action
output_ = nodeHtml "output"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress_ :: [Attribute action] -> [View action] -> View action
progress_ = nodeHtml "progress"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter_ :: [Attribute action] -> [View action] -> View action
meter_ = nodeHtml "meter"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center_ :: [Attribute action] -> [View action] -> View action
center_ = nodeHtml "center"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio_ :: [Attribute action] -> [View action] -> View action
audio_ = nodeHtml "audio"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video_ :: [Attribute action] -> [View action] -> View action
video_ = nodeHtml "video"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source_ :: [Attribute action] -> View action
source_ = flip (nodeHtml "source") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track_ :: [Attribute action] -> View action
track_ = flip (nodeHtml "track") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed_ :: [Attribute action] -> View action
embed_ = flip (nodeHtml "embed") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object_ :: [Attribute action] -> [View action] -> View action
object_ = nodeHtml "object"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param_ :: [Attribute action] -> View action
param_ = flip (nodeHtml "param") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins_ :: [Attribute action] -> [View action] -> View action
ins_ = nodeHtml "ins"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del_ :: [Attribute action] -> [View action] -> View action
del_ = nodeHtml "del"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small_ :: [Attribute action] -> [View action] -> View action
small_ = nodeHtml "small"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite_ :: [Attribute action] -> [View action] -> View action
cite_ = nodeHtml "cite"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn_ :: [Attribute action] -> [View action] -> View action
dfn_ = nodeHtml "dfn"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr_ :: [Attribute action] -> [View action] -> View action
abbr_ = nodeHtml "abbr"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time_ :: [Attribute action] -> [View action] -> View action
time_ = nodeHtml "time"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var_ :: [Attribute action] -> [View action] -> View action
var_ = nodeHtml "var"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp_ :: [Attribute action] -> [View action] -> View action
samp_ = nodeHtml "samp"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd_ :: [Attribute action] -> [View action] -> View action
kbd_ = nodeHtml "kbd"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption_ :: [Attribute action] -> [View action] -> View action
caption_ = nodeHtml "caption"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup_ :: [Attribute action] -> [View action] -> View action
colgroup_ = nodeHtml "colgroup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col_ :: [Attribute action] -> View action
col_ = flip (nodeHtml "col") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav_ :: [Attribute action] -> [View action] -> View action
nav_ = nodeHtml "nav"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article_ :: [Attribute action] -> [View action] -> View action
article_ = nodeHtml "article"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside_ :: [Attribute action] -> [View action] -> View action
aside_ = nodeHtml "aside"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address_ :: [Attribute action] -> [View action] -> View action
address_ = nodeHtml "address"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [Attribute action] -> [View action] -> View action
main_ = nodeHtml "main"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body_ :: [Attribute action] -> [View action] -> View action
body_ = nodeHtml "body"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure_ :: [Attribute action] -> [View action] -> View action
figure_ = nodeHtml "figure"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption_ :: [Attribute action] -> [View action] -> View action
figcaption_ = nodeHtml "figcaption"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl_ :: [Attribute action] -> [View action] -> View action
dl_ = nodeHtml "dl"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt_ :: [Attribute action] -> [View action] -> View action
dt_ = nodeHtml "dt"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd_ :: [Attribute action] -> [View action] -> View action
dd_ = nodeHtml "dd"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img_ :: [Attribute action] -> View action
img_ = flip (nodeHtml "img") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe_ :: [Attribute action] -> [View action] -> View action
iframe_ = nodeHtml "iframe"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas_ :: [Attribute action] -> [View action] -> View action
canvas_ = nodeHtml "canvas"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math_ :: [Attribute action] -> [View action] -> View action
math_ = nodeHtml "math"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select_ :: [Attribute action] -> [View action] -> View action
select_ = nodeHtml "select"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option_ :: [Attribute action] -> [View action] -> View action
option_ = nodeHtml "option"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea_ :: [Attribute action] -> [View action] -> View action
textarea_ = nodeHtml "textarea"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub_ :: [Attribute action] -> [View action] -> View action
sub_ = nodeHtml "sub"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup_ :: [Attribute action] -> [View action] -> View action
sup_ = nodeHtml "sup"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br_ :: [Attribute action] -> View action
br_ = flip (nodeHtml "br") []
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol_ :: [Attribute action] -> [View action] -> View action
ol_ = nodeHtml "ol"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote_ :: [Attribute action] -> [View action] -> View action
blockquote_ = nodeHtml "blockquote"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code_ :: [Attribute action] -> [View action] -> View action
code_ = nodeHtml "code"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em_ :: [Attribute action] -> [View action] -> View action
em_ = nodeHtml "em"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i_ :: [Attribute action] -> [View action] -> View action
i_ = nodeHtml "i"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b_ :: [Attribute actbon] -> [View actbon] -> View actbon
b_ = nodeHtml "b"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u_ :: [Attribute actuon] -> [View actuon] -> View actuon
u_ = nodeHtml "u"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q_ :: [Attribute actqon] -> [View actqon] -> View actqon
q_ = nodeHtml "q"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script_ :: [Attribute action] -> [View action] -> View action
script_ = nodeHtml "script"
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link_ :: [Attribute action] -> View action
link_ = flip (nodeHtml "link") []
