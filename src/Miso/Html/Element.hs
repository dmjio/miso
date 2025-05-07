-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Element
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.Element
  ( -- *** Smart constructors
      nodeHtml
    , nodeHtmlKeyed
    -- *** Combinators
    , doctype_
    , html_
    , head_
    , meta_
    , h1_
    , h2_
    , h3_
    , h4_
    , h5_
    , h6_
    , div_
    , p_
    , hr_
    , pre_
    , blockquote_
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
    , ol_
    , ul_
    , li_
    , liKeyed_
    , dl_
    , dt_
    , dd_
    , img_
    , iframe_
    , canvas_
    , script_
    , link_
    , style_
    , style
    , select_
    , option_
    , textarea_
    , form
    , input_
    , button_
    , section_
    , header_
    , footer_
    , nav_
    , article_
    , aside_
    , address_
    , main_
    , body_
    , figure_
    , figcaption_
    , table_
    , caption_
    , colgroup_
    , col_
    , tbody_
    , thead_
    , tfoot_
    , tr_
    , trKeyed_
    , td_
    , th_
    , label_
    , fieldset_
    , legend_
    , datalist_
    , optgroup_
    , output_
    , progress_
    , meter_
    , mark_
    , ruby_
    , rt_
    , rp_
    , bdi_
    , bdo_
    , wbr_
    , audio_
    , video_
    , source_
    , track_
    , embed_
    , object_
    , ins_
    , del_
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
    , details_
    , summary_
    , menu_
    ) where
-----------------------------------------------------------------------------
import           Miso.Html.Types
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Used to construct @Node@ in @View@
nodeHtml :: MisoString -> [Attribute action] -> [View action] -> View action
nodeHtml nodeName = node HTML nodeName Nothing
-----------------------------------------------------------------------------
-- | Construct a node with a @Key@
nodeHtmlKeyed :: MisoString -> Key -> [Attribute action] -> [View action] -> View action
nodeHtmlKeyed name = node HTML name . pure
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/div
div_ :: [Attribute action] -> [View action] -> View action
div_  = nodeHtml "div"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/table
table_ :: [Attribute action] -> [View action] -> View action
table_  = nodeHtml "table"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/thead
thead_ :: [Attribute action] -> [View action] -> View action
thead_  = nodeHtml "thead"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tbody
tbody_ :: [Attribute action] -> [View action] -> View action
tbody_  = nodeHtml "tbody"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tr
tr_ :: [Attribute action] -> [View action] -> View action
tr_  = nodeHtml "tr"
-----------------------------------------------------------------------------
-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tr>
-----------------------------------------------------------------------------
trKeyed_ :: Key -> [Attribute action] -> [View action] -> View action
trKeyed_ = node HTML "tr" . pure
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/th
th_ :: [Attribute action] -> [View action] -> View action
th_  = nodeHtml "th"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/td
td_ :: [Attribute action] -> [View action] -> View action
td_  = nodeHtml "td"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tfoot
tfoot_ :: [Attribute action] -> [View action] -> View action
tfoot_  = nodeHtml "tfoot"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/section
section_ :: [Attribute action] -> [View action] -> View action
section_  = nodeHtml "section"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/header
header_ :: [Attribute action] -> [View action] -> View action
header_  = nodeHtml "header"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/footer
footer_ :: [Attribute action] -> [View action] -> View action
footer_  = nodeHtml "footer"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/button
button_ :: [Attribute action] -> [View action] -> View action
button_ = nodeHtml "button"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/form
--
-- For usage in a real-world application with the `onSubmit` event.
--
-- >
-- > view :: Model -> View action
-- > view model = form [ onSubmit NoOp ] [ input [ type_ "submit" ] ]
-- >
--
-- Note: @onSubmit@ will use @preventDefault = True@. This will keep
-- the form from submitting to the server.
--
form :: [Attribute action] -> [View action] -> View action
form = nodeHtml "form"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/p
p_ :: [Attribute action] -> [View action] -> View action
p_ = nodeHtml "p"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/s
s_ :: [Attribute action] -> [View action] -> View action
s_ = nodeHtml "s"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ul
ul_ :: [Attribute action] -> [View action] -> View action
ul_ = nodeHtml "ul"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/span
span_ :: [Attribute action] -> [View action] -> View action
span_ = nodeHtml "span"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/strong
strong_ :: [Attribute action] -> [View action] -> View action
strong_ = nodeHtml "strong"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/li
li_ :: [Attribute action] -> [View action] -> View action
li_ = nodeHtml "li"
-----------------------------------------------------------------------------
-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/li>
--
liKeyed_ :: Key -> [Attribute action] -> [View action] -> View action
liKeyed_ = node HTML "li" . pure
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
h1_ :: [Attribute action] -> [View action] -> View action
h1_ = nodeHtml "h1"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
h2_ :: [Attribute action] -> [View action] -> View action
h2_ = nodeHtml "h2"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
h3_ :: [Attribute action] -> [View action] -> View action
h3_ = nodeHtml "h3"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
h4_ :: [Attribute action] -> [View action] -> View action
h4_ = nodeHtml "h4"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
h5_ :: [Attribute action] -> [View action] -> View action
h5_ = nodeHtml "h5"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements
h6_ :: [Attribute action] -> [View action] -> View action
h6_ = nodeHtml "h6"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/hr
hr_ :: [Attribute action] -> View action
hr_ = flip (nodeHtml "hr") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/pre
pre_ :: [Attribute action] -> [View action] -> View action
pre_ = nodeHtml "pre"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/input
input_ :: [Attribute action] -> View action
input_ = flip (nodeHtml "input") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/label
label_ :: [Attribute action] -> [View action] -> View action
label_ = nodeHtml "label"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/a
a_ :: [Attribute action] -> [View action] -> View action
a_ = nodeHtml "a"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/mark
mark_ :: [Attribute action] -> [View action] -> View action
mark_ = nodeHtml "mark"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ruby
ruby_ :: [Attribute action] -> [View action] -> View action
ruby_ = nodeHtml "ruby"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/rt
rt_ :: [Attribute action] -> [View action] -> View action
rt_ = nodeHtml "rt"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/rp
rp_ :: [Attribute action] -> [View action] -> View action
rp_ = nodeHtml "rp"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/bdi
bdi_ :: [Attribute action] -> [View action] -> View action
bdi_ = nodeHtml "bdi"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/bdo
bdo_ :: [Attribute action] -> [View action] -> View action
bdo_ = nodeHtml "bdo"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/wbr
wbr_ :: [Attribute action] -> View action
wbr_ = flip (nodeHtml "wbr") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/details
details_ :: [Attribute action] -> [View action] -> View action
details_ = nodeHtml "details"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/summary
summary_ :: [Attribute action] -> [View action] -> View action
summary_ = nodeHtml "summary"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/menu
menu_ :: [Attribute action] -> [View action] -> View action
menu_ = nodeHtml "menu"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/fieldset
fieldset_ :: [Attribute action] -> [View action] -> View action
fieldset_ = nodeHtml "fieldset"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/legend
legend_ :: [Attribute action] -> [View action] -> View action
legend_ = nodeHtml "legend"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/datalist
datalist_ :: [Attribute action] -> [View action] -> View action
datalist_ = nodeHtml "datalist"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/optgroup
optgroup_ :: [Attribute action] -> [View action] -> View action
optgroup_ = nodeHtml "optgroup"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/output
output_ :: [Attribute action] -> [View action] -> View action
output_ = nodeHtml "output"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/progress
progress_ :: [Attribute action] -> [View action] -> View action
progress_ = nodeHtml "progress"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/meter
meter_ :: [Attribute action] -> [View action] -> View action
meter_ = nodeHtml "meter"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/audio
audio_ :: [Attribute action] -> [View action] -> View action
audio_ = nodeHtml "audio"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/video
video_ :: [Attribute action] -> [View action] -> View action
video_ = nodeHtml "video"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/source
source_ :: [Attribute action] -> View action
source_ = flip (nodeHtml "source") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/track
track_ :: [Attribute action] -> View action
track_ = flip (nodeHtml "track") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/embed
embed_ :: [Attribute action] -> View action
embed_ = flip (nodeHtml "embed") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/object
object_ :: [Attribute action] -> [View action] -> View action
object_ = nodeHtml "object"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ins
ins_ :: [Attribute action] -> [View action] -> View action
ins_ = nodeHtml "ins"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/del
del_ :: [Attribute action] -> [View action] -> View action
del_ = nodeHtml "del"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/small
small_ :: [Attribute action] -> [View action] -> View action
small_ = nodeHtml "small"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/cite
cite_ :: [Attribute action] -> [View action] -> View action
cite_ = nodeHtml "cite"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dfn
dfn_ :: [Attribute action] -> [View action] -> View action
dfn_ = nodeHtml "dfn"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/abbr
abbr_ :: [Attribute action] -> [View action] -> View action
abbr_ = nodeHtml "abbr"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/time
time_ :: [Attribute action] -> [View action] -> View action
time_ = nodeHtml "time"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/var
var_ :: [Attribute action] -> [View action] -> View action
var_ = nodeHtml "var"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/samp
samp_ :: [Attribute action] -> [View action] -> View action
samp_ = nodeHtml "samp"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/kbd
kbd_ :: [Attribute action] -> [View action] -> View action
kbd_ = nodeHtml "kbd"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/caption
caption_ :: [Attribute action] -> [View action] -> View action
caption_ = nodeHtml "caption"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/colgroup
colgroup_ :: [Attribute action] -> [View action] -> View action
colgroup_ = nodeHtml "colgroup"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/col
col_ :: [Attribute action] -> View action
col_ = flip (nodeHtml "col") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/nav
nav_ :: [Attribute action] -> [View action] -> View action
nav_ = nodeHtml "nav"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/article
article_ :: [Attribute action] -> [View action] -> View action
article_ = nodeHtml "article"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/aside
aside_ :: [Attribute action] -> [View action] -> View action
aside_ = nodeHtml "aside"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/address
address_ :: [Attribute action] -> [View action] -> View action
address_ = nodeHtml "address"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/main
main_ :: [Attribute action] -> [View action] -> View action
main_ = nodeHtml "main"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/body
body_ :: [Attribute action] -> [View action] -> View action
body_ = nodeHtml "body"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/figure
figure_ :: [Attribute action] -> [View action] -> View action
figure_ = nodeHtml "figure"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/figcaption
figcaption_ :: [Attribute action] -> [View action] -> View action
figcaption_ = nodeHtml "figcaption"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dl
dl_ :: [Attribute action] -> [View action] -> View action
dl_ = nodeHtml "dl"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dt
dt_ :: [Attribute action] -> [View action] -> View action
dt_ = nodeHtml "dt"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dd
dd_ :: [Attribute action] -> [View action] -> View action
dd_ = nodeHtml "dd"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/img
img_ :: [Attribute action] -> View action
img_ = flip (nodeHtml "img") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/iframe
iframe_ :: [Attribute action] -> [View action] -> View action
iframe_ = nodeHtml "iframe"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/canvas
canvas_ :: [Attribute action] -> [View action] -> View action
canvas_ = nodeHtml "canvas"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/select
select_ :: [Attribute action] -> [View action] -> View action
select_ = nodeHtml "select"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/option
option_ :: [Attribute action] -> [View action] -> View action
option_ = nodeHtml "option"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/textarea
textarea_ :: [Attribute action] -> [View action] -> View action
textarea_ = nodeHtml "textarea"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/sub
sub_ :: [Attribute action] -> [View action] -> View action
sub_ = nodeHtml "sub"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/sup
sup_ :: [Attribute action] -> [View action] -> View action
sup_ = nodeHtml "sup"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/br
br_ :: [Attribute action] -> View action
br_ = flip (nodeHtml "br") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ol
ol_ :: [Attribute action] -> [View action] -> View action
ol_ = nodeHtml "ol"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/blockquote
blockquote_ :: [Attribute action] -> [View action] -> View action
blockquote_ = nodeHtml "blockquote"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/code
code_ :: [Attribute action] -> [View action] -> View action
code_ = nodeHtml "code"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/em
em_ :: [Attribute action] -> [View action] -> View action
em_ = nodeHtml "em"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/i
i_ :: [Attribute action] -> [View action] -> View action
i_ = nodeHtml "i"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/b
b_ :: [Attribute action] -> [View action] -> View action
b_ = nodeHtml "b"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/u
u_ :: [Attribute action] -> [View action] -> View action
u_ = nodeHtml "u"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/q
q_ :: [Attribute action] -> [View action] -> View action
q_ = nodeHtml "q"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/link
link_ :: [Attribute action] -> View action
link_ = flip (nodeHtml "link") []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/style
--
-- This takes the raw text to be put in the style tag.
--
-- That means that if any part of the text is not trusted there's
-- a potential CSS injection. Read more at
-- https://owasp.org/www-project-web-security-testing-guide/latest/4-Web_Application_Security_Testing/11-Client_Side_Testing/05-Testing_for_CSS_Injection
--
-- You can also easily shoot yourself in the foot with something like:
--
-- @'style_' [] "\</style\>"@
style :: [Attribute action] -> MisoString -> View action
style attrs rawText = node HTML "style" Nothing attrs [textRaw rawText]
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/script
--
-- This takes the raw text to be put in the script tag.
--
-- That means that if any part of the text is not trusted there's
-- a potential JavaScript injection. Read more at
-- https://owasp.org/www-community/attacks/xss/
--
-- You can also easily shoot yourself in the foot with something like:
--
-- @'script_' [] "\</script\>"@
script_ :: [Attribute action] -> MisoString -> View action
script_ attrs rawText = node HTML "script" Nothing attrs [textRaw rawText]
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Glossary/Doctype
doctype_ :: View action
doctype_ = nodeHtml "doctype" [] []
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/html
html_ :: [Attribute action] -> [View action] -> View action
html_ = nodeHtml "html"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/head
head_ :: [Attribute action] -> [View action] -> View action
head_ = nodeHtml "head"
-----------------------------------------------------------------------------
-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/meta
meta_ :: [Attribute action] -> View action
meta_ = flip (nodeHtml "meta") []
-----------------------------------------------------------------------------
