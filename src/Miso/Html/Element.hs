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
  ( -- ** Smart constructors
      nodeHtml
    -- ** Document metadata
    , html_
    , doctype_
    , base_
    , head_
    , link_
    , meta_
    , style_
    , title_
    -- ** Sectioning root
    , body_
    -- ** Content sectioning
    , address_
    , article_
    , aside_
    , footer_
    , header_
    , h1_
    , h2_
    , h3_
    , h4_
    , h5_
    , h6_
    , hgroup_
    , main_
    , nav_
    , section_
    , search_
    -- ** Text content
    , blockquote_
    , dd_
    , div_
    , dl_
    , dt_
    , figcaption_
    , figure_
    , hr_
    , li_
    , menu_
    , ol_
    , p_
    , pre_
    , ul_
    -- ** Inline text semantics
    , a_
    , abbr_
    , b_
    , bdi_
    , bdo_
    , br_
    , cite_
    , code_
    , data_
    , dfn_
    , em_
    , i_
    , kbd_
    , mark_
    , q_
    , rp_
    , rt_
    , ruby_
    , s_
    , samp_
    , small_
    , span_
    , strong_
    , sub_
    , sup_
    , time_
    , u_
    , var_
    , wbr_
    -- ** Image and multimedia
    , area_
    , audio_
    , img_
    , map_
    , track_
    , video_
    -- ** Embedded content
    , embed_
    , fencedframe_
    , iframe_
    , object_
    , picture_
    , source_
    -- ** Scripting
    , canvas_
    , noscript_
    , script_
    -- ** Demarcating edits
    , del_
    , ins_
    -- ** Table content
    , caption_
    , col_
    , colgroup_
    , table_
    , tbody_
    , td_
    , tfoot_
    , th_
    , thead_
    , tr_
    -- ** Forms
    , button_
    , datalist_
    , fieldset_
    , form
    , input_
    , label_
    , legend_
    , meter_
    , optgroup_
    , option_
    , output_
    , progress_
    , select_
    , textarea_
    -- ** Interactive elements
    , details_
    , dialog_
    , summary_
    -- ** Web components
    , slot_
    , template_
    -- * SVG
    , svg_
    ) where
-----------------------------------------------------------------------------
import           Miso.Types
-----------------------------------------------------------------------------
import           Miso.Svg.Element (svg_)
-----------------------------------------------------------------------------
-- | Low-level helper used to construct 'HTML' 'node' in 'View'.
-- Almost all functions in this module, like 'div_', 'table_' etc. are defined in terms of it.
nodeHtml :: MisoString -> [Attribute action] -> [View model action] -> View model action
nodeHtml nodeName = node HTML nodeName
-----------------------------------------------------------------------------
-- | [\<div\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/div)
div_ :: [Attribute action] -> [View model action] -> View model action
div_ = nodeHtml "div"
-----------------------------------------------------------------------------
-- | [\<table\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/table)
table_ :: [Attribute action] -> [View model action] -> View model action
table_ = nodeHtml "table"
-----------------------------------------------------------------------------
-- | [\<thead\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/thead)
thead_ :: [Attribute action] -> [View model action] -> View model action
thead_ = nodeHtml "thead"
-----------------------------------------------------------------------------
-- | [\<tbody\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tbody)
tbody_ :: [Attribute action] -> [View model action] -> View model action
tbody_ = nodeHtml "tbody"
-----------------------------------------------------------------------------
-- | [\<tr\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tr)
tr_ :: [Attribute action] -> [View model action] -> View model action
tr_ = nodeHtml "tr"
-----------------------------------------------------------------------------
-- | [\<th\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/th)
th_ :: [Attribute action] -> [View model action] -> View model action
th_ = nodeHtml "th"
-----------------------------------------------------------------------------
-- | [\<td\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/td)
td_ :: [Attribute action] -> [View model action] -> View model action
td_ = nodeHtml "td"
-----------------------------------------------------------------------------
-- | [\<tfoot\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/tfoot)
tfoot_ :: [Attribute action] -> [View model action] -> View model action
tfoot_ = nodeHtml "tfoot"
-----------------------------------------------------------------------------
-- | [\<section\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/section)
section_ :: [Attribute action] -> [View model action] -> View model action
section_ = nodeHtml "section"
-----------------------------------------------------------------------------
-- | [\<header\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/header)
header_ :: [Attribute action] -> [View model action] -> View model action
header_ = nodeHtml "header"
-----------------------------------------------------------------------------
-- | [\<footer\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/footer)
footer_ :: [Attribute action] -> [View model action] -> View model action
footer_ = nodeHtml "footer"
-----------------------------------------------------------------------------
-- | [\<button\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/button)
button_ :: [Attribute action] -> [View model action] -> View model action
button_ = nodeHtml "button"
-----------------------------------------------------------------------------
-- | [\<form\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/form)
--
-- For usage in a real-world application with the 'onSubmit' event.
--
-- > view :: Model -> View model action
-- > view model = form [ onSubmit NoOp ] [ input [ type_ "submit" ] ]
--
-- Note: @onSubmit@ will use @preventDefault = True@. This will keep
-- the form from submitting to the server.
--
form :: [Attribute action] -> [View model action] -> View model action
form = nodeHtml "form"
-----------------------------------------------------------------------------
-- | [\<p\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/p)
p_ :: [Attribute action] -> [View model action] -> View model action
p_ = nodeHtml "p"
-----------------------------------------------------------------------------
-- | [\<s\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/s)
s_ :: [Attribute action] -> [View model action] -> View model action
s_ = nodeHtml "s"
-----------------------------------------------------------------------------
-- | [\<ul\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ul)
ul_ :: [Attribute action] -> [View model action] -> View model action
ul_ = nodeHtml "ul"
-----------------------------------------------------------------------------
-- | [\<span\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/span)
span_ :: [Attribute action] -> [View model action] -> View model action
span_ = nodeHtml "span"
-----------------------------------------------------------------------------
-- | [\<strong\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/strong)
strong_ :: [Attribute action] -> [View model action] -> View model action
strong_ = nodeHtml "strong"
-----------------------------------------------------------------------------
-- | [\<li\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/li)
li_ :: [Attribute action] -> [View model action] -> View model action
li_ = nodeHtml "li"
-----------------------------------------------------------------------------
-- | [\<h1\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements)
h1_ :: [Attribute action] -> [View model action] -> View model action
h1_ = nodeHtml "h1"
-----------------------------------------------------------------------------
-- | [\<h2\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements)
h2_ :: [Attribute action] -> [View model action] -> View model action
h2_ = nodeHtml "h2"
-----------------------------------------------------------------------------
-- | [\<h3\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements)
h3_ :: [Attribute action] -> [View model action] -> View model action
h3_ = nodeHtml "h3"
-----------------------------------------------------------------------------
-- | [\<h4\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements)
h4_ :: [Attribute action] -> [View model action] -> View model action
h4_ = nodeHtml "h4"
-----------------------------------------------------------------------------
-- | [\<h5\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements)
h5_ :: [Attribute action] -> [View model action] -> View model action
h5_ = nodeHtml "h5"
-----------------------------------------------------------------------------
-- | [\<h6\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/Heading_Elements)
h6_ :: [Attribute action] -> [View model action] -> View model action
h6_ = nodeHtml "h6"
-----------------------------------------------------------------------------
-- | [\<hr\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/hr)
hr_ :: [Attribute action] -> View model action
hr_ = flip (nodeHtml "hr") []
-----------------------------------------------------------------------------
-- | [\<pre\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/pre)
pre_ :: [Attribute action] -> [View model action] -> View model action
pre_ = nodeHtml "pre"
-----------------------------------------------------------------------------
-- | [\<input\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/input)
input_ :: [Attribute action] -> View model action
input_ = flip (nodeHtml "input") []
-----------------------------------------------------------------------------
-- | [\<label\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/label)
label_ :: [Attribute action] -> [View model action] -> View model action
label_ = nodeHtml "label"
-----------------------------------------------------------------------------
-- | [\<a\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/a)
a_ :: [Attribute action] -> [View model action] -> View model action
a_ = nodeHtml "a"
-----------------------------------------------------------------------------
-- | [\<mark\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/mark)
mark_ :: [Attribute action] -> [View model action] -> View model action
mark_ = nodeHtml "mark"
-----------------------------------------------------------------------------
-- | [\<ruby\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ruby)
ruby_ :: [Attribute action] -> [View model action] -> View model action
ruby_ = nodeHtml "ruby"
-----------------------------------------------------------------------------
-- | [\<rt\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/rt)
rt_ :: [Attribute action] -> [View model action] -> View model action
rt_ = nodeHtml "rt"
-----------------------------------------------------------------------------
-- | [\<rp\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/rp)
rp_ :: [Attribute action] -> [View model action] -> View model action
rp_ = nodeHtml "rp"
-----------------------------------------------------------------------------
-- | [\<bdi\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/bdi)
bdi_ :: [Attribute action] -> [View model action] -> View model action
bdi_ = nodeHtml "bdi"
-----------------------------------------------------------------------------
-- | [\<bdo\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/bdo)
bdo_ :: [Attribute action] -> [View model action] -> View model action
bdo_ = nodeHtml "bdo"
-----------------------------------------------------------------------------
-- | [\<wbr\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/wbr)
wbr_ :: [Attribute action] -> View model action
wbr_ = flip (nodeHtml "wbr") []
-----------------------------------------------------------------------------
-- | [\<details\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/details)
details_ :: [Attribute action] -> [View model action] -> View model action
details_ = nodeHtml "details"
-----------------------------------------------------------------------------
-- | [\<summary\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/summary)
summary_ :: [Attribute action] -> [View model action] -> View model action
summary_ = nodeHtml "summary"
-----------------------------------------------------------------------------
-- | [\<menu\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/menu)
menu_ :: [Attribute action] -> [View model action] -> View model action
menu_ = nodeHtml "menu"
-----------------------------------------------------------------------------
-- | [\<fieldset\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/fieldset)
fieldset_ :: [Attribute action] -> [View model action] -> View model action
fieldset_ = nodeHtml "fieldset"
-----------------------------------------------------------------------------
-- | [\<legend\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/legend)
legend_ :: [Attribute action] -> [View model action] -> View model action
legend_ = nodeHtml "legend"
-----------------------------------------------------------------------------
-- | [\<datalist\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/datalist)
datalist_ :: [Attribute action] -> [View model action] -> View model action
datalist_ = nodeHtml "datalist"
-----------------------------------------------------------------------------
-- | [\<optgroup\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/optgroup)
optgroup_ :: [Attribute action] -> [View model action] -> View model action
optgroup_ = nodeHtml "optgroup"
-----------------------------------------------------------------------------
-- | [\<output\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/output)
output_ :: [Attribute action] -> [View model action] -> View model action
output_ = nodeHtml "output"
-----------------------------------------------------------------------------
-- | [\<progress\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/progress)
progress_ :: [Attribute action] -> [View model action] -> View model action
progress_ = nodeHtml "progress"
-----------------------------------------------------------------------------
-- | [\<meter\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/meter)
meter_ :: [Attribute action] -> [View model action] -> View model action
meter_ = nodeHtml "meter"
-----------------------------------------------------------------------------
-- | [\<audio\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/audio)
audio_ :: [Attribute action] -> [View model action] -> View model action
audio_ = nodeHtml "audio"
-----------------------------------------------------------------------------
-- | [\<video\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/video)
video_ :: [Attribute action] -> [View model action] -> View model action
video_ = nodeHtml "video"
-----------------------------------------------------------------------------
-- | [\<source\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/source)
source_ :: [Attribute action] -> View model action
source_ = flip (nodeHtml "source") []
-----------------------------------------------------------------------------
-- | [\<track\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/track)
track_ :: [Attribute action] -> View model action
track_ = flip (nodeHtml "track") []
-----------------------------------------------------------------------------
-- | [\<embed\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/embed)
embed_ :: [Attribute action] -> View model action
embed_ = flip (nodeHtml "embed") []
-----------------------------------------------------------------------------
-- | [\<object\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/object)
object_ :: [Attribute action] -> [View model action] -> View model action
object_ = nodeHtml "object"
-----------------------------------------------------------------------------
-- | [\<ins\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ins)
ins_ :: [Attribute action] -> [View model action] -> View model action
ins_ = nodeHtml "ins"
-----------------------------------------------------------------------------
-- | [\<del\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/del)
del_ :: [Attribute action] -> [View model action] -> View model action
del_ = nodeHtml "del"
-----------------------------------------------------------------------------
-- | [\<small\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/small)
small_ :: [Attribute action] -> [View model action] -> View model action
small_ = nodeHtml "small"
-----------------------------------------------------------------------------
-- | [\<cite\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/cite)
cite_ :: [Attribute action] -> [View model action] -> View model action
cite_ = nodeHtml "cite"
-----------------------------------------------------------------------------
-- | [\<dfn\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dfn)
dfn_ :: [Attribute action] -> [View model action] -> View model action
dfn_ = nodeHtml "dfn"
-----------------------------------------------------------------------------
-- | [\<abbr\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/abbr)
abbr_ :: [Attribute action] -> [View model action] -> View model action
abbr_ = nodeHtml "abbr"
-----------------------------------------------------------------------------
-- | [\<time\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/time)
time_ :: [Attribute action] -> [View model action] -> View model action
time_ = nodeHtml "time"
-----------------------------------------------------------------------------
-- | [\<var\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/var)
var_ :: [Attribute action] -> [View model action] -> View model action
var_ = nodeHtml "var"
-----------------------------------------------------------------------------
-- | [\<samp\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/samp)
samp_ :: [Attribute action] -> [View model action] -> View model action
samp_ = nodeHtml "samp"
-----------------------------------------------------------------------------
-- | [\<kbd\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/kbd)
kbd_ :: [Attribute action] -> [View model action] -> View model action
kbd_ = nodeHtml "kbd"
-----------------------------------------------------------------------------
-- | [\<caption\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/caption)
caption_ :: [Attribute action] -> [View model action] -> View model action
caption_ = nodeHtml "caption"
-----------------------------------------------------------------------------
-- | [\<colgroup\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/colgroup)
colgroup_ :: [Attribute action] -> [View model action] -> View model action
colgroup_ = nodeHtml "colgroup"
-----------------------------------------------------------------------------
-- | [\<col\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/col)
col_ :: [Attribute action] -> View model action
col_ = flip (nodeHtml "col") []
-----------------------------------------------------------------------------
-- | [\<nav\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/nav)
nav_ :: [Attribute action] -> [View model action] -> View model action
nav_ = nodeHtml "nav"
-----------------------------------------------------------------------------
-- | [\<article\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/article)
article_ :: [Attribute action] -> [View model action] -> View model action
article_ = nodeHtml "article"
-----------------------------------------------------------------------------
-- | [\<aside\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/aside)
aside_ :: [Attribute action] -> [View model action] -> View model action
aside_ = nodeHtml "aside"
-----------------------------------------------------------------------------
-- | [\<address\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/address)
address_ :: [Attribute action] -> [View model action] -> View model action
address_ = nodeHtml "address"
-----------------------------------------------------------------------------
-- | [\<main\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/main)
main_ :: [Attribute action] -> [View model action] -> View model action
main_ = nodeHtml "main"
-----------------------------------------------------------------------------
-- | [\<body\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/body)
body_ :: [Attribute action] -> [View model action] -> View model action
body_ = nodeHtml "body"
-----------------------------------------------------------------------------
-- | [\<figure\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/figure)
figure_ :: [Attribute action] -> [View model action] -> View model action
figure_ = nodeHtml "figure"
-----------------------------------------------------------------------------
-- | [\<figcaption\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/figcaption)
figcaption_ :: [Attribute action] -> [View model action] -> View model action
figcaption_ = nodeHtml "figcaption"
-----------------------------------------------------------------------------
-- | [\<dl\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dl)
dl_ :: [Attribute action] -> [View model action] -> View model action
dl_ = nodeHtml "dl"
-----------------------------------------------------------------------------
-- | [\<dt\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dt)
dt_ :: [Attribute action] -> [View model action] -> View model action
dt_ = nodeHtml "dt"
-----------------------------------------------------------------------------
-- | [\<dd\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dd)
dd_ :: [Attribute action] -> [View model action] -> View model action
dd_ = nodeHtml "dd"
-----------------------------------------------------------------------------
-- | [\<img\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/img)
img_ :: [Attribute action] -> View model action
img_ = flip (nodeHtml "img") []
-----------------------------------------------------------------------------
-- | [\<iframe\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/iframe)
iframe_ :: [Attribute action] -> [View model action] -> View model action
iframe_ = nodeHtml "iframe"
-----------------------------------------------------------------------------
-- | [\<canvas\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/canvas)
--
-- Note this just renders a canvas element.
-- See also 'Miso.Canvas.canvas_' which supports canvas drawing DSL.
canvas_ :: [Attribute action] -> [View model action] -> View model action
canvas_ = nodeHtml "canvas"
-----------------------------------------------------------------------------
-- | [\<select\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/select)
select_ :: [Attribute action] -> [View model action] -> View model action
select_ = nodeHtml "select"
-----------------------------------------------------------------------------
-- | [\<option\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/option)
option_ :: [Attribute action] -> [View model action] -> View model action
option_ = nodeHtml "option"
-----------------------------------------------------------------------------
-- | [\<textarea\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/textarea)
textarea_ :: [Attribute action] -> [View model action] -> View model action
textarea_ = nodeHtml "textarea"
-----------------------------------------------------------------------------
-- | [\<sub\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/sub)
sub_ :: [Attribute action] -> [View model action] -> View model action
sub_ = nodeHtml "sub"
-----------------------------------------------------------------------------
-- | [\<sup\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/sup)
sup_ :: [Attribute action] -> [View model action] -> View model action
sup_ = nodeHtml "sup"
-----------------------------------------------------------------------------
-- | [\<br\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/br)
br_ :: [Attribute action] -> View model action
br_ = flip (nodeHtml "br") []
-----------------------------------------------------------------------------
-- | [\<ol\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/ol)
ol_ :: [Attribute action] -> [View model action] -> View model action
ol_ = nodeHtml "ol"
-----------------------------------------------------------------------------
-- | [\<blockquote\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/blockquote)
blockquote_ :: [Attribute action] -> [View model action] -> View model action
blockquote_ = nodeHtml "blockquote"
-----------------------------------------------------------------------------
-- | [\<code\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/code)
code_ :: [Attribute action] -> [View model action] -> View model action
code_ = nodeHtml "code"
-----------------------------------------------------------------------------
-- | [\<em\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/em)
em_ :: [Attribute action] -> [View model action] -> View model action
em_ = nodeHtml "em"
-----------------------------------------------------------------------------
-- | [\<i\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/i)
i_ :: [Attribute action] -> [View model action] -> View model action
i_ = nodeHtml "i"
-----------------------------------------------------------------------------
-- | [\<b\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/b)
b_ :: [Attribute action] -> [View model action] -> View model action
b_ = nodeHtml "b"
-----------------------------------------------------------------------------
-- | [\<u\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/u)
u_ :: [Attribute action] -> [View model action] -> View model action
u_ = nodeHtml "u"
-----------------------------------------------------------------------------
-- | [\<q\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/q)
q_ :: [Attribute action] -> [View model action] -> View model action
q_ = nodeHtml "q"
-----------------------------------------------------------------------------
-- | [\<link\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/link)
link_ :: [Attribute action] -> View model action
link_ = flip (nodeHtml "link") []
-----------------------------------------------------------------------------
-- | [\<style\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/style)
--
-- This takes the raw text to be put in the style tag.
--
-- That means that if any part of the text is not trusted there's
-- a potential CSS injection. Read more at
-- https://owasp.org/www-project-web-security-testing-guide/stable/4-Web_Application_Security_Testing/11-Client-side_Testing/05-Testing_for_CSS_Injection
--
-- You can also easily shoot yourself in the foot with something like:
--
-- @'style_' [] "\</style\>"@
style_ :: [Attribute action] -> MisoString -> View model action
style_ attrs rawText = node HTML "style" attrs [textRaw rawText]
-----------------------------------------------------------------------------
-- | [\<script\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/script)
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
script_ :: [Attribute action] -> MisoString -> View model action
script_ attrs rawText = node HTML "script" attrs [textRaw rawText]
-----------------------------------------------------------------------------
-- | [\<doctype\>](https://developer.mozilla.org/en-US/docs/Glossary/Doctype)
doctype_ :: View model action
doctype_ = nodeHtml "doctype" [] []
-----------------------------------------------------------------------------
-- | [\<html\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/html)
html_ :: [Attribute action] -> [View model action] -> View model action
html_ = nodeHtml "html"
-----------------------------------------------------------------------------
-- | [\<head\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/head)
head_ :: [Attribute action] -> [View model action] -> View model action
head_ = nodeHtml "head"
-----------------------------------------------------------------------------
-- | [\<meta\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/meta)
meta_ :: [Attribute action] -> View model action
meta_ = flip (nodeHtml "meta") []
-----------------------------------------------------------------------------
-- | [\<area\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/area)
--
-- @since 1.9.0.0
area_ :: [Attribute action] -> [View model action] -> View model action
area_ = nodeHtml "area"
-----------------------------------------------------------------------------
-- | [\<base\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/base)
--
-- @since 1.9.0.0
base_ :: [Attribute action] -> [View model action] -> View model action
base_ = nodeHtml "base"
-----------------------------------------------------------------------------
-- | [\<data\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/data)
--
-- @since 1.9.0.0
data_ :: [Attribute action] -> [View model action] -> View model action
data_ = nodeHtml "data"
-----------------------------------------------------------------------------
-- | [\<dialog\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/dialog)
--
-- @since 1.9.0.0
dialog_ :: [Attribute action] -> [View model action] -> View model action
dialog_ = nodeHtml "dialog"
-----------------------------------------------------------------------------
-- | [\<fencedframe\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/fencedframe)
--
-- @since 1.9.0.0
fencedframe_ :: [Attribute action] -> [View model action] -> View model action
fencedframe_ = nodeHtml "fencedframe"
-----------------------------------------------------------------------------
-- | [\<hgroup\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/hgroup)
--
-- @since 1.9.0.0
hgroup_ :: [Attribute action] -> [View model action] -> View model action
hgroup_ = nodeHtml "hgroup"
-----------------------------------------------------------------------------
-- | [\<map\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/map)
--
-- @since 1.9.0.0
map_ :: [Attribute action] -> [View model action] -> View model action
map_ = nodeHtml "map"
-----------------------------------------------------------------------------
-- | [\<noscript\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/noscript)
--
-- @since 1.9.0.0
noscript_ :: [Attribute action] -> [View model action] -> View model action
noscript_ = nodeHtml "noscript"
-----------------------------------------------------------------------------
-- | [\<picture\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/picture)
--
-- @since 1.9.0.0
picture_ :: [Attribute action] -> [View model action] -> View model action
picture_ = nodeHtml "picture"
-----------------------------------------------------------------------------
-- | [\<search\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/search)
--
-- @since 1.9.0.0
search_ :: [Attribute action] -> [View model action] -> View model action
search_ = nodeHtml "search"
-----------------------------------------------------------------------------
-- | [\<slot\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/slot)
--
-- @since 1.9.0.0
slot_ :: [Attribute action] -> [View model action] -> View model action
slot_ = nodeHtml "slot"
-----------------------------------------------------------------------------
-- | [\<template\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/template)
--
-- @since 1.9.0.0
template_ :: [Attribute action] -> [View model action] -> View model action
template_ = nodeHtml "template"
-----------------------------------------------------------------------------
-- | [\<title\>](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/title)
--
-- @since 1.9.0.0
title_ :: [Attribute action] -> [View model action] -> View model action
title_ = nodeHtml "title"
-----------------------------------------------------------------------------
