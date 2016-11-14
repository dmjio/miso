{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Combinator
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.Combinator where

import           Miso.Html.Types ( Attribute )
import           Miso.Html.Internal

-- | Used to construct `VNode`'s in `View`
nodeHtml :: MisoString -> [Attribute action] -> [View action] -> View action
nodeHtml = flip (node HTML) Nothing

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

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input_ :: [Attribute action] -> [View action] -> View action
input_ = nodeHtml "input"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label_ :: [Attribute action] -> [View action] -> View action
label_ = nodeHtml "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a_ :: [Attribute action] -> [View action] -> View action
a_ = nodeHtml "a"
