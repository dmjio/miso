-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Types
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
module Miso.Html.Types
  ( -- *** Types
    VTree     (..)
  , View      (..)
  , Attribute (..)
  , Key       (..)
  , NS        (..)
  -- *** Classes
  , ToView    (..)
  -- *** Combinators
  , node
  , text
  , textRaw
  , rawHtml
  , prop
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson (ToJSON(..))
import           Data.Map.Strict (Map)
import           Language.Javascript.JSaddle (Object)
-----------------------------------------------------------------------------
import           Miso.String hiding (reverse)
import           Miso.Types
-----------------------------------------------------------------------------
-- | Create a new @Miso.Html.Types.TextRaw@.
--
-- @expandable@
-- a 'rawHtml' node takes raw HTML and attempts to convert it to a 'VTree'
-- at runtime. This is a way to dynamically populate the virtual DOM from
-- HTML received at runtime. If rawHtml cannot parse the HTML it will not render.
rawHtml
  :: MisoString
  -> View action
rawHtml = TextRaw
-----------------------------------------------------------------------------
-- | Create a new @Miso.Html.Types.Node@.
--
-- @node ns tag key attrs children@ creates a new node with tag @tag@
-- and 'Key' @key@ in the namespace @ns@. All @attrs@ are called when
-- the node is created and its children are initialized to @children@.
node :: NS
     -> MisoString
     -> Maybe Key
     -> [Attribute action]
     -> [View action]
     -> View action
node = Node
-----------------------------------------------------------------------------
-- | Create a new @Text@ with the given content.
text :: MisoString -> View action
text = Text
-----------------------------------------------------------------------------
-- | `TextRaw` creation. Don't use directly
textRaw :: MisoString -> View action
textRaw = TextRaw
-----------------------------------------------------------------------------
-- | Virtual DOM implemented as a JavaScript `Object`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree = VTree { getTree :: Object }
-----------------------------------------------------------------------------
-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM node associated with the vnode
-- to @v@.
prop :: ToJSON a => MisoString -> a -> Attribute action
prop k v = Property k (toJSON v)
-----------------------------------------------------------------------------
