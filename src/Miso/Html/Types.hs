{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Html.Types
  ( -- *** Types
    VTree     (..)
  , View      (..)
  , Attribute (..)
  , Key       (..)
  , NS        (..)
  -- *** Classes
  , ToView    (..)
  -- *** Smart Constructors
  , node
  , text
  , textRaw
  , rawHtml
  , prop
  , style_
  ) where

import           Data.Aeson (ToJSON(..))
import           Data.Map.Strict (Map)
import           Language.Javascript.JSaddle (Object)

import           Miso.String hiding (reverse)
import           Miso.Types

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

-- | Create a new @Text@ with the given content.
text :: MisoString -> View action
text = Text

-- | `TextRaw` creation. Don't use directly
textRaw :: MisoString -> View action
textRaw = TextRaw

-- | Virtual DOM implemented as a JavaScript `Object`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree = VTree { getTree :: Object }

-- | @prop k v@ is an attribute that will set the attribute @k@ of the DOM node associated with the vnode
-- to @v@.
prop :: ToJSON a => MisoString -> a -> Attribute action
prop k v = P k (toJSON v)

-- | @style_ attrs@ is an attribute that will set the @style@
-- attribute of the associated DOM node to @attrs@.
--
-- @style@ attributes not contained in @attrs@ will be deleted.
--
-- > import qualified Data.Map as M
-- > div_ [ style_  $ M.singleton "background" "red" ] [ ]
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS>
--
style_ :: Map MisoString MisoString -> Attribute action
style_ = S
