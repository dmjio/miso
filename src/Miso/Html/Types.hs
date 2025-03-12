{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Miso.Html.Types (
    -- * Core types and interface
      VTree  (..)
    , View   (..)
    , ToView (..)
    -- * Smart `View` constructors
    , node
    , text
    , textRaw
    , rawHtml
    -- * Core types and interface
    , Attribute (..)
    -- * Key patch internals
    , Key    (..)
    -- * Namespace
    , NS(..)
    -- * Setting properties on virtual DOM nodes
    , prop
    -- * Setting css
    , style_
    -- * Handling events
    , on
    , onWithOptions
    -- * Life cycle events
    , onCreated
    , onDestroyed
    , onBeforeDestroyed
    ) where

import           Control.Monad
import           Data.Aeson (ToJSON, toJSON)
import           Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as M
import           GHCJS.Marshal (fromJSVal, toJSVal)
import           JavaScript.Object (create, getProp)
import           JavaScript.Object.Internal (Object(Object))
import           Prelude hiding (null)

import           Miso.Event
import           Miso.FFI
import           Miso.String                hiding (reverse)
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

-- | Convenience wrapper for @onWithOptions defaultOptions@.
--
-- > let clickHandler = on "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: MisoString
   -> Decoder r
   -> (r -> action)
   -> Attribute action
on = onWithOptions defaultOptions

-- | @onWithOptions opts eventName decoder toAction@ is an attribute
-- that will set the event handler of the associated DOM node to a function that
-- decodes its argument using @decoder@, converts it to an action
-- using @toAction@ and then feeds that action back to the @update@ function.
--
-- @opts@ can be used to disable further event propagation.
--
-- > let clickHandler = onWithOptions defaultOptions "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
onWithOptions
  :: Options
  -> MisoString
  -> Decoder r
  -> (r -> action)
  -> Attribute action
onWithOptions options eventName Decoder{..} toAction =
  E $ \sink n -> do
   eventObj <- getProp "events" n
   eventHandlerObject@(Object eo) <- create
   jsOptions <- toJSVal options
   decodeAtVal <- toJSVal decodeAt
   cb <- toJSVal <=< asyncCallback1 $ \e -> do
       Just v <- fromJSVal =<< objectToJSON decodeAtVal e
       case parseEither decoder v of
         Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
         Right r -> sink (toAction r)
   set "runEvent" cb eventHandlerObject
   registerCallback cb
   set "options" jsOptions eventHandlerObject
   set eventName eo (Object eventObj)

-- | @onCreated action@ is an event that gets called after the actual DOM
-- element is created.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onCreated :: action -> Attribute action
onCreated action =
  E $ \sink n -> do
    cb <- toJSVal =<< syncCallback (sink action)
    set "onCreated" cb n
    registerCallback cb

-- | @onDestroyed action@ is an event that gets called after the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onDestroyed :: action -> Attribute action
onDestroyed action =
  E $ \sink n -> do
    cb <- toJSVal =<< syncCallback (sink action)
    set "onDestroyed" cb n
    registerCallback cb

-- | @onBeforeDestroyed action@ is an event that gets called before the DOM element
-- is removed from the DOM. The @action@ is given the DOM element that was
-- removed from the DOM tree.
--
-- Important note: Any node that uses this event MUST have a unique @Key@,
-- otherwise the event may not be reliably called!
onBeforeDestroyed :: action -> Attribute action
onBeforeDestroyed action =
  E $ \sink n -> do
    cb <- toJSVal =<< syncCallback (sink action)
    set "onBeforeDestroyed" cb n
    registerCallback cb

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
style_ :: M.Map MisoString MisoString -> Attribute action
style_ = S
