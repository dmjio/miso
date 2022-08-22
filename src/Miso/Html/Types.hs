{-# LANGUAGE CPP                  #-}
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
    -- * `View` runner
    , runView
    -- * Smart `View` constructors
    , node
    , text
    , textRaw
    , rawHtml
    -- * Core types and interface
    , Attribute (..)
    -- * Key patch internals
    , Key    (..)
    , ToKey  (..)
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

import           Control.Monad              (forM_, (<=<))
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (ToJSON, Value, toJSON)
import qualified Data.Aeson                 as A
import           Data.Aeson.Types           (parseEither)
import           Data.JSString              (JSString)
import qualified Data.Map                   as M
import           Data.Proxy                 (Proxy(Proxy))
import           Data.String                (IsString, fromString)
import qualified Data.Text                  as T
import           GHCJS.Marshal              (ToJSVal, fromJSVal, toJSVal)
import           GHCJS.Types                (jsval)
import qualified JavaScript.Array           as JSArray
import           JavaScript.Object          (create, getProp)
import           JavaScript.Object.Internal (Object(Object))
import qualified Lucid                      as L
import qualified Lucid.Base                 as L
import           Prelude                    hiding (null)
import           Servant.API                (Get, HasLink, MkLink, toLink)
import           Text.HTML.TagSoup.Tree     (parseTree, TagTree(..))
import           Text.HTML.TagSoup          (Tag(..))

import           Miso.Effect
import           Miso.Event
import           Miso.FFI
import           Miso.String hiding (elem, reverse)

-- | Core type for constructing a `VTree`, use this instead of `VTree` directly.
data View action
    = Node NS MisoString (Maybe Key) [Attribute action] [View action]
    | Text MisoString
    | TextRaw MisoString
    deriving Functor

-- | For constructing type-safe links
instance HasLink (View a) where
#if MIN_VERSION_servant(0,14,0)
  type MkLink (View a) b = MkLink (Get '[] ()) b
  toLink toA Proxy = toLink toA (Proxy :: Proxy (Get '[] ()))
#else
  type MkLink (View a) = MkLink (Get '[] ())
  toLink _ = toLink (Proxy :: Proxy (Get '[] ()))
#endif

-- | Convenience class for using View
class ToView v where toView :: v -> View action

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

-- | `IsString` instance
instance IsString (View a) where
  fromString = text . fromString

-- | Converting `View` to Lucid's `L.Html`
instance L.ToHtml (View action) where
  toHtmlRaw = L.toHtml
  toHtml (Node _ vType _ attrs vChildren) = L.with ele lattrs
    where
      noEnd = ["img", "input", "br", "hr", "meta"]
      tag = toTag $ fromMisoString vType
      ele = if tag `elem` noEnd
          then L.makeElementNoEnd tag
          else L.makeElement tag kids
      classes = T.intercalate " " [ v | P "class" (A.String v) <- attrs ]
      propClass = M.fromList $ attrs >>= \case
          P k v -> [(k, v)]
          E _ -> []
          S m -> [("style", A.String . fromMisoString $ M.foldrWithKey go mempty m)]
            where
              go :: MisoString -> MisoString -> MisoString -> MisoString
              go k v ys = mconcat [ k, ":", v, ";" ] <> ys
      xs = if not (T.null classes)
          then M.insert "class" (A.String classes) propClass
          else propClass
      lattrs = [ L.makeAttribute k' (if k `elem` exceptions && v == A.Bool True then k' else v')
               | (k,v) <- M.toList xs
               , let k' = fromMisoString k
               , let v' = toHtmlFromJSON v
               , not (k `elem` exceptions && v == A.Bool False)
               ]
      exceptions = [ "checked"
                   , "disabled"
                   , "selected"
                   , "hidden"
                   , "readOnly"
                   , "autoplay"
                   , "required"
                   , "default"
                   , "autofocus"
                   , "multiple"
                   , "noValidate"
                   , "autocomplete"
                   ]
      toTag = T.toLower
      kids = foldMap L.toHtml $ collapseSiblingTextNodes vChildren
  toHtml (Text x) | null x = L.toHtml (" " :: T.Text)
                  | otherwise = L.toHtml (fromMisoString x :: T.Text)
  toHtml (TextRaw x)
    | null x = L.toHtml (" " :: T.Text)
    | otherwise = L.toHtmlRaw (fromMisoString x :: T.Text)

collapseSiblingTextNodes :: [View a] -> [View a]
collapseSiblingTextNodes [] = []
collapseSiblingTextNodes (Text x : Text y : xs) =
  collapseSiblingTextNodes (Text (x <> y) : xs)
-- TextRaw is the only child, so no need to collapse.
collapseSiblingTextNodes (x:xs) =
  x : collapseSiblingTextNodes xs

-- | Helper for turning JSON into Text
-- Object, Array and Null are kind of non-sensical here
toHtmlFromJSON :: Value -> T.Text
toHtmlFromJSON (A.String t) = t
toHtmlFromJSON (A.Number t) = T.pack (show t)
toHtmlFromJSON (A.Bool b) = if b then "true" else "false"
toHtmlFromJSON A.Null = "null"
toHtmlFromJSON (A.Object o) = T.pack (show o)
toHtmlFromJSON (A.Array a) = T.pack (show a)

-- | Virtual DOM implemented as a JavaScript `Object`.
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree = VTree { getTree :: Object }

runView :: View action -> Sink action -> JSM VTree
runView (Node ns tag key attrs kids) sink = do
  vnode <- create
  cssObj <- objectToJSVal =<< create
  propsObj <- objectToJSVal =<< create
  eventObj <- objectToJSVal =<< create
  set "css" cssObj vnode
  set "props" propsObj vnode
  set "events" eventObj vnode
  set "type" ("vnode" :: JSString) vnode
  set "ns" ns vnode
  set "tag" tag vnode
  set "key" key vnode
  setAttrs vnode
  flip (set "children") vnode
    =<< ghcjsPure . jsval
    =<< setKids
  pure $ VTree vnode
    where
      setAttrs vnode =
        forM_ attrs $ \case
          P k v -> do
            val <- toJSVal v
            o <- getProp "props" vnode
            set k val (Object o)
          E attr -> attr sink vnode
          S m -> do
            cssObj <- getProp "css" vnode
            forM_ (M.toList m) $ \(k,v) -> do
              set k v (Object cssObj)
      setKids = do
        kidsViews <- traverse (objectToJSVal . getTree <=< flip runView sink) kids
        ghcjsPure (JSArray.fromList kidsViews)
runView (Text t) _ = do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" t vtree
  pure $ VTree vtree
runView (TextRaw str) sink =
  case parseView str of
    [] ->
      runView (Text (" " :: MisoString)) sink
    [parent] ->
      runView parent sink
    kids -> do
      runView (Node HTML "div" Nothing mempty kids) sink

-- Filters tree to only branches and leaves w/ Text tags.
-- converts to View a. Note: if HTML is malformed,
-- (e.g. closing tags and opening tags are present) they will
-- be removed.
parseView :: MisoString -> [View a]
parseView html = reverse (go (parseTree html) [])
  where
    go [] xs = xs
    go (TagLeaf (TagText s) : next) views =
      go next (Text s : views)
    go (TagLeaf (TagOpen name attrs) : next) views =
      go (TagBranch name attrs [] : next) views
    go (TagBranch name attrs kids : next) views =
      let
        attrs' = [ P key $ A.String (fromMisoString val)
                 | (key, val) <- attrs
                 ]
        newNode =
          Node HTML name Nothing attrs' (reverse (go kids []))
      in
        go next (newNode:views)
    go (TagLeaf _ : next) views =
      go next views

-- | Namespace of DOM elements.
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  | MATHML  -- ^ MATHML Namespace
  deriving (Show, Eq)

instance ToJSVal NS where
  toJSVal SVG  = toJSVal ("svg" :: JSString)
  toJSVal HTML = toJSVal ("html" :: JSString)
  toJSVal MATHML = toJSVal ("mathml" :: JSString)

-- | A unique key for a dom node.
--
-- This key is only used to speed up diffing the children of a DOM
-- node, the actual content is not important. The keys of the children
-- of a given DOM node must be unique. Failure to satisfy this
-- invariant gives undefined behavior at runtime.
newtype Key = Key MisoString

instance ToJSVal Key where toJSVal (Key x) = toJSVal x

-- | Convert custom key types to `Key`.
--
-- Instances of this class do not have to guarantee uniqueness of the
-- generated keys, it is up to the user to do so. `toKey` must be an
-- injective function.
class ToKey key where toKey :: key -> Key
-- | Identity instance
instance ToKey Key where toKey = id
-- | Convert `MisoString` to `Key`
instance ToKey JSString where toKey = Key . toMisoString
-- | Convert `T.Text` to `Key`
instance ToKey T.Text where toKey = Key . toMisoString
-- | Convert `String` to `Key`
instance ToKey String where toKey = Key . toMisoString
-- | Convert `Int` to `Key`
instance ToKey Int where toKey = Key . toMisoString
-- | Convert `Double` to `Key`
instance ToKey Double where toKey = Key . toMisoString
-- | Convert `Float` to `Key`
instance ToKey Float where toKey = Key . toMisoString
-- | Convert `Word` to `Key`
instance ToKey Word where toKey = Key . toMisoString

-- | Attribute of a vnode in a `View`.
--
-- The 'Sink' callback can be used to dispatch actions which are fed back to
-- the @update@ function. This is especially useful for event handlers
-- like the @onclick@ attribute. The second argument represents the
-- vnode the attribute is attached to.
data Attribute action
    = P MisoString Value
    | E (Sink action -> Object -> JSM ())
    | S (M.Map MisoString MisoString)
    deriving Functor

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
   cb <- callbackToJSVal <=< asyncCallback1 $ \e -> do
       Just v <- fromJSVal =<< objectToJSON decodeAtVal e
       case parseEither decoder v of
         Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
         Right r -> liftIO (sink (toAction r))
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
    cb <- callbackToJSVal =<< asyncCallback (liftIO (sink action))
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
    cb <- callbackToJSVal =<< asyncCallback (liftIO (sink action))
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
    cb <- callbackToJSVal =<< asyncCallback (liftIO (sink action))
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
