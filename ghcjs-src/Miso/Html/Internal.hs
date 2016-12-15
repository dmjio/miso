{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Html.Internal
-- Copyright   :  (C) 2016-2017 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Html.Internal (
  -- * Core types and interface
    VTree  (..)
  , View   (..)
  , ToView (..)
  , Attribute (..)
  -- * Smart `View` constructors
  , node
  , text
  -- * Key patch internals
  , Key    (..)
  , ToKey  (..)
  -- * Namespace
  , NS     (..)
  -- * Setting properties on virtual DOM nodes
  , prop
  -- * Setting css
  , style_
  -- * Handling events
  , on
  , onWithOptions
  -- * String
  , MisoString
  , module Data.JSString
  , ToMisoString (..)
  -- * Events
  , defaultEvents
  , Target (..)
  , Event (..)
  , HasEvent (..)
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson hiding (Object)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.JSString
import           Data.JSString.Text
import qualified Data.Map                   as M
import           Data.Proxy
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LT
import           GHC.Generics
import           GHC.TypeLits
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Array.Internal  (fromList)
import           JavaScript.Object
import           JavaScript.Object.Internal (Object (Object))
import           Miso.Types

-- | Virtual DOM implemented as a JavaScript `Object`
--   Used for diffing, patching and event delegation.
--   Not meant to be constructed directly, see `View` instead.
newtype VTree m = VTree { getTree :: Object }

-- | Core type for constructing a `VTree`, use this instead of `VTree` directly.
newtype View m = View { runView :: Sink m -> IO (VTree m) }

-- | Convenience class for using View
class ToView v where toView :: v -> View m

set :: ToJSVal v => JSString -> v -> Object -> IO ()
set k v obj = toJSVal v >>= \x -> setProp k x obj

-- | `VNode` creation
node :: NS
     -> MisoString
     -> Maybe Key
     -> [Attribute m]
     -> [View m]
     -> View m
node ns tag key attrs kids = View $ \sink -> do
  vnode <- create
  cssObj <- jsval <$> create
  propsObj <- jsval <$> create
  eventObj <- jsval <$> create
  set "css" cssObj vnode
  set "props" propsObj vnode
  set "events" eventObj vnode
  set "type" ("vnode" :: JSString) vnode
  set "ns" ns vnode
  set "tag" tag vnode
  set "key" key vnode
  setAttrs vnode sink
  flip (set "children") vnode =<< setKids sink
  pure $ VTree vnode
    where
      setAttrs vnode sink =
        forM_ attrs $ \(Attribute attr) ->
          attr sink vnode

      setKids sink =
        jsval . fromList <$>
          fmap (jsval . getTree) <$>
            traverse (flip runView sink) kids

instance ToJSVal Options
instance ToJSVal Key where toJSVal (Key x) = toJSVal x

instance ToJSVal NS where
  toJSVal SVG  = toJSVal ("svg" :: JSString)
  toJSVal HTML = toJSVal ("html" :: JSString)

-- | Namespace for element creation
data NS
  = HTML -- ^ HTML Namespace
  | SVG  -- ^ SVG Namespace
  deriving (Show, Eq)

-- | `VText` creation
text :: ToMisoString str => str -> View m
text t = View . const $ do
  vtree <- create
  set "type" ("vtext" :: JSString) vtree
  set "text" (toMisoString t) vtree
  pure $ VTree vtree

-- | Key for specific children patch
newtype Key = Key { getKey :: MisoString }
  deriving (Show, Eq, Ord, Generic)

-- | Convert type into Key, ensure `Key` is unique
class ToKey key where toKey :: key -> Key
-- | Identity instance
instance ToKey Key where toKey = id
-- | Convert `MisoString` to `Key`
instance ToKey MisoString where toKey = Key
-- | Convert `Text` to `Key`
instance ToKey T.Text where toKey = Key . textToJSString
-- | Convert `String` to `Key`
instance ToKey String where toKey = Key . pack
-- | Convert `Int` to `Key`
instance ToKey Int where toKey = Key . pack . show
-- | Convert `Double` to `Key`
instance ToKey Double where toKey = Key . pack . show
-- | Convert `Float` to `Key`
instance ToKey Float where toKey = Key . pack . show
-- | Convert `Word` to `Key`
instance ToKey Word where toKey = Key . pack . show

-- | `View` Attributes to annotate DOM, converted into `Events`, `Props`, `Attrs` and `CSS`
newtype Attribute m = Attribute (Sink m -> Object -> IO ())

-- | Constructs a property on a `VNode`, used to set fields on a DOM Node
prop :: ToJSVal a => MisoString -> a -> Attribute model
prop k v = Attribute . const $ \n -> do
  val <- toJSVal v
  o <- getProp ("props" :: MisoString) n
  set k val (Object o)

-- | For defining delegated events with options
--
-- > let clickHandler = on (Proxy :: Proxy "click") $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
on :: (KnownSymbol name, HasEvent name return )
   => Proxy name
   -> (return -> Action model)
   -> Attribute model
on = onWithOptions defaultOptions

onWithOptions
   :: (KnownSymbol name, HasEvent name return )
   => Options
   -> Proxy name
   -> (return -> Action model)
   -> Attribute model
onWithOptions options eventName f = Attribute $ \sink n -> do
   eventObj <- getProp "events" n
   eventHandlerObject@(Object eo) <- create
   jsOptions <- toJSVal options
   cb <- jsval <$> (asyncCallback1 $ \e -> do
       r <- runGrammar $ parseEvent eventName (Event e)
       sink (f r)
       void . forkIO $ sink =<< effect eventName r)
   setProp "runEvent" cb eventHandlerObject
   setProp "options" jsOptions eventHandlerObject
   setProp (pack name) eo (Object eventObj)
     where
       name = symbolVal eventName

-- | Constructs `CSS` for a DOM Element
--
-- > import qualified Data.Map as M
-- > div_ [ style_  $ M.singleton "background" "red" ] [ ]
--
-- <https://developer.mozilla.org/en-US/docs/Web/CSS>
--
style_ :: M.Map MisoString MisoString -> Attribute action
style_ m = Attribute . const $ \n -> do
   cssObj <- getProp "css" n
   forM_ (M.toList m) $ \(k,v) ->
     setProp k (jsval v) (Object cssObj)

-- | String type swappable based on compiler
type MisoString = JSString

-- | `ToJSON` for `MisoString`
instance ToJSON MisoString where
  toJSON = String . textFromJSString

-- | `FromJSON` for `MisoString`
instance FromJSON MisoString where
  parseJSON =
    withText "Not a valid string" $ \x ->
      pure (toMisoString x)

class ToMisoString str where
  toMisoString :: str -> MisoString

instance ToMisoString MisoString where toMisoString = id
instance ToMisoString String where toMisoString = pack
instance ToMisoString T.Text where toMisoString = textToJSString
instance ToMisoString LT.Text where toMisoString = lazyTextToJSString
instance ToMisoString B.ByteString where toMisoString = toMisoString . T.decodeUtf8
instance ToMisoString BL.ByteString where toMisoString = toMisoString . LT.decodeUtf8

defaultEvents :: M.Map JSString Bool
defaultEvents = M.fromList [
    ("blur", True)
  , ("change", False)
  , ("click", False)
  , ("dblclick", False)
  , ("focus", False)
  , ("input", False)
  , ("keydown", False)
  , ("keypress", False)
  , ("keyup", False)
  , ("mouseup", False)
  , ("mousedown", False)
  , ("mouseenter", False)
  , ("mouseleave", False)
  , ("mouseover", False)
  , ("mouseout", False)
  , ("dragstart", False)
  , ("dragover", False)
  , ("dragend", False)
  , ("dragenter", False)
  , ("dragleave", False)
  , ("drag", False)
  , ("drop", False)
  , ("submit", False)
  ]

class HasEvent (eventName :: Symbol) returnType where
  parseEvent :: Proxy eventName -> Event -> Grammar returnType
  effect :: Proxy eventName -> returnType -> IO (model -> model)
  effect _ _ = pure id

newtype Grammar returnType = Grammar { runGrammar :: IO returnType }
  deriving (Monad, Applicative, Functor)

newtype Event = Event JSVal
newtype Target = Target JSVal

-- | Retrieves "value" field in `Grammar`
-- inputGrammar :: FromJSON a => Event -> Grammar a
-- inputGrammar e = do
--   target <- getTarget
--   Just value <- getField "value" target
--   stringify value

-- | Retrieves "checked" field in `Grammar`
-- checkedGrammar :: Event -> Grammar Bool
-- checkedGrammar e = do
--   Just checked <- getField "checked" =<< getTarget e
--   stringify checked

-- | Retrieves either "keyCode", "which" or "charCode" field in `Grammar`
-- keyGrammar :: Event -> Grammar Int
-- keyGrammar e = do
--   keyCode <- getField "keyCode" e
--   which <- getField "which" e
--   charCode <- getField "charCode" e
--   stringify $ head $ catMaybes [ keyCode, which, charCode ]


---- | Retrieve event
----
---- > function (e) { return e; }
----
---- | Retrieve target
----
---- > e.target;
----
-- $(makeFreeCon 'GetTarget)

---- | Attempts to lookup parent field on DOM node
----
---- > node.parent;
----
-- $(makeFreeCon 'GetParent)

---- | Attempts to lookup field on DOM node
----
---- > object.field;
----
--- $(makeFreeCon 'GetField)

---- | Assigns field on event object
----
---- > function (e) { e.field = value; }
----
--- $(makeFreeCon 'SetField)

---- | Retrieves children
----
---- > node.children;
----
--- $(makeFreeCon 'GetChildren)

---- | Retrieves child at index
----
---- > node.children[index];
----
--- $(makeFreeCon 'GetItem)

---- | Retrieves next sibling
----
---- > node.nextSibling;
----
--- $(makeFreeCon 'GetNextSibling)

---- | Executes function on object
----
---- > node['f'].apply(node.['f'], ["arg1", "arg2"])
----
--- $(makeFreeCon 'Apply)

---- | Logs to console
----
---- > console.log(o);
----
--- $(makeFreeCon 'ConsoleLog)

---- | Converts a `JSVal` to a `(FromJSON a => Result a)`
----
---- > JSON.stringify(o);
----
--- $(makeFreeCon 'Stringify)

-- | Listens on "blur" event, returns `()`
instance HasEvent "blur" () where parseEvent _ _ = pure ()

-- | Listens on "change" event, returns `Bool` from `event.target.checked`
instance HasEvent "change" Bool   where parseEvent _ = undefined
--checkedGrammar

-- | Listens on "click" event, returns `()`
instance HasEvent "click" () where
  parseEvent _ _ = pure ()

-- | Listens on "dblclick" event, returns `()`
instance HasEvent "dblclick" ()   where parseEvent _ _ = pure ()

-- | Listens on "focus" event, returns `()`
instance HasEvent "focus" ()      where parseEvent _ _ = pure ()

-- | Listens on "input" event, returns `MisoString` from  `event.target.value`
instance HasEvent "input" MisoString  where parseEvent _ = undefined
--inputGrammar

-- | Listens on "keydown" event, returns `Int` from `keyCode`, `which` or `charCode`
instance HasEvent "keydown" Int   where parseEvent _ = undefined
--keyGrammar

-- | Listens on "keypress" event, returns `Int` from `keyCode`, `which` or `charCode`
instance HasEvent "keypress" Int  where parseEvent _ = undefined
--keyGrammar

-- | Listens on "keyup" event, returns `Int` from `keyCode`, `which` or `charCode`
instance HasEvent "keyup" Int     where parseEvent _ = undefined
--keyGrammar

-- | Listens on "mouseup" event, returns `()`
instance HasEvent "mouseup" ()    where parseEvent _ _ = pure ()

-- | Listens on "mousedown" event, returns `()`
instance HasEvent "mousedown" ()  where parseEvent _ _ = pure ()

-- | Listens on "mouseenter" event, returns `()`
instance HasEvent "mouseenter" () where parseEvent _ _ = pure ()

-- | Listens on "mouseleave" event, returns `()`
instance HasEvent "mouseleave" () where parseEvent _ _ = pure ()

-- | Listens on "mouseover" event, returns `()`
instance HasEvent "mouseover" ()  where parseEvent _ _ = pure ()

-- | Listens on "mouseout" event, returns `()`
instance HasEvent "mouseout" ()   where parseEvent _ _ = pure ()

-- | Listens on "dragstart" event, returns `()`
instance HasEvent "dragstart" ()  where parseEvent _ _ = pure ()

-- | Listens on "dragover" event, returns `()`
instance HasEvent "dragover" ()   where parseEvent _ _ = pure ()

-- | Listens on "dragend" event, returns `()`
instance HasEvent "dragend" ()    where parseEvent _ _ = pure ()

-- | Listens on "dragenter" event, returns `()`
instance HasEvent "dragenter" ()  where parseEvent _ _ = pure ()

-- | Listens on "dragleave" event, returns `()`
instance HasEvent "dragleave" ()  where parseEvent _ _ = pure ()

-- | Listens on "drag" event, returns `()`
instance HasEvent "drag" ()       where parseEvent _ _ = pure ()

-- | Listens on "drop" event, returns `()`
instance HasEvent "drop" ()       where parseEvent _ _ = pure ()

-- | Listens on "submit" event, returns `()`
instance HasEvent "submit" ()     where parseEvent _ _ = pure ()

-- | Listens on "begin" event, returns `()`
instance HasEvent "begin" () where parseEvent _ _ = pure ()

-- | Listens on "end" event, returns `()`

instance HasEvent "end" () where parseEvent _ _ = pure ()

-- | Listens on "repeat" repeat, returns `()`
instance HasEvent "repeat" () where parseEvent _ _ = pure ()

-- | Listens on "abort" abort, returns `()`
instance HasEvent "abort" () where parseEvent _ _ = pure ()

-- | Listens on "error" error, returns `()`
instance HasEvent "error" () where parseEvent _ _ = pure ()

-- | Listens on "resize" resize, returns `()`
instance HasEvent "resize" () where parseEvent _ _ = pure ()

-- | Listens on "mousemove" mousemove, returns `()`
instance HasEvent "mousemove" () where parseEvent _ _ = pure ()

-- | Listens on "focusout" focusout, returns `()`
instance HasEvent "focusout" () where parseEvent _ _ = pure ()

-- | Listens on "focusin" focusin, returns `()`
instance HasEvent "focusin" () where parseEvent _ _ = pure ()

-- | Listens on "activate" activate, returns `()`
instance HasEvent "activate" () where parseEvent _ _ = pure ()

-- | Listens on "zoom" zoom, returns `()`
instance HasEvent "zoom" () where parseEvent _ _ = pure ()

-- | Listens on "unload" unload, returns `()`
instance HasEvent "unload" () where parseEvent _ _ = pure ()

-- | Listens on "load" load, returns `()`
instance HasEvent "load" () where parseEvent _ _ = pure ()

-- | Listens on "scroll" scroll, returns `()`
instance HasEvent "scroll" () where parseEvent _ _ = pure ()

--evalEventGrammar :: JSVal -> F (Action JSVal) a -> IO a
--evalEventGrammar e = do
--   iterM $ \x ->
--     case x of
--       GetEvent cb -> cb e
--       GetTarget cb -> cb =<< getProp "target" (Object e)
--       GetParent obj cb -> cb =<< getProp "parentNode" (Object obj)
--       GetField key obj cb ->
--         cb . nullableToMaybe =<< Nullable <$> getProp key (Object obj)
--       SetField key val cb -> do
--         jsValue <- toJSVal (toJSON val)
--         setProp key jsValue (Object e) >> cb
--       GetChildren obj cb ->
--         cb =<< getProp "childNodes" (Object obj)
--       GetItem obj n cb ->
--         cb . nullableToMaybe =<< Nullable <$> item obj n
--       Stringify val cb -> do
--         Just v <- fmap fromJSON <$> fromJSVal val
--         case v of
--           Error err -> error $ "Decode failure: " ++ err
--           Success s -> cb s
--       ConsoleLog o cb -> do
--         cb =<< Miso.FFI.consoleLog o
--       GetNextSibling obj cb ->
--         cb =<< getProp "nextSibling" (Object obj)
--       Apply obj str xs cb ->
--         cb =<< applyFunction obj str
--            =<< fromList <$> mapM toJSVal xs

-- Use TypeError ? (GHC8 only)
-- ifdef here to make it nice
type family Elem (x :: Symbol) xs :: Bool where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs
