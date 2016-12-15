{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Miso.Event where

import qualified Data.Map     as M
import           Data.Proxy
import           GHC.TypeLits
import           GHCJS.Types
import           Miso.String

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

-- | `EventHandler` object action
newtype EventHandler model = EventHandler (IO JSVal)

newtype Grammar returnType = Grammar { runGrammar :: IO returnType }
  deriving (Monad, Applicative, Functor)

foo :: Grammar Int
foo = return 1

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
instance HasEvent "click" ()      where parseEvent _ _ = pure ()

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

