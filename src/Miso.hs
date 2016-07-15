{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Miso where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Data.Aeson                    hiding (Object)
import           Data.Bool
import qualified Data.Foldable                 as F
import           Data.IORef
import           Data.JSString.Text
import           Data.List                     (find)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                      as S
import           Data.String.Conversions
import qualified Data.Text                     as T
import           FRP.Elerea.Simple             (externalMulti, transfer, start, effectful1)
import           GHC.Ptr
import           GHC.TypeLits
import           GHCJS.DOM
import           GHCJS.DOM.CharacterData
import           GHCJS.DOM.Document            hiding (drop, getLocation)
import           GHCJS.DOM.Element             (removeAttribute, setAttribute)
import           GHCJS.DOM.Event               (Event)
import qualified GHCJS.DOM.Event               as E
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures
import qualified GHCJS.DOM.Node                as Node
import           GHCJS.DOM.Node                hiding (getNextSibling)
import           GHCJS.DOM.NodeList            hiding (getLength)
import qualified GHCJS.DOM.Storage             as S
import           GHCJS.DOM.Types               hiding (Event, Attr)
import           GHCJS.DOM.Window              (getLocalStorage)
import           GHCJS.Foreign                 hiding (Object, Number)
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import qualified GHCJS.Types                   as G
import           JavaScript.Object.Internal
import           JavaScript.Web.AnimationFrame
import           Miso.Types
import           Prelude                       hiding (repeat)

data Action object a = 
     GetTarget object (object -> a)
   | GetParent object (object -> a)
   | GetChildren object (object -> a)
   | GetItem object Int (Maybe object -> a)
   | GetNextSibling object (Maybe object -> a)
   | forall v . FromJSON v => GetField T.Text object (v -> a)
   | forall v . FromJSON v => GetAction T.Text object (IO v -> a)

$(makeFreeCon 'GetTarget)
$(makeFreeCon 'GetParent)
$(makeFreeCon 'GetChildren)
$(makeFreeCon 'GetItem)
$(makeFreeCon 'GetNextSibling)
$(makeFreeCon 'GetField)
$(makeFreeCon 'GetAction)

foreign import javascript unsafe "$1[$2]"
  retrieveAction :: G.JSVal -> G.JSString -> IO (G.JSVal)

evalEventGrammar :: Grammar G.JSVal a -> IO a
evalEventGrammar = 
  iterM $ \x ->
    case x of 
      GetTarget obj cb -> do
        cb =<< pToJSVal <$> E.getTarget (pFromJSVal obj :: E.Event)
      GetParent obj cb -> do
        Just p <- getParentNode (pFromJSVal obj :: Node)
        cb (pToJSVal p)
      GetChildren obj cb -> do
        Just nodeList <- getChildNodes (pFromJSVal obj :: Node)
        cb (pToJSVal nodeList)
      GetItem obj n cb -> do
        result <- item (pFromJSVal obj :: NodeList) (fromIntegral n)
        cb (fmap pToJSVal result)
      GetNextSibling obj cb -> do
        result <- Node.getNextSibling (pFromJSVal obj :: Node) 
        cb (fmap pToJSVal result)
        -- TODO: convert JSVal to object
      GetField key obj cb -> do
        val <- getProp (textToJSString key) (Object obj) 
        cb ( (undefined :: FromJSON v => G.JSVal -> v) val)
        -- TODO: convert JSVal to FromJSON v => v
      GetAction key obj cb -> do
        let val = retrieveAction obj (textToJSString key)
        cb ( fmap (undefined :: FromJSON v => G.JSVal -> v) val) -- fix undefined

      -- - Just result <- E.getT(toEvent bje)t obj)
      -- toJSVal esult)

instance HasEvent "keypress" (IO ()) where
  parseEvent Proxy e = do
    v <- getTarget e
    children <- getChildren v
    Just child <- getItem children 0
    getAction "focus" child
        

deriving instance Functor (Action object)

type Grammar obj a = Free (Action obj) a

class HasEvent (eventName :: Symbol) returnType where
  parseEvent :: Proxy eventName -> obj -> Grammar obj returnType

on :: (FromJSON returnType, KnownSymbol eventName, HasEvent eventName returnType)
   => Proxy eventName
   -> (returnType -> IO ())
   -> Attribute
on p = EventHandler (symbolVal p) p

newtype MyKey = MyKey Int deriving FromJSON

-- getField :: FromJSON v => T.Text -> obj -> Grammar obj v
-- getField key obj = liftF $ GetField key obj id


-- getAction :: FromJSON v => T.Text -> obj -> Grammar obj (IO v)
-- getAction key obj  = liftF $ GetAction key obj id

-- getParent :: Grammar obj obj
-- getParent = liftF $ GetParent id

-- getNextSibling :: Grammar obj obj
-- getNextSibling = liftF $ GetNextSibling id

-- getChildren :: Grammar obj [obj]
-- getChildren = liftF $ GetChildren id

data Attribute = forall eventName returnType . HasEvent eventName returnType =>
    EventHandler String (Proxy eventName) (returnType -> IO ())
  | Attr T.Text T.Text
  | Prop T.Text Value

instance Eq Attribute where
  Prop x1 x2 == Prop y1 y2 = x1 == y1 && x2 == y2
  EventHandler x _ _ == EventHandler y _ _ = x == y
  _ == _                 = False

instance Show Attribute where
  show (EventHandler name _ _) = "<event=" <> name <> ">"
  show (Attr k v) = T.unpack $ k <> "=" <> v
  show (Prop k v) = T.unpack $ k <> "=" <> T.pack (show v)

type Key = Maybe Int

type VTree = VTreeBase (Maybe (Ptr ()))

toPtr :: Node -> Ptr ()
toPtr = G.toPtr . pToJSVal 

fromPtr :: Ptr a -> Node
fromPtr = pFromJSVal . G.fromPtr

toPtrFromEvent :: Event -> Ptr () 
toPtrFromEvent = G.toPtr . pToJSVal 

getKey :: VTreeBase a -> Maybe Int
getKey (VNode _ _ _ maybeKey _) = maybeKey
getKey _ = Nothing

data VTreeBase a where
  VNode :: String -> [ Attribute ] -> [ VTreeBase a ] -> Maybe Int -> a -> VTreeBase a 
  VText :: String -> a -> VTreeBase a 
  VEmpty :: VTreeBase a
  deriving (Eq)

getChildDOMNodes :: VTree -> [Node]
getChildDOMNodes (VNode _ _ children _ _) =
  [ fromPtr node | VNode _ _ _ _ (Just node) <- children ]
getChildDOMNodes _ = []

getDOMNode :: VTree -> Maybe Node
getDOMNode (VNode _ _ _ _ ref) = fromPtr <$> ref
getDOMNode _ = Nothing

instance Show (VTreeBase e) where
  show VEmpty = "<empty>"
  show (VNode typ evts children _ _) =
    "<" ++ typ ++ ">" ++ show evts ++
      concatMap show children ++ "\n" ++ "</" ++ typ ++ ">"
  show (VText val _ ) = val

mkNode :: String -> [Attribute] -> [VTree] -> VTree
mkNode name as xs = VNode name as xs Nothing Nothing

text_ :: String -> VTree
text_ = flip VText Nothing

div_ :: [Attribute] -> [VTree] -> VTree
div_  = mkNode "div"

section_ :: [Attribute] -> [VTree] -> VTree
section_  = mkNode "section"

header_ :: [Attribute] -> [VTree] -> VTree
header_  = mkNode "header"

footer_ :: [Attribute] -> [VTree] -> VTree
footer_  = mkNode "footer"

btn_ :: [Attribute] -> [VTree] -> VTree
btn_ = mkNode "button"

delegator :: IORef (VTree) -> Events -> IO ()
delegator ref events = do
  Just doc <- currentDocument
  Just body <- fmap toNode <$> getBody doc
  listener <- eventListenerNew (f body)
  forM_ events $ \event ->
    addEventListener body event (Just listener) True
    where
      f :: Node -> E.Event -> IO ()
      f body e = do
        Just target <- E.getTarget e
        vtree <- readIORef ref
        eventType :: String <- E.getType e
        stack <- buildTargetToBody body (castToNode target)
        delegateEvent e vtree eventType stack

buildTargetToBody :: Node -> Node -> IO [Node]
buildTargetToBody body target = f target [target]
    where
      f currentNode nodes
        | body == currentNode = pure (drop 2 nodes)
        | otherwise = do
            Just parent <- getParentNode currentNode
            f parent (parent:nodes)

runner :: Event -> Attribute -> IO ()
runner e (EventHandler _ prox action) = runEvent e prox action  
runner _ _ = pure ()

runEvent
  :: HasEvent eventName returnType
  => Event
  -> Proxy eventName
  -> (returnType -> IO ())
  -> IO ()
runEvent e prox action =
  action =<< do evalEventGrammar $ parseEvent prox (pToJSVal e)

delegateEvent :: Event -> VTree -> String -> [Node] -> IO ()
delegateEvent e (VNode _ _ children _ _) eventName = findEvent children 
    where
      findEvent _ [] = pure ()
      findEvent childNodes [y] = 
       forM_ (findNode childNodes y) $ \(VNode _ attrs _ _ _) ->
         forM_ (getEventHandler attrs) $ \evt ->
           runner e evt

      findEvent childNodes (y:ys) = 
        forM_ (findNode childNodes y) $ \(VNode _ _ childrenNext _  _) ->
          findEvent childrenNext ys

      findNode childNodes ref = do
        let nodes = getVNodesOnly childNodes
        flip find nodes $ \node ->
          getDOMNode node == Just ref
  
      getVNodesOnly childs = do
        vnode@VNode{} <- childs
        pure vnode

      getEventHandler attrs =
       listToMaybe $ do
          eh@(EventHandler evtName _ _) <- attrs
          guard (evtName == eventName)
          pure eh
delegateEvent _ _ _ = const $ pure ()

defaultEvents :: [String] 
defaultEvents = [
    "blur", "change", "click", "dblclick",
    "focus", "focusin", "focusout", "input", "keydown",
    "keypress", "keyup", "mousedown", "mouseup",
    "mousemove", "mouseover", "select", "submit"
    ]

initTree :: VTree -> IO (VTree)
initTree initial = do
  Just document <- currentDocument
  Just body <- getBody document
  vdom <- datch VEmpty initial
  case vdom of
    VText _ ref -> void $ appendChild body (fromPtr <$> ref)
    VNode _ _ _ _ ref -> void $ appendChild body (fromPtr <$> ref)
    VEmpty -> pure ()
  pure vdom

-- copies body first child into vtree, to avoid flickering 
copyDOMIntoVTree :: Node -> VTree -> IO (VTree)
copyDOMIntoVTree _ VEmpty = pure VEmpty -- should never get called
copyDOMIntoVTree node (VText s _) = pure $ VText s (toPtr <$> Just node)
copyDOMIntoVTree node (VNode name attrs children key _) = do
  xs <- forM (zip [0 :: Int ..] children) $ \(index, childNode) -> do
          Just childNodes <- getChildNodes node
          Just child <- item childNodes (fromIntegral index)
          copyDOMIntoVTree child childNode
  pure $ VNode name attrs xs key (toPtr <$> Just node)

datch :: VTree -> VTree -> IO (VTree)
datch currentTree newTree = do
  Just document <- currentDocument
  Just body <- fmap toNode <$> getBody document
  goDatch document body currentTree newTree

goDatch :: Document -> Node -> VTree -> VTree -> IO (VTree)
goDatch _ _ VEmpty VEmpty = pure VEmpty

-- Ensure correct initialization (always true if internal)
goDatch _ _ (VNode _ _ _ _ Nothing) _ = Prelude.error "VNode not initialized"
goDatch _ _ (VText _ Nothing) _ = Prelude.error "VText not initialized"

-- Make a new text node
goDatch doc parentNode VEmpty (VText str _) = do
  newTextNode <- createTextNode doc str
  void $ appendChild parentNode newTextNode
  pure $ VText str (toPtr <$> toNode <$> newTextNode)

-- Remove a text node
goDatch _ parentNode (VText _ node) VEmpty = do
  void $ removeChild parentNode (fromPtr <$> node)
  pure VEmpty

-- Make a new element
goDatch doc parentNode VEmpty (VNode typ attrs children key _) = do
  node@(Just newNode)  <- fmap toNode <$> createElement doc (Just typ)
  void $ diffAttrs newNode [] attrs
  newChildren <- forM children $ \childNode ->
    goDatch doc newNode VEmpty childNode
  void $ appendChild parentNode node
  pure $ VNode typ attrs newChildren key (toPtr <$> node)

-- Remove an element
goDatch _ parentNode (VNode _ _ _ _ node) VEmpty = 
  VEmpty <$ removeChild parentNode (fromPtr <$> node)

-- Replace an element with a text node
goDatch doc parentNode (VNode _ _ _ _ ref) (VText str _) = do
  newTextNode <- fmap toNode <$> createTextNode doc str
  void $ replaceChild parentNode newTextNode (fromPtr <$> ref)
  pure $ VText str (toPtr <$> newTextNode)

-- Replace a text node with an Element
goDatch doc parentNode (VText _ ref) (VNode typ attrs children key _) = do
  node@(Just newNode) <- fmap toNode <$> createElement doc (Just typ)
  newChildren <- forM children $ \childNode ->
    goDatch doc newNode VEmpty childNode
  void $ replaceChild parentNode node (fromPtr <$> ref)
  pure $ VNode typ attrs newChildren key (toPtr <$> node)

-- Replace a text node with a text node
goDatch _ _ (VText currentStr currRef) (VText newStr _) = do
  when (currentStr /= newStr) $ do
    F.forM_ currRef $ \ref -> do
      let txt = castToText (fromPtr ref)
      oldLength <- getLength txt
      replaceData txt 0 oldLength newStr
  pure $ VText newStr currRef

-- Diff two nodes together
goDatch doc parent
  (VNode typA attrsA childrenA _ (Just ref))
  (VNode typB attrsB childrenB keyB _) = do
 case typA == typB of
   True ->
      VNode typB <$> diffAttrs (fromPtr ref) attrsA attrsB
                 <*> diffChildren doc (fromPtr ref) childrenA childrenB
                 <*> pure keyB
                 <*> pure (Just ref)
   False -> do      
      node@(Just newNode) <- fmap toNode <$> createElement doc (Just typB)
      void $ diffAttrs newNode [] attrsB
      newChildren <- forM childrenB $ \childNode ->
        goDatch doc newNode VEmpty childNode
      void $ replaceChild parent node (fromPtr <$> Just ref)
      pure $ VNode typB attrsB newChildren keyB (toPtr <$> node)

diffAttrs
  :: Node
  -> [Attribute]
  -> [Attribute]
  -> IO [Attribute]
diffAttrs node attrsA attrsB = do
  when (attrsA /= attrsB) $ diffPropsAndAttrs node attrsA attrsB
  pure attrsB

diffPropsAndAttrs :: Node -> [Attribute] -> [Attribute] -> IO ()
diffPropsAndAttrs node old new = do
  obj <- Object <$> toJSVal node
  let el = castToElement node
      
      newAttrs = S.fromList [ (k, v) | Attr k v <- new ]
      oldAttrs = S.fromList [ (k, v) | Attr k v <- old ]

      removeAttrs = oldAttrs `S.difference` newAttrs
      addAttrs    = newAttrs `S.difference` oldAttrs

      newProps = M.fromList [ (k,v) | Prop k v <- new ]
      oldProps = M.fromList [ (k,v) | Prop k v <- old ]

      propsToRemove = oldProps `M.difference` newProps
      propsToAdd    = newProps `M.difference` oldProps
      propsToDiff   = newProps `M.intersection` oldProps

  forM_ (M.toList propsToRemove) $ \(k, _) -> do
    setProp (textToJSString k) jsNull obj

  forM_ (M.toList propsToAdd) $ \(k, v) -> do
    val <- toJSVal v
    setProp (textToJSString k) val obj

  forM_ (M.toList propsToDiff) $ \(k, _) -> do
    case (M.lookup k oldProps, M.lookup k newProps) of
      (Just oldVal, Just newVal) ->
        when (oldVal /= newVal) $ do
        val <- toJSVal newVal
        setProp (textToJSString k) val obj
      (_, _) -> pure ()

  forM_ removeAttrs $ \(k,_) -> removeAttribute el k 
  forM_ addAttrs $ \(k,v) -> setAttribute el k v

diffChildren 
  :: Document
  -> Node
  -> [VTree]
  -> [VTree]
  -> IO [VTree]
diffChildren doc parent as bs = do
  xs <- diffChildren' doc parent as bs
  pure $ filter (/=VEmpty) xs

diffChildren'
  :: Document
  -> Node
  -> [VTree]
  -> [VTree]
  -> IO [VTree]
diffChildren' _ _ [] [] = pure []
diffChildren' doc parent [] (b:bs) = 
  (:) <$> goDatch doc parent VEmpty b
      <*> diffChildren doc parent [] bs
diffChildren' doc parent (a:as) [] = 
  (:) <$> goDatch doc parent a VEmpty
      <*> diffChildren doc parent as [] 
diffChildren' doc parent (a:as) (b:bs) = do
  (:) <$> goDatch doc parent a b
      <*> diffChildren doc parent as bs

type Events = [ String ]

data AppConfig config = AppConfig {
      useStorage :: Bool
    , storageKey :: T.Text
    } 

class ToJSON model => HasConfig model where
  getConfig :: AppConfig model

runSignal :: Events -> Signal (VTree) -> IO ()
runSignal events (Signal s) = do
  vtreeRef <- newIORef =<< initTree VEmpty
  _ <- forkIO $ delegator vtreeRef events
  emitter <- start s
  forever $ 
    waitForAnimationFrame >>
      emitter >>= \case
        Changed [ newTree ] -> do
          patchedTree <- (`datch` newTree) =<< readIORef vtreeRef
          writeIORef vtreeRef patchedTree
        _ -> pure ()

signal :: Show a => a -> IO (Signal a, a -> IO ())
signal x = do
  (s, writer) <- externalMulti 
  writer x
  pure (Signal $ fmap toSample <$> s, writer)
    where
      toSample [] = NotChanged []
      toSample xs = Changed xs

getFromStorage
  :: forall model . (FromJSON model, HasConfig model)
  => IO (Either String model)
getFromStorage = do
  let AppConfig _ key = getConfig :: AppConfig model
  Just w <- currentWindow
  Just s <- getLocalStorage w
  maybeVal <- S.getItem s key
  pure $ case maybeVal of
    Nothing -> Left "Not found"
    Just m -> eitherDecode (cs (m :: T.Text))

setStorage :: HasConfig model => T.Text -> model -> IO ()
setStorage key m = do
  Just w <- currentWindow
  Just s <- getLocalStorage w
  S.setItem s (textToJSString key) (cs (encode m) :: T.Text)

foldp :: (HasConfig model, Eq model)
      => (action -> model -> model) -> model -> Signal action -> Signal model
foldp f ini (Signal gen) =
   Signal $ gen >>= transfer (pure [ini]) update
                >>= effectful1 saveToStorage
      where
        saveToStorage :: forall model . HasConfig model => Sample [model] -> IO (Sample [model])
        saveToStorage (Changed [m]) = do
          let AppConfig{..} = getConfig :: AppConfig model
          when useStorage $ void . forkIO $ setStorage storageKey m
          pure (Changed [m])
        saveToStorage m = pure m

        update (NotChanged _) xs = NotChanged (fromChanged xs)
        update (Changed actions) model = do
          let oldModel : _ = fromChanged model
              newModel = foldr f oldModel (reverse actions)
          bool (notChanged newModel) (changed newModel) (oldModel /= newModel)
            where
              notChanged = NotChanged . pure
              changed = Changed . pure        

attr :: T.Text -> T.Text -> Attribute
attr = Attr

prop :: ToJSON a => T.Text -> a -> Attribute 
prop k v = Prop k (toJSON v)

boolProp :: T.Text -> Bool -> Attribute 
boolProp = prop

stringProp :: T.Text -> T.Text -> Attribute
stringProp = prop

textProp :: T.Text -> T.Text -> Attribute
textProp = prop

intProp :: T.Text -> Int -> Attribute
intProp = prop

integerProp :: T.Text -> Integer -> Attribute
integerProp = prop

doubleProp :: T.Text -> Double -> Attribute
doubleProp = prop

checked_ :: Bool -> Attribute
checked_ = boolProp "checked"

form_ :: [Attribute] -> [VTree] -> VTree
form_ = mkNode "form" 

p_ :: [Attribute] -> [VTree] -> VTree
p_ = mkNode "p" 

s_ :: [Attribute] -> [VTree] -> VTree
s_ = mkNode "s" 

ul_ :: [Attribute] -> [VTree] -> VTree
ul_ = mkNode "ul" 

span_ :: [Attribute] -> [VTree] -> VTree
span_ = mkNode "span" 

strong_ :: [Attribute] -> [VTree] -> VTree
strong_ = mkNode "strong" 

li_ :: [Attribute] -> [VTree] -> VTree
li_ = mkNode "li" 

h1_ :: [Attribute] -> [VTree] -> VTree
h1_ = mkNode "h1" 

input_ :: [Attribute] -> [VTree] -> VTree
input_ = mkNode "input" 

label_ :: [Attribute] -> [VTree] -> VTree
label_ = mkNode "label" 

a_ :: [Attribute] -> [VTree] -> VTree
a_ = mkNode "a" 

style_ :: T.Text -> Attribute 
style_ = attr "style" 

type_ :: T.Text -> Attribute 
type_ = attr "type"

name_ :: T.Text -> Attribute 
name_ = attr "name"

href_ :: T.Text -> Attribute 
href_ = attr "href"

className_ :: T.Text -> Attribute 
className_ = stringProp "className"

class_ :: T.Text -> Attribute 
class_ = attr "class"

id_ :: T.Text -> Attribute 
id_ = attr "id"

placeholder :: T.Text -> Attribute 
placeholder = attr "placeholder" 

autofocus :: Bool -> Attribute 
autofocus = boolProp "autofocus"

template :: VTree
template = div_  [] []

-- data JObject = JValue Value | JAction (IO ())
-- type JSObject = M.Map T.Text JObject
-- type Parser a = Either T.Text a
-- class FromJSObject a where fromObject :: JSObject -> Parser a
-- class ToJVal a where toVal :: JObject -> Either T.Text a

-- instance ToJVal a => ToJVal (IO a) where
--   toVal (JAction x) = undefined
--   toVal _ = Left "type mismatch for IO ()"

-- instance ToJVal Int where
--   toVal (JValue v) =
--     case fromJSON v of
--       Error x -> Left (T.pack x)
--       Success x -> pure x
--   toVal _ = Left "type mismatch for Int"

-- instance ToJVal () where
--   toVal (JValue _) = pure ()
--   toVal _ = Left "type mismatch for ()"

-- instance ToJVal Double where
--   toVal (JValue v) =
--     case fromJSON v of
--       Error x -> Left (T.pack x)
--       Success x -> pure x
--   toVal _ = Left "type mismatch for Double"

-- instance {-# overlappable #-} FromJSON a => ToJVal [a] where
--   toVal (JValue v) = 
--     case fromJSON v of
--       Error x -> Left (T.pack x)
--       Success x -> pure x
--   toVal _ = Left "type mismatch for Double"

-- instance ToJVal String where
--   toVal (JValue v) =
--     case fromJSON v of
--       Error x -> Left (T.pack x)
--       Success x -> pure x
--   toVal _ = Left "type mismatch for string"

-- instance ToJVal T.Text where
--   toVal (JValue v) =
--     case fromJSON v of
--       Error x -> Left (T.pack x)
--       Success x -> pure x
--   toVal _ = Left "type mismatch for Text"

-- instance ToJVal Bool where
--   toVal (JValue v) =
--     case fromJSON v of
--       Error x -> Left (T.pack x)
--       Success x -> pure x
--   toVal _ = Left "type mismatch for Int"

-- (.#) :: ToJVal a => JSObject -> T.Text -> Parser a
-- o .# name =
--   case M.lookup name o of
--     Nothing -> Left $ "Couldn't find " <> name
--     Just x -> toVal x

-- data KeyEvent = KeyEvent {
--      code :: Int
--    , name :: String
--    }

-- instance FromJSObject KeyEvent where
--   fromObject o =
--     KeyEvent <$> o .# "which"
--              <*> o .# "keyCode"
