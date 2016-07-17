{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
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
import           GHCJS.DOM.Document            hiding (drop, getLocation, focus)
import           GHCJS.DOM.Element             (removeAttribute, setAttribute, focus)
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
import qualified GHCJS.Foreign.Internal        as Foreign
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import qualified GHCJS.Types                   as G
import           JavaScript.Object.Internal
import           JavaScript.Web.AnimationFrame
import qualified Lucid                         as L
import qualified Lucid.Base                    as L
import           Miso.Types
import           Prelude                       hiding (repeat)

data Action object a where
  GetTarget :: object -> (object -> a) -> Action object a
  PreventDefault :: object -> a -> Action object a
  StopPropagation :: object -> a -> Action object a
  GetParent :: object -> (object -> a) -> Action object a
  GetField  :: FromJSON v => T.Text -> object -> (Maybe v -> a) -> Action object a
  GetChildren ::  object -> (object -> a) -> Action object a
  GetItem :: object -> Int -> (Maybe object -> a) -> Action object a
  GetNextSibling :: object -> (Maybe object -> a) -> Action object a

$(makeFreeCon 'GetTarget)
$(makeFreeCon 'GetParent)
$(makeFreeCon 'GetField)
$(makeFreeCon 'GetChildren)
$(makeFreeCon 'GetItem)
$(makeFreeCon 'GetNextSibling)
$(makeFreeCon 'PreventDefault)
$(makeFreeCon 'StopPropagation)

jsToJSON :: FromJSON v => JSType -> G.JSVal -> IO (Maybe v)
jsToJSON Foreign.Number  g = convertToJSON g
jsToJSON Foreign.Boolean g = convertToJSON g
jsToJSON Foreign.Object  g = convertToJSON g
jsToJSON Foreign.String  g = convertToJSON g
jsToJSON _ _ = pure Nothing

convertToJSON :: FromJSON v => G.JSVal -> IO (Maybe v)
convertToJSON g = do
  Just (val :: Value) <- fromJSVal g
  case fromJSON val of -- Should *always* be able to decode this
    Error e -> Prelude.error $ "Error while decoding Value: " <> e <> " " <> show val
    Success v -> pure (pure v)

evalEventGrammar :: Grammar G.JSVal a -> IO a
evalEventGrammar = do
  iterM $ \x ->
    case x of 
      GetTarget obj cb -> do
        cb =<< pToJSVal <$> E.getTarget (pFromJSVal obj :: E.Event)
      GetParent obj cb -> do
        Just p <- getParentNode (pFromJSVal obj :: Node)
        cb (pToJSVal p)
      GetField key obj cb -> do
        val <- getProp (textToJSString key) (Object obj)
        cb =<< jsToJSON (jsTypeOf val) val
      GetChildren obj cb -> do
        Just nodeList <- getChildNodes (pFromJSVal obj :: Node)
        cb $ pToJSVal nodeList
      GetItem obj n cb -> do
        result <- item (pFromJSVal obj :: NodeList) (fromIntegral n)
        cb $ pToJSVal <$> result
      GetNextSibling obj cb -> do
        result <- Node.getNextSibling (pFromJSVal obj :: Node) 
        cb $ pToJSVal <$> result
      StopPropagation obj cb -> do
        void $ E.stopPropagation (pFromJSVal obj :: E.Event)
        cb
      PreventDefault obj cb -> do
        void $ E.preventDefault (pFromJSVal obj :: E.Event)
        cb

deriving instance Functor (Action object)

type Grammar obj a = Free (Action obj) a

class HasEvent (eventName :: Symbol) returnType where
  parseEvent :: Proxy eventName -> obj -> Grammar obj returnType

on :: (KnownSymbol eventName, HasEvent eventName returnType)
   => Proxy eventName
   -> (returnType -> IO ())
   -> Attribute
on p = EventHandler (symbolVal p) p

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
  VNode :: T.Text -> [ Attribute ] -> [ VTreeBase a ] -> Maybe Int -> a -> VTreeBase a 
  VText :: T.Text -> a -> VTreeBase a 
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
  show (VText val _ ) = T.unpack val
  show (VNode typ evts children _ _) =
    "<" ++ T.unpack typ ++ ">" ++ show evts ++
      concatMap show children ++ "\n" ++ "</" ++ T.unpack typ ++ ">"

mkNode :: T.Text -> [Attribute] -> [VTree] -> VTree
mkNode name as xs = VNode name as xs Nothing Nothing

text_ :: T.Text -> VTree
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
  forM_ (M.toList events) $ \(event, capture) ->
    addEventListener body event (Just listener) capture
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

instance L.ToHtml VTree where
  toHtmlRaw = L.toHtml
  toHtml VEmpty = Prelude.error "VEmpty for internal use only"
  toHtml (VText x _) = L.toHtml x
  toHtml (VNode typ attrs children _ _) =
    let ele = L.makeElement (toTag typ) (foldMap L.toHtml children)
    in L.with ele as
      where
        as = [ L.makeAttribute k v | Attr k v <- attrs ]
        toTag = T.toLower

diffAttrs
  :: Node
  -> [Attribute]
  -> [Attribute]
  -> IO [Attribute]
diffAttrs node attrsA attrsB = do
  when (attrsA /= attrsB) $ diffPropsAndAttrs node attrsA attrsB
  pure attrsB

observables :: M.Map T.Text (Element -> IO ())
observables = M.fromList [("autofocus", focus)]

dispatchObservable :: T.Text -> Element -> IO ()
dispatchObservable key el = do
  F.forM_ (M.lookup key observables) $ \f -> f el

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
        dispatchObservable k el
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

type Events = M.Map T.Text Bool

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

foldp :: (HasConfig model)
      => (action -> model -> model)
      -> model
      -> Signal action
      -> Signal model
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
          let [ oldModel ] = fromChanged model
          Changed [ foldr f oldModel (reverse actions) ]

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

-- | (EventName, Capture)
defaultEvents :: Events
defaultEvents = 
  M.fromList [
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
  , ("submit", False)
  ]

instance HasEvent "blur" () where parseEvent _ _ = pure ()
instance HasEvent "change" Bool where parseEvent _ = checkedGrammar
instance HasEvent "click" () where parseEvent _ _ = pure ()
instance HasEvent "dblclick" () where parseEvent _ _ = pure ()
instance HasEvent "focus" () where parseEvent _ _ = pure ()
instance HasEvent "input" T.Text where parseEvent _ = inputGrammar
instance HasEvent "keydown" Int where parseEvent Proxy = keyGrammar
instance HasEvent "keypress" Int where parseEvent Proxy = keyGrammar
instance HasEvent "keyup" Int where parseEvent Proxy = keyGrammar
instance HasEvent "mouseup" () where parseEvent _ _ = pure ()
instance HasEvent "mousedown" () where parseEvent _ _ = pure ()
instance HasEvent "mouseenter" () where parseEvent _ _ = pure ()
instance HasEvent "mouseleave" () where parseEvent _ _ = pure ()
instance HasEvent "mouseover" () where parseEvent _ _ = pure ()
instance HasEvent "mouseout" () where parseEvent _ _ = pure ()
instance HasEvent "submit" () where parseEvent _ = preventDefault 

onBlur :: IO () -> Attribute
onBlur action = on (Proxy :: Proxy "blur") $ \() -> action

onChecked :: (Bool -> IO ()) -> Attribute
onChecked = on (Proxy :: Proxy "change")

onClick :: IO () -> Attribute
onClick action = on (Proxy :: Proxy "click") $ \() -> action

onFocus :: IO () -> Attribute
onFocus action = on (Proxy :: Proxy "focus") $ \() -> action

onDoubleClick :: IO () -> Attribute
onDoubleClick action = on (Proxy :: Proxy "dblclick") $ \() -> action

onInput :: (T.Text -> IO ()) -> Attribute
onInput = on (Proxy :: Proxy "input")

onKeyDown :: (Int -> IO ()) -> Attribute
onKeyDown = on (Proxy :: Proxy "keydown")

onKeyPress :: (Int -> IO ()) -> Attribute
onKeyPress = on (Proxy :: Proxy "keypress")

onKeyUp :: (Int -> IO ()) -> Attribute
onKeyUp = on (Proxy :: Proxy "keyup")

onMouseUp :: IO () -> Attribute
onMouseUp action = on (Proxy :: Proxy "mouseup") $ \() -> action

onMouseDown :: IO () -> Attribute
onMouseDown action = on (Proxy :: Proxy "mousedown") $ \() -> action

onMouseEnter :: IO () -> Attribute
onMouseEnter action = on (Proxy :: Proxy "mouseenter") $ \() -> action

onMouseLeave :: IO () -> Attribute
onMouseLeave action = on (Proxy :: Proxy "mouseleave") $ \() -> action

onMouseOver :: IO () -> Attribute
onMouseOver action = on (Proxy :: Proxy "mouseover") $ \() -> action

onMouseOut :: IO () -> Attribute
onMouseOut action = on (Proxy :: Proxy "mouseout") $ \() -> action

onSubmit :: IO () -> Attribute
onSubmit action = on (Proxy :: Proxy "submit") $ \() -> action

inputGrammar :: FromJSON a => obj -> Grammar obj a 
inputGrammar e = do
    target <- getTarget e
    result <- getField "value" target
    case result of
      Nothing -> Prelude.error "Couldn't retrieve target input value"
      Just value -> pure value

checkedGrammar :: FromJSON a => obj -> Grammar obj a 
checkedGrammar e = do
    target <- getTarget e
    result <- getField "checked" target
    case result of
      Nothing -> Prelude.error "Couldn't retrieve target checked value"
      Just value -> pure value

keyGrammar :: FromJSON a => obj -> Grammar obj a 
keyGrammar e = do   
    keyCode <- getField "keyCode" e
    which <- getField "which" e
    charCode <- getField "charCode" e
    pure $ head $ catMaybes [ keyCode, which, charCode ]

