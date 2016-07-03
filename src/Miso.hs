{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE LambdaCase                #-}

module Miso where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson                    hiding (Object)
import           Data.Bool
import qualified Data.Foldable                 as F
import           Data.IORef
import           Data.JSString.Text
import           Data.List                     (find)
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           FRP.Elerea.Simple             (externalMulti, transfer, start)
import           GHCJS.DOM
import           GHCJS.DOM.CharacterData
import           GHCJS.DOM.Document            hiding (drop, getLocation)
import           GHCJS.DOM.Element             (setAttribute, removeAttribute)
import           GHCJS.DOM.Event               (Event, getTarget)
import qualified GHCJS.DOM.Event               as E
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types               hiding (Event, Attr)
import           GHCJS.Foreign                 hiding (Object)
import           GHCJS.Marshal
import           JavaScript.Object.Internal
import           JavaScript.Web.AnimationFrame
import           Prelude                       hiding (repeat)

import           Miso.Types

data Attribute 
  = Event String (Event -> IO ())
  | Attr T.Text T.Text
  | Prop T.Text Value

on :: String
   -> (Event -> IO ())
   -> Attribute  
on = Event

instance Eq Attribute where
  Prop x1 x2 == Prop y1 y2 = x1 == y1 && x2 == y2
  Event x _ == Event y _ = x == y
  _ == _                 = False

instance Show Attribute where
  show (Event name _) = "<event=" ++ name ++ ">"
  show (Attr k v) = T.unpack $ k <> "=" <> v
  show (Prop k v) = T.unpack $ k <> "=" <> T.pack (show v)

type Key = Maybe Int
type VTree = VTreeBase (Maybe Node) 

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
  [ node | VNode _ _ _ _ (Just node) <- children ]
getChildDOMNodes _ = []

getDOMNode :: VTree -> Maybe Node
getDOMNode (VNode _ _ _ _ ref) = ref
getDOMNode _ = Nothing

instance Show (VTreeBase a) where
  show VEmpty = "<empty>"
  show (VNode typ evts children _ _) =
    "<" ++ typ ++ ">" ++ show evts ++
      concatMap show children ++ "\n" ++ "</" ++ typ ++ ">"
  show (VText val _ ) = val

mkNode :: String -> [Attribute] -> [VTree] -> VTree
mkNode name as cs = VNode name as cs (Nothing :: Maybe Int) Nothing

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

delegator :: IORef VTree -> Events -> IO ()
delegator ref events = do
  Just doc <- currentDocument
  Just body <- fmap toNode <$> getBody doc
  listener <- eventListenerNew (f body)
  forM_ events $ \event ->
    addEventListener body event (Just listener) True
    where
      f :: Node -> Event -> IO ()
      f body e = do
        Just target <- getTarget e
        vtree <- readIORef ref
        eventType <- E.getType e
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

delegateEvent :: Event -> VTree -> String -> [Node] -> IO ()
delegateEvent e (VNode _ _ children _ _) eventName = findEvent children 
    where
      findEvent _ [] = pure ()
      findEvent childNodes [y] = 
       forM_ (findNode childNodes y) $ \(VNode _ attrs _ _ _) ->
         forM_ (getAction attrs) $ \action -> action e

      findEvent childNodes (y:ys) = 
        forM_ (findNode childNodes y) $ \(VNode _ _ childrenNext _  _) ->
          findEvent childrenNext ys

      findNode childNodes ref = do
        let nodes = getVNodesOnly childNodes
        flip find nodes $ \node ->
          getDOMNode node == Just ref
  
      getVNodesOnly cs = do
        vnode@VNode{} <- cs
        pure vnode

      getAction attrs =
        listToMaybe $ do
          Event evtName action <- attrs
          guard (evtName == eventName)
          pure action

delegateEvent _ _ _ = const $ pure ()

defaultEvents :: [String] 
defaultEvents = [
    "blur", "change", "click", "dblclick",
    "focus", "focusin", "focusout", "input", "keydown",
    "keypress", "keyup", "mousedown", "mouseup",
    "mousemove", "mouseover", "select", "submit"
    ]

initTree :: VTree -> IO VTree
initTree initial = do
  Just document <- currentDocument
  Just body <- getBody document
  vdom <- datch VEmpty initial
  case vdom of
    VText _ ref -> void $ appendChild body ref
    VNode _ _ _ _ ref -> void $ appendChild body ref
    VEmpty -> pure ()
  pure vdom

datch :: VTree -> VTree -> IO VTree
datch currentTree newTree = do
  Just document <- currentDocument
  Just body <- fmap toNode <$> getBody document
  goDatch document body currentTree newTree

goDatch :: Document -> Node -> VTree -> VTree -> IO VTree
goDatch _ _ VEmpty VEmpty = pure VEmpty

-- Ensure correct initialization (always true if internal)
goDatch _ _ (VNode _ _ _ _ Nothing) _ = Prelude.error "VNode not initialized"
goDatch _ _ (VText _ Nothing) _ = Prelude.error "VText not initialized"

-- Make a new text node
goDatch doc parentNode VEmpty (VText str _) = do
  newTextNode <- createTextNode doc str
  void $ appendChild parentNode newTextNode
  pure $ VText str (toNode <$> newTextNode)

-- Remove a text node
goDatch _ parentNode (VText _ node) VEmpty = do
  void $ removeChild parentNode node
  pure VEmpty

-- Make a new element
goDatch doc parentNode VEmpty (VNode typ attrs children key _) = do
  node@(Just newNode)  <- fmap toNode <$> createElement doc (Just typ)
  void $ diffAttrs newNode [] attrs
  newChildren <- forM children $ \childNode ->
    goDatch doc newNode VEmpty childNode
  void $ appendChild parentNode node
  pure $ VNode typ attrs newChildren key node

-- Remove an element
goDatch _ parentNode (VNode _ _ _ _ node) VEmpty = 
  VEmpty <$ removeChild parentNode node

-- Replace an element with a text node
goDatch doc parentNode (VNode _ _ _ _ ref) (VText str _) = do
  newTextNode <- fmap toNode <$> createTextNode doc str
  void $ replaceChild parentNode newTextNode ref
  pure $ VText str newTextNode

-- Replace a text node with an Element
goDatch doc parentNode (VText _ ref) (VNode typ attrs children key _) = do
  node@(Just newNode) <- fmap toNode <$> createElement doc (Just typ)
  newChildren <- forM children $ \childNode ->
    goDatch doc newNode VEmpty childNode
  void $ replaceChild parentNode node ref
  pure $ VNode typ attrs newChildren key node

-- Replace a text node with a text node
goDatch _ _ (VText currentStr currRef) (VText newStr _) = do
  when (currentStr /= newStr) $ do
    F.forM_ currRef $ \ref -> do
      let txt = castToText ref
      oldLength <- getLength txt
      replaceData txt 0 oldLength newStr
  pure $ VText newStr currRef

-- Diff two nodes together
goDatch doc parent
  (VNode typA attrsA childrenA _ (Just ref))
  (VNode typB attrsB childrenB keyB _) = do
 case typA == typB of
   True ->
      VNode typB <$> diffAttrs ref attrsA attrsB
                 <*> diffChildren doc ref childrenA childrenB
                 <*> pure keyB
                 <*> pure (Just ref)
   False -> do      
      node@(Just newNode) <- fmap toNode <$> createElement doc (Just typB)
      void $ diffAttrs newNode [] attrsB
      newChildren <- forM childrenB $ \childNode ->
        goDatch doc newNode VEmpty childNode
      void $ replaceChild parent node (Just ref)
      pure $ VNode typB attrsB newChildren keyB node

diffAttrs
  :: Node
  -> [Attribute]
  -> [Attribute]
  -> IO [Attribute]
diffAttrs node attrsA attrsB = do
  when (attrsA /= attrsB) $ do
    diffPropsAndAttrs node attrsA attrsB
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
diffChildren _ _ [] [] = pure []
diffChildren doc parent [] (b:bs) = 
  (:) <$> goDatch doc parent VEmpty b
      <*> diffChildren doc parent [] bs
diffChildren doc parent (a:as) [] = 
  (:) <$> goDatch doc parent a VEmpty
      <*> diffChildren doc parent as [] 
diffChildren doc parent (a:as) (b:bs) = do
  (:) <$> goDatch doc parent a b
      <*> diffChildren doc parent as bs

type Events = [ String ]

runSignal :: Events -> Signal VTree -> IO ()
runSignal events (Signal s) = do
  vtreeRef <- newIORef =<< initTree VEmpty
  _ <- forkIO $ delegator vtreeRef events
  emitter <- start s
  forever $ 
    waitForAnimationFrame >>
      emitter >>= \case
        Changed [ newTree ] -> do
          tree <- (`datch` newTree) =<< readIORef vtreeRef
          writeIORef vtreeRef tree
        _ -> pure ()

signal :: Show a => a -> IO (Signal a, a -> IO ())
signal x = do
  (s, writer) <- externalMulti 
  writer x
  pure (Signal $ fmap toSample <$> s, writer)
    where
      toSample [] = NotChanged []
      toSample xs = Changed xs

foldp :: Eq model => (action -> model -> model) -> model -> Signal action -> Signal model
foldp f ini (Signal gen) =
  Signal $ gen >>= transfer (pure [ini]) update
      where
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

integerProp :: T.Text -> Int -> Attribute
integerProp = prop

doubleProp :: T.Text -> Int -> Attribute
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

