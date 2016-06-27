{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Data.Foldable                 as F
import           Data.List                     (find)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import           GHCJS.DOM
import           GHCJS.DOM.CharacterData
import           GHCJS.DOM.Document            hiding (drop)
import           GHCJS.DOM.Event               (getType, Event, getTarget)
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types               hiding (Event)

foreign import javascript unsafe "alert($1);" 
  alert :: Double -> IO ()

data Options = Options {
       stopPropogation :: Bool
     , preventDefault :: Bool
     } deriving Show

defaultOptions :: Options
defaultOptions = Options False False

data Attribute action 
  = Event String (IO ())
  | KV Bool T.Text T.Text

instance Show action => Show (Attribute action) where
  show (Event _ _) = "<action>"
  show (KV _ k v) = T.unpack $ k <> "=" <> v

type DOMNode = Maybe Node

data VTree action =
    VNode String [ Attribute action ] [ VTree action ] DOMNode
  | VText String DOMNode
  | VEmpty

getChildDOMNodes :: VTree action -> [Node]
getChildDOMNodes (VNode _ _ children _) = [ node | VNode _ _ _ (Just node) <- children ]
getChildDOMNodes _ = []

getDOMNode :: VTree action -> Maybe Node
getDOMNode (VNode _ _ _ ref) = ref
getDOMNode _ = Nothing

instance Show action => Show (VTree action) where
  show VEmpty = "<empty>"
  show (VNode typ _ children _) =
    "<" ++ typ ++ ">" ++ concatMap show children ++ "</" ++ typ ++ ">"
  show (VText val _ ) = val

mkNode :: String -> [Attribute action] -> [VTree action] -> VTree action
mkNode name as cs = VNode name as cs Nothing

text_ :: String -> VTree action
text_ = flip VText Nothing

div_ :: [Attribute action] -> [VTree action] -> VTree action
div_  = mkNode "div"

btn_ :: [Attribute action] -> [VTree action] -> VTree action
btn_ = mkNode "button"

click_ :: Attribute action 
click_ = Event "click" (alert 42)

delegate :: VTree a -> IO ()
delegate vtree = do
  Just doc <- currentDocument
  Just body <- fmap toNode <$> getBody doc
  listener <- eventListenerNew (f body)
  addEventListener body ("click" :: String) (Just listener) True
    where
      f :: Node -> Event -> IO ()
      f body e = do
        (eventType :: String) <- getType e
        Just target <- getTarget e
        stack <- buildTargetToBody body (castToNode target)
        delegateEvent vtree eventType stack

buildTargetToBody :: Node -> Node -> IO [Node]
buildTargetToBody body target = f target [target]
    where
      f currentNode nodes
        | body == currentNode = pure (drop 2 nodes)
        | otherwise = do
            Just parent <- getParentNode currentNode
            f parent (parent:nodes)

delegateEvent :: VTree a -> String -> [Node] -> IO ()
delegateEvent (VNode _ _ children _) eventName = findEvent children 
    where
      findEvent _ [] = pure ()
      findEvent childNodes [y] = do
       VNode _ attrs _ _ <- findNode childNodes y
       forM_ (getAction attrs) $ \action -> action

      findEvent childNodes (y:ys) = do
        VNode _ _ childrenNext _ <- findNode childNodes y
        findEvent childrenNext ys

      findNode childNodes ref = do
        let nodes = getVNodesOnly childNodes
            Just x = flip find nodes $ \node ->
              getDOMNode node == Just ref
        pure x
  
      getVNodesOnly cs = do
        vnode@VNode{} <- cs
        pure vnode

      getAction attrs =
        listToMaybe $ do
          Event evtName action <- attrs
          guard (evtName == eventName)
          pure action
delegateEvent _ _ = const $ pure ()

temp :: VTree ()
temp = div_ [ ] [
    btn_ [ click_ ] [ text_ "hey" ]
  ]
         
main :: IO ()
main = do
  tree <- initTree temp
  void $ forkIO $ delegate tree
  forever $ do
    threadDelay (secs 10)
    pure ()
  
secs :: Int -> Int
secs = (*1000000)

initTree :: Show action => VTree action -> IO (VTree action)
initTree initial = do
  Just document <- currentDocument
  Just body <- getBody document
  vdom <- diff VEmpty initial
  case vdom of
    VText _ ref -> void $ appendChild body ref
    VNode _ _ _ ref -> void $ appendChild body ref
    VEmpty -> pure ()
  pure vdom

diff :: Show action => VTree action -> VTree action -> IO (VTree action)
diff currentTree newTree = do
  Just document <- currentDocument
  Just body <- fmap toNode <$> getBody document
  go document body currentTree newTree

go :: Show action => Document -> Node -> VTree action -> VTree action -> IO (VTree action)
go _ _ VEmpty VEmpty = pure VEmpty

-- Make a new text node
go doc parentNode VEmpty (VText str _) = do
  newTextNode <- createTextNode doc str
  void $ appendChild parentNode newTextNode
  pure $ VText str (toNode <$> newTextNode)

-- Remove a text node
go _ parentNode (VText _ ref) VEmpty = do
  void $ removeChild parentNode ref  
  pure VEmpty

-- Make new element
go doc parentNode VEmpty (VNode typ attrs children _) = do
  node@(Just newNode)  <- fmap toNode <$> createElement doc (Just typ)
  newChildren <- forM children $ \childNode ->
    go doc newNode VEmpty childNode
  void $ appendChild parentNode node
  pure $ VNode typ attrs newChildren node

-- Remove an element
go _ parentNode (VNode _ _ _ ref) VEmpty = 
  VEmpty <$ removeChild parentNode ref

-- Replace an Element with a text node
go doc parentNode (VNode _ _ _ ref) (VText str _) = do
  newTextNode <- fmap toNode <$> createTextNode doc str
  void $ replaceChild parentNode newTextNode ref
  pure $ VText str newTextNode

-- Replace a text node with an Element
go doc parentNode (VText _ ref) (VNode typ attrs children _) = do
  node@(Just newNode) <- fmap toNode <$> createElement doc (Just typ)
  newChildren <- forM children $ \childNode ->
    go doc newNode VEmpty childNode
  void $ replaceChild parentNode node ref
  pure $ VNode typ attrs newChildren node

-- Replace a text node with a text node
go _ _ (VText currentStr currRef) (VText newStr _) = do
  when (currentStr /= newStr) $ do
    F.forM_ currRef $ \ref -> do
      let txt = castToText ref
      oldLength <- getLength txt
      replaceData txt 0 oldLength newStr
  pure $ VText newStr currRef

-- Diff two nodes together
go doc parent (VNode typA attrsA childrenA (Just ref)) (VNode typB attrsB childrenB _) = do
  case typA == typB of
    False -> do      
      node@(Just newNode) <- fmap toNode <$> createElement doc (Just typB)
      newChildren <- forM childrenB $ \childNode ->
        go doc newNode VEmpty childNode
      void $ replaceChild parent (Just ref) node
      pure $ VNode typB attrsB newChildren node
    True ->
      VNode typB <$> diffAttrs doc ref attrsA attrsB
                 <*> diffChildren doc ref childrenA childrenB
                 <*> pure (Just ref)
diffAttrs
  :: Show action
  => Document
  -> Node
  -> [Attribute action]
  -> [Attribute action]
  -> IO [Attribute action]
diffAttrs = undefined -- doc ref attrAs attrBs = pure []
  -- let as = S.fromList [ (k,v,o) | KV o k v <- attrAs ]
  --     bs = S.fromList [ (k,v,o) | KV o k v <- attrBs ]
  --     addAttrs = as `S.difference` bs
  --     remAttrs = bs `S.difference` as
  -- forM_ addAttrs $ \(k,v,o) -> do
  --   setAttribute ref 
  -- forM_ remAttrs $ \(k,v,o) -> do
  -- pure attrBs

diffChildren
  :: Show action
  => Document
  -> Node
  -> [VTree action]
  -> [VTree action]
  -> IO [VTree action]
diffChildren _ _ [] [] = pure []
diffChildren doc parent [] (b:bs) = 
  (:) <$> go doc parent VEmpty b
      <*> diffChildren doc parent [] bs
diffChildren doc parent (a:as) [] = 
  (:) <$> go doc parent a VEmpty
      <*> diffChildren doc parent as [] 
diffChildren doc parent (a:as) (b:bs) = 
  (:) <$> go doc parent a b
      <*> diffChildren doc parent as bs
