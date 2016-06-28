{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Bool
import qualified Data.Foldable                 as F
import           Data.IORef
import           Data.List                     (find)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import           FRP.Elerea.Simple             (externalMulti, transfer, start)
import           GHCJS.DOM
import           GHCJS.DOM.CharacterData
import           GHCJS.DOM.Document            hiding (drop)
import           GHCJS.DOM.Element             (setAttribute)
import           GHCJS.DOM.Event               (getType, Event, getTarget)
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types               hiding (Event)
import           JavaScript.Web.AnimationFrame
import           Types
-- import Signal

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

type Key = Maybe Int

data VTree action =
    VNode String [ Attribute action ] [ VTree action ] Key DOMNode
  | VText String DOMNode
  | VEmpty

getChildDOMNodes :: VTree action -> [Node]
getChildDOMNodes (VNode _ _ children _ _) = [ node | VNode _ _ _ _ (Just node) <- children ]
getChildDOMNodes _ = []

getDOMNode :: VTree action -> Maybe Node
getDOMNode (VNode _ _ _ _ ref) = ref
getDOMNode _ = Nothing

instance Show action => Show (VTree action) where
  show VEmpty = "<empty>"
  show (VNode typ _ children _ _) =
    "<" ++ typ ++ ">" ++ concatMap show children ++ "</" ++ typ ++ ">"
  show (VText val _ ) = val

mkNode :: String -> [Attribute action] -> [VTree action] -> VTree action
mkNode name as cs = VNode name as cs Nothing Nothing

text_ :: String -> VTree action
text_ = flip VText Nothing

div_ :: [Attribute action] -> [VTree action] -> VTree action
div_  = mkNode "div"

btn_ :: [Attribute action] -> [VTree action] -> VTree action
btn_ = mkNode "button"

click_ :: IO () -> Attribute action 
click_ = Event "click" 

delegate :: IORef (VTree a) -> Events -> IO ()
delegate ref events = do
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
        eventType <- getType e
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
delegateEvent (VNode _ _ children _ _) eventName = findEvent children 
    where
      findEvent _ [] = pure ()
      findEvent childNodes [y] = 
       forM_ (findNode childNodes y) $ \(VNode _ attrs _ _ _) ->
         forM_ (getAction attrs) $ \action -> action

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
delegateEvent _ _ = const $ pure ()

defaultEvents :: [String] 
defaultEvents = [
    "blur", "change", "click", "dblclick",
    "focus", "focusin", "focusout", "input", "keydown",
    "keypress", "keyup", "mousedown", "mouseup",
    "mousemove", "mouseover", "select", "submit"
    ]

initTree :: Show action => VTree action -> IO (VTree action)
initTree initial = do
  Just document <- currentDocument
  Just body <- getBody document
  vdom <- diff VEmpty initial
  case vdom of
    VText _ ref -> void $ appendChild body ref
    VNode _ _ _ _ ref -> void $ appendChild body ref
    VEmpty -> pure ()
  pure vdom

diff :: Show action => VTree action -> VTree action -> IO (VTree action)
diff currentTree newTree = do
  Just document <- currentDocument
  Just body <- fmap toNode <$> getBody document
  go document body currentTree newTree

go :: Show action => Document -> Node -> VTree action -> VTree action -> IO (VTree action)
go _ _ VEmpty VEmpty = pure VEmpty

-- Ensure correct initialization (always true if internal)
go _ _ (VNode _ _ _ _ Nothing) _ = Prelude.error "VNode not initialized"
go _ _ (VText _ Nothing) _ = Prelude.error "VText not initialized"

-- Make a new text node
go doc parentNode VEmpty (VText str _) = do
  newTextNode <- createTextNode doc str
  void $ appendChild parentNode newTextNode
  pure $ VText str (toNode <$> newTextNode)

-- Remove a text node
go _ parentNode (VText _ ref) VEmpty = do
  void $ removeChild parentNode ref  
  pure VEmpty

-- Make a new element
go doc parentNode VEmpty (VNode typ attrs children key _) = do
  node@(Just newNode)  <- fmap toNode <$> createElement doc (Just typ)
  newChildren <- forM children $ \childNode ->
    go doc newNode VEmpty childNode
  void $ appendChild parentNode node
  pure $ VNode typ attrs newChildren key node

-- Remove an element
go _ parentNode (VNode _ _ _ _ ref) VEmpty = 
  VEmpty <$ removeChild parentNode ref

-- Replace an element with a text node
go doc parentNode (VNode _ _ _ _ ref) (VText str _) = do
  newTextNode <- fmap toNode <$> createTextNode doc str
  void $ replaceChild parentNode newTextNode ref
  pure $ VText str newTextNode

-- Replace a text node with an Element
go doc parentNode (VText _ ref) (VNode typ attrs children key _) = do
  node@(Just newNode) <- fmap toNode <$> createElement doc (Just typ)
  newChildren <- forM children $ \childNode ->
    go doc newNode VEmpty childNode
  void $ replaceChild parentNode node ref
  pure $ VNode typ attrs newChildren key node 

-- Replace a text node with a text node
go _ _ (VText currentStr currRef) (VText newStr _) = do
  when (currentStr /= newStr) $ do
    F.forM_ currRef $ \ref -> do
      let txt = castToText ref
      oldLength <- getLength txt
      replaceData txt 0 oldLength newStr
  pure $ VText newStr currRef

-- Diff two nodes together
go doc parent (VNode typA attrsA childrenA _ (Just ref)) (VNode typB attrsB childrenB keyB _) = do
  case typA == typB of
    False -> do      
      node@(Just newNode) <- fmap toNode <$> createElement doc (Just typB)
      newChildren <- forM childrenB $ \childNode ->
        go doc newNode VEmpty childNode
      void $ replaceChild parent (Just ref) node
      pure $ VNode typB attrsB newChildren keyB node
    True ->
      VNode typB <$> diffAttrs ref attrsA attrsB
                 <*> diffChildren doc ref childrenA childrenB
                 <*> pure keyB
                 <*> pure (Just ref)
diffAttrs
  :: Show action
  => Node
  -> [Attribute action]
  -> [Attribute action]
  -> IO [Attribute action]
diffAttrs node as bs = do
   let attrsA = [(k,v) | KV _ k v <- as]
       attrsB = [(k,v) | KV _ k v <- bs]
   when (attrsA /= attrsB) $ setAttrs node bs
   pure bs

setAttrs :: Show action => Node -> [Attribute action] -> IO ()
setAttrs node xs =
  forM_ [(k,v) | KV _ k v <- xs] $ \(k,v) ->
    setAttribute (castToElement node) k v

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
diffChildren doc parent (a:as) (b:bs) = do
  (:) <$> go doc parent a b
      <*> diffChildren doc parent as bs

type Events = [ String ]

runSignal :: Show a => Events -> Signal (VTree a) -> IO ()
runSignal events (Signal s) = do
  ref <- newIORef =<< do initTree $ mkNode "div" [] []
  void . forkIO $ delegate ref events
  emitter <- start s
  forever $ do
    _ <- waitForAnimationFrame
    x <- emitter
    case x of 
        Changed [ newTree ] -> 
          writeIORef ref =<< (`diff` newTree) =<< readIORef ref
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

data Action = AddOne | SubOne
  deriving (Show)

main :: IO ()
main = do
  putStrLn "hi"
  (sig, send) <- signal AddOne
  send AddOne
  let s :: Signal (VTree ()) = view send <$> foldp update (0 :: Int) sig
  runSignal [ "click" :: String ] s 
    where
      update :: Action -> Int -> Int
      update AddOne x = x + 1
      update SubOne x = x - 1

      view send model = div_ [ KV True "style" "background:red;"] [
            btn_ [ click_ $ send AddOne ] [ text_ "+" ]
          , text_ (show model)
          , btn_ [ click_ $ send SubOne ] [ text_ "-" ]
          ]
