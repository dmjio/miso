{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

-- import           Debug.Trace (traceShow)
import qualified Data.Map                      as M
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
import           GHCJS.DOM.Event               (Event, getTarget)
import qualified GHCJS.DOM.Event               as E
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures
import           GHCJS.DOM.HTMLInputElement
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types               hiding (Event)
import           GHCJS.DOM.UIEvent             (getKeyCode)

import           JavaScript.Web.AnimationFrame
import           Types

-- import Signal

data Options = Options {
       stopPropogation :: Bool
     , preventDefault :: Bool
     } deriving Show

defaultOptions :: Options
defaultOptions = Options False False

data Attribute 
  = Event Options String (E.Event -> IO ())
  | KV T.Text T.Text

onWithOptions :: Options -> String -> (E.Event -> IO ()) -> Attribute 
onWithOptions = Event

on :: String -> (E.Event -> IO ()) -> Attribute 
on = onWithOptions defaultOptions

instance Show Attribute where
  show (Event _ name _) = "<event=" ++ name ++ ">"
  show (KV k v) = T.unpack $ k <> "=" <> v

type Key = Maybe Int

type VTree = VTreeBase (Maybe Node)

data VTreeBase a = 
    VNode String [ Attribute ] [ VTree ] Key a
  | VText String a
  | VEmpty

getChildDOMNodes :: VTree -> [Node]
getChildDOMNodes (VNode _ _ children _ _) =
  [ node
  | VNode _ _ _ _ (Just node) <- children
  ]
getChildDOMNodes _ = []

getDOMNode :: VTree -> Maybe Node
getDOMNode (VNode _ _ _ _ ref) = ref
getDOMNode _ = Nothing

instance Show VTree where
  show VEmpty = "<empty>"
  show (VNode typ evts children _ _) =
    "<" ++ typ ++ ">" ++ show evts ++ concatMap show children ++ "</" ++ typ ++ ">"
  show (VText val _ ) = val

mkNode :: String -> [Attribute] -> [VTree] -> VTree
mkNode name as cs = VNode name as cs Nothing Nothing

text_ :: String -> VTree
text_ = flip VText Nothing

div_ :: [Attribute] -> [VTree] -> VTree
div_  = mkNode "div"

btn_ :: [Attribute] -> [VTree] -> VTree
btn_ = mkNode "button"

delegate :: IORef VTree -> Events -> IO ()
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
        eventType <- E.getType e
        stack <- buildTargetToBody body (castToNode target)
        print ("got event ->" :: String, eventType)
        delegateEvent e vtree eventType stack

buildTargetToBody :: Node -> Node -> IO [Node]
buildTargetToBody body target = f target [target]
    where
      f currentNode nodes
        | body == currentNode = pure (drop 2 nodes)
        | otherwise = do
            Just parent <- getParentNode currentNode
            f parent (parent:nodes)

delegateEvent :: E.Event -> VTree -> String -> [Node] -> IO ()
delegateEvent e (VNode _ _ children _ _) eventName = findEvent children 
    where
      findEvent _ [] = pure ()
      findEvent childNodes [y] = 
       forM_ (findNode childNodes y) $ \(VNode _ attrs _ _ _) ->
         forM_ (getAction attrs) $
           \(Options{..}, action) -> do
              putStrLn "made it here..."
              when stopPropogation $ E.stopPropagation e
              when preventDefault $ E.preventDefault e
              action e

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
          Event opts evtName action <- attrs
          guard (evtName == eventName)
          pure (opts, action)
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
  vdom <- diff VEmpty initial
  case vdom of
    VText _ ref -> void $ appendChild body ref
    VNode _ _ _ _ ref -> void $ appendChild body ref
    VEmpty -> pure ()
  pure vdom

diff :: VTree -> VTree -> IO VTree
diff currentTree newTree = do
  Just document <- currentDocument
  Just body <- fmap toNode <$> getBody document
  go document body currentTree newTree

go :: Document -> Node -> VTree -> VTree -> IO VTree
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
go _ parentNode (VText _ node) VEmpty = do
  void $ removeChild parentNode node
  pure VEmpty

-- Make a new element
go doc parentNode VEmpty (VNode typ attrs children key _) = do
  node@(Just newNode)  <- fmap toNode <$> createElement doc (Just typ)
  setAttrs newNode attrs
  newChildren <- forM children $ \childNode ->
    go doc newNode VEmpty childNode
  void $ appendChild parentNode node
  pure $ VNode typ attrs newChildren key node

-- Remove an element
go _ parentNode (VNode _ _ _ _ node) VEmpty = 
  VEmpty <$ removeChild parentNode node

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
      void $ replaceChild parent node (Just ref)
      pure $ VNode typB attrsB newChildren keyB node
    True ->
      VNode typB <$> diffAttrs ref attrsA attrsB
                 <*> diffChildren doc ref childrenA childrenB
                 <*> pure keyB
                 <*> pure (Just ref)
diffAttrs
  :: Node
  -> [Attribute]
  -> [Attribute]
  -> IO [Attribute]
diffAttrs node as bs = do
   let attrsA = [(k,v) | KV k v <- as]
       attrsB = [(k,v) | KV k v <- bs]
   when (attrsA /= attrsB) $ setAttrs node bs
   pure bs

setAttrs :: Node -> [Attribute] -> IO ()
setAttrs node xs =
  forM_ [(k,v) | KV k v <- xs] $ \(k,v) ->
    setAttribute (castToElement node) k v

diffChildren
  :: Document
  -> Node
  -> [VTree]
  -> [VTree]
  -> IO [VTree]
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

runSignal :: Events -> Signal VTree -> IO ()
runSignal events (Signal s) = do
  ref <- newIORef =<< do initTree $ mkNode "div" [] []
  void . forkIO $ delegate ref events
  emitter <- start s
  forever $ 
    waitForAnimationFrame >>
      emitter >>= \case
        Changed [ newTree ] -> do
          result <- (`diff` newTree) =<< readIORef ref
          print result
          writeIORef ref result
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

data Model = Model {
    nextTaskNum :: Int
  , tasks       :: M.Map Int Task 
  , turnOn      :: Bool
  } deriving (Show, Eq)

data Task = Task {
        taskCompleted :: Bool
      , taskContent :: String
      } deriving (Show, Eq)

data Action =
    AddTask String
  | RemoveTask Int
  | ToggleCompleted Int Bool
  | ToggleAllCompleted
  | Switch
  deriving (Show)

main :: IO ()
main = do
  (sig, send) <- signal Switch
  runSignal ["keypress", "click"] $
    view send <$> foldp update (Model 0 mempty False) sig
    where
      update :: Action -> Model -> Model
      update Switch model = model { turnOn = True }
      update (ToggleCompleted taskId isCompleted) model =
        model { tasks = M.adjust modifyTask taskId (tasks model) }
          where
            modifyTask task = task { taskCompleted = isCompleted }
      update (RemoveTask n) model@Model{..} = model { tasks = M.delete n tasks }
      update ToggleAllCompleted model =
        case F.length (tasks model) == (F.length $ M.filter (\t -> taskCompleted t == False ) (tasks model)) of
          True ->
             model {
                tasks = M.map (\t -> t { taskCompleted = True }) (tasks model)
              }
          False ->
             model {
                tasks = M.map (\t -> t { taskCompleted = False }) (tasks model)
              }

      update (AddTask str) model@Model{..} =
        model { nextTaskNum = nextTaskNum + 1
              , tasks = M.insert nextTaskNum (Task False str) tasks
              }
      view send Model { .. } = div_ [ ] [
            h1_ [] [ text_ "todos"]
          , input_ [ type_ "text"
                   , placeholder "What needs to be done?"
                   , autofocus
                   , on "keypress" $ \e -> do
                       Just target <- getTarget e
                       let ele = castToHTMLInputElement target
                       Just value <- getValue ele
                       key <- getKeyCode (castToUIEvent e :: UIEvent)
                       when (key == 13) $ do
                         setValue ele (Just ("" :: T.Text))
                         send $ AddTask value
                   ] [ ]
          , ul_ [] $ flip map (M.toList tasks) $ \(taskId, Task {..}) ->
              li_ [] [
               div_ [] [
                  if taskCompleted then
                    s_ [] [ text_ taskContent ]
                  else
                    text_ taskContent
                  , btn_ [ on "click" $ const $ send (RemoveTask taskId) ]
                         [ text_ "x" ]
                ]
               ]
          ]

p_ :: [Attribute] -> [VTree] -> VTree
p_ = mkNode "p" 

s_ :: [Attribute] -> [VTree] -> VTree
s_ = mkNode "s" 

ul_ :: [Attribute] -> [VTree] -> VTree
ul_ = mkNode "ul" 

li_ :: [Attribute] -> [VTree] -> VTree
li_ = mkNode "li" 

h1_ :: [Attribute] -> [VTree] -> VTree
h1_ = mkNode "h1" 

section_ :: [Attribute] -> [VTree] -> VTree
section_ = mkNode "section" 

input_ :: [Attribute] -> [VTree] -> VTree
input_ = mkNode "input" 

type_ :: T.Text -> Attribute 
type_ = KV "type"

placeholder :: T.Text -> Attribute 
placeholder = KV "placeholder"

autofocus :: Attribute 
autofocus = KV "autofocus" mempty
