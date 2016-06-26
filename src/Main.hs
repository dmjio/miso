{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}

module Main where

import           Control.Monad
import qualified Data.Foldable           as F
import qualified Data.Text               as T
import           GHCJS.DOM
import           GHCJS.DOM.CharacterData
import           GHCJS.DOM.Document
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types         hiding (Event)

data Attribute action 
  = Event (String -> IO ())
  | KV Bool T.Text T.Text

instance Show action => Show (Attribute action)
  where show (Event _) = "<action>"

type DOMNode = Maybe Node

data VTree action =
    VNode String [ Attribute action ] [ VTree action ] DOMNode
  | VText String DOMNode
  | VEmpty

instance Show action => Show (VTree action) where
  show VEmpty = "<empty>"
  show (VNode typ _ children _) =
    "<" ++ typ ++ ">" ++ concatMap show children ++ "</" ++ typ ++ ">"
  show (VText val _ ) = val

mkNode name as cs = VNode name as cs Nothing

text_ = flip VText Nothing
div_  = mkNode "div"
btn_ = mkNode "button"

click_ :: Attribute action 
click_ = Event $ \_ -> putStrLn "hi"

temp = div_ [ ] [
    btn_ [ click_ ] [ text_ "hey" ]
  ]

main :: IO ()
main = do
  putStrLn "hi"
  print =<< do
    initTree (VNode "div" [] [ VNode "p" [] [ VText "heh2" Nothing ] Nothing ] Nothing)
      :: IO (VTree ())
  
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
diffAttrs doc ref attrAs attrBs = pure []
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

